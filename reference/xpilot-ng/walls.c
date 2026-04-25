/*
 * XPilot NG, a multiplayer space war game.
 *
 * Copyright (C) 2000-2004 Uoti Urpala <uau@users.sourceforge.net>
 *
 * Copyright (C) 1991-2001 by
 *
 *      Bj�rn Stabell        <bjoern@xpilot.org>
 *      Ken Ronny Schouten   <ken@xpilot.org>
 *      Bert Gijsbers        <bert@xpilot.org>
 *      Dick Balaska         <dick@xpilot.org>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "xpserver.h"

struct move_parameters mp;
static char msg[MSG_LEN];

/* polygon map related stuff */

/* start */

/* Maximum line length 32767-B_CLICKS, 30000 used in checks
 * There's a minimum map size to avoid "too much wrapping". A bit smaller
 * than that would cause rare errors for fast-moving things. I haven't
 * bothered to figure out what the limit is. 80k x 80k clicks should
 * be more than enough (probably...). */
#define B_SHIFT 11
#define B_CLICKS (1 << B_SHIFT)
#define B_MASK (B_CLICKS - 1)
#define CUTOFF (2 * BLOCK_CLICKS) /* Not sure about the optimum value */
#define MAX_MOVE 32000
#define SEPARATION_DIST 64
/* This must be increased if the ship corners are allowed to go farther
 * when turning! */
/* this is big enough for asteroids of size 4 */
#define MAX_SHAPE_OFFSET (52 * CLICK)

#if ((-3) / 2 != -1) || ((-3) % 2 != -1)
#error "This code assumes that negative numbers round upwards."
#endif

struct tl2
{
	int base;
	int x;
	int y;
};

struct bline
{
	clvec_t start;
	clvec_t delta;
	double c;
	double s;
	short group;
};

struct blockinfo
{
	unsigned short distance;
	unsigned short *lines;
	unsigned short *points;
};

struct inside_block
{
	short *y;
	short *lines;
	struct inside_block *next;
	short group;
	char base_value;
};

struct inside_block *inside_table;

struct test
{
	double distance;
	int inside;
	struct tempy *y;
	struct templine *lines;
};

struct test *temparray;

shape_t ball_wire;

#define LINEY(X, Y, BASE, ARG) (((Y) * (ARG) + (BASE)) / (X))
#define SIDE(X, Y, LINE) (linet[(LINE)].delta.cy * (X) - linet[(LINE)].delta.cx * (Y))
#define SIGN(X) ((X) >= 0 ? 1 : -1)

struct bline *linet;
#define S_LINES 100 /* stupid hack */

struct group *groups = NULL;
int num_groups = 0, max_groups = 0;

struct blockinfo *blockline;
unsigned short *llist;
unsigned short *plist;
int num_lines = 0;
int num_polys = 0;
int mapx, mapy;

static inline bool can_hit(group_t *gp, const move_t *move)
{
	if (gp->hitmask & move->hitmask)
		return false;
	if (gp->hitfunc == NULL)
		return true;
	return gp->hitfunc(gp, move);
}

void Move_init(void)
{
	LIMIT(options.maxObjectWallBounceSpeed, 0, world->hypotenuse);
	LIMIT(options.maxShieldedWallBounceSpeed, 0, world->hypotenuse);
	LIMIT(options.maxUnshieldedWallBounceSpeed, 0, world->hypotenuse);

	LIMIT(options.playerWallBounceBrakeFactor, 0, 1);
	LIMIT(options.playerWallFriction, 0, FLT_MAX);
	LIMIT(options.objectWallBounceBrakeFactor, 0, 1);
	LIMIT(options.objectWallBounceLifeFactor, 0, 1);

	mp.obj_bounce_mask = 0;
	if (options.sparksWallBounce)
		SET_BIT(mp.obj_bounce_mask, OBJ_SPARK_BIT);
	if (options.debrisWallBounce)
		SET_BIT(mp.obj_bounce_mask, OBJ_DEBRIS_BIT);
	if (options.shotsWallBounce)
		SET_BIT(mp.obj_bounce_mask, OBJ_SHOT_BIT | OBJ_CANNON_SHOT_BIT);
	if (options.itemsWallBounce)
		SET_BIT(mp.obj_bounce_mask, OBJ_ITEM_BIT);
	if (options.missilesWallBounce)
		SET_BIT(mp.obj_bounce_mask,
				OBJ_SMART_SHOT_BIT | OBJ_TORPEDO_BIT | OBJ_HEAT_SHOT_BIT);
	if (options.minesWallBounce)
		SET_BIT(mp.obj_bounce_mask, OBJ_MINE_BIT);
	if (options.ballsWallBounce)
		SET_BIT(mp.obj_bounce_mask, OBJ_BALL_BIT);
	if (options.asteroidsWallBounce)
		SET_BIT(mp.obj_bounce_mask, OBJ_ASTEROID_BIT);
	if (options.pulsesWallBounce)
		SET_BIT(mp.obj_bounce_mask, OBJ_PULSE_BIT);

	mp.obj_cannon_mask = (KILLING_SHOTS) | OBJ_MINE_BIT | OBJ_SHOT_BIT | OBJ_PULSE_BIT | OBJ_SMART_SHOT_BIT | OBJ_TORPEDO_BIT | OBJ_HEAT_SHOT_BIT | OBJ_ASTEROID_BIT;
	if (options.cannonsPickupItems)
		mp.obj_cannon_mask |= OBJ_ITEM_BIT;
	mp.obj_target_mask = mp.obj_cannon_mask | OBJ_BALL_BIT | OBJ_SPARK_BIT;
	mp.obj_treasure_mask = mp.obj_bounce_mask | OBJ_BALL_BIT | OBJ_PULSE_BIT;
}

void Object_crash(object_t *obj, int crashtype, int mapobj_ind)
{
	switch (crashtype)
	{

	default:
		break;

	case CrashWormHole:
		Object_hits_wormhole(obj, mapobj_ind);
		break;

	case CrashTreasure:
		/*
		 * Ball type has already been handled.
		 */
		if (obj->type == OBJ_BALL)
			break;
		obj->life = 0;
		break;

	case CrashTarget:
		obj->life = 0;
		Object_hits_target(obj, Target_by_index(mapobj_ind), -1.0);
		break;

	case CrashWall:
		obj->life = 0;
		/* add sparks ??? */
		break;

	case CrashUniverse:
		obj->life = 0;
		break;

	case CrashCannon:
		obj->life = 0;
		Object_hits_cannon(obj, Cannon_by_index(mapobj_ind));
		break;

	case CrashUnknown:
		obj->life = 0;
		break;
	}
}

void Player_crash(player_t *pl, int crashtype, int mapobj_ind, int pt)
{
	const char *howfmt = NULL;
	const char *hudmsg = NULL;

	msg[0] = '\0';

	switch (crashtype)
	{

	default:
	case NotACrash:
		warn("Unrecognized crash %d", crashtype);
		break;

	case CrashWormHole:
		Object_hits_wormhole(OBJ_PTR(pl), mapobj_ind);
		break;

	case CrashWall:
		howfmt = "%s crashed%s against a wall";
		hudmsg = "[Wall]";
		sound_play_sensors(pl->pos, PLAYER_HIT_WALL_SOUND);
		break;

	case CrashWallSpeed:
		howfmt = "%s smashed%s against a wall";
		hudmsg = "[Wall]";
		sound_play_sensors(pl->pos, PLAYER_HIT_WALL_SOUND);
		break;

	case CrashWallNoFuel:
		howfmt = "%s smacked%s against a wall";
		hudmsg = "[Wall]";
		sound_play_sensors(pl->pos, PLAYER_HIT_WALL_SOUND);
		break;

	case CrashWallAngle:
		howfmt = "%s was trashed%s against a wall";
		hudmsg = "[Wall]";
		sound_play_sensors(pl->pos, PLAYER_HIT_WALL_SOUND);
		break;

	case CrashTarget:
		howfmt = "%s smashed%s against a target";
		hudmsg = "[Target]";
		sound_play_sensors(pl->pos, PLAYER_HIT_WALL_SOUND);
		Object_hits_target(OBJ_PTR(pl), Target_by_index(mapobj_ind), -1.0);
		break;

	case CrashTreasure:
		howfmt = "%s smashed%s against a treasure";
		hudmsg = "[Treasure]";
		sound_play_sensors(pl->pos, PLAYER_HIT_WALL_SOUND);
		break;

	case CrashCannon:
	{
		cannon_t *cannon = Cannon_by_index(mapobj_ind);

		if (!Player_uses_emergency_shield(pl))
		{
			howfmt = "%s smashed%s against a cannon";
			hudmsg = "[Cannon]";
			sound_play_sensors(pl->pos, PLAYER_HIT_CANNON_SOUND);
		}
		if (!BIT(cannon->used, HAS_EMERGENCY_SHIELD))
		{
			/* pl gets points if the cannon is rammed with shields up */
			if (Player_uses_emergency_shield(pl))
				Cannon_dies(cannon, pl);
			else
				Cannon_dies(cannon, NULL);
		}
	}
	break;

	case CrashUniverse:
		howfmt = "%s left the known universe%s";
		hudmsg = "[Universe]";
		sound_play_sensors(pl->pos, PLAYER_HIT_WALL_SOUND);
		break;

	case CrashUnknown:
		howfmt = "%s slammed%s into a programming error";
		hudmsg = "[Bug]";
		sound_play_sensors(pl->pos, PLAYER_HIT_WALL_SOUND);
		break;
	}

	if (howfmt && hudmsg)
	{
		player_t *pushers[MAX_RECORDED_SHOVES];
		int cnt[MAX_RECORDED_SHOVES];
		int num_pushers = 0;
		int total_pusher_count = 0;
		double total_pusher_score = 0;
		int i, j;

		Player_set_state(pl, PL_STATE_KILLED);
		sprintf(msg, howfmt, pl->name, (!pt) ? " head first" : "");

		/* get a list of who pushed me */
		for (i = 0; i < MAX_RECORDED_SHOVES; i++)
		{
			shove_t *shove = &pl->shove_record[i];

			if (shove->pusher_id == NO_ID)
				continue;

			if (shove->time < frame_loops - (int)((20 / timeStep) + 0.5))
				continue;

			for (j = 0; j < num_pushers; j++)
			{
				if (shove->pusher_id == pushers[j]->id)
				{
					cnt[j]++;
					break;
				}
			}
			if (j == num_pushers)
			{
				pushers[num_pushers++] = Player_by_id(shove->pusher_id);
				cnt[j] = 1;
			}
			total_pusher_count++;
			total_pusher_score += Get_Score(pushers[j]);
		}
		if (num_pushers == 0)
		{
			Handle_Scoring(SCORE_WALL_DEATH, NULL, pl, NULL, hudmsg);
			strcat(msg, ".");
			Set_message(msg);
		}
		else
		{
			int msg_len = strlen(msg);
			char *msg_ptr = &msg[msg_len];
			double average_pusher_score = total_pusher_score / total_pusher_count;
			bool was_tagged = (tagItPlayerId == pl->id),
				 pusher_is_tagged = false;
			double mult;
			player_t dummy;

			for (i = 0; i < num_pushers; i++)
			{
				player_t *pusher = pushers[i];
				const char *sep = (!i)					  ? " with help from "
								  : (i < num_pushers - 1) ? ", "
														  : " and ";
				size_t sep_len = strlen(sep);
				size_t name_len = strlen(pusher->name);

				if (msg_len + sep_len + name_len + 2 < sizeof msg)
				{
					strcpy(msg_ptr, sep);
					msg_len += sep_len;
					msg_ptr += sep_len;
					strcpy(msg_ptr, pusher->name);
					msg_len += name_len;
					msg_ptr += name_len;
				}
				mult = cnt[i] / total_pusher_count;
				if (options.tagGame)
				{
					if (tagItPlayerId == pusher->id)
					{
						mult *= options.tagItKillScoreMult;
						pusher_is_tagged = true;
					}
					else if (was_tagged && num_pushers == 1)
					{
						mult *= options.tagKillItScoreMult;
						Transfer_tag(pl, pusher);
					}
				}

				Handle_Scoring(SCORE_SHOVE_KILL, pusher, pl, &mult, NULL);
				if (i >= num_pushers - 1)
					Rank_add_shove_kill(pusher);
			}

			if (options.tagGame && num_pushers == 1)
			{
				if (was_tagged)
					mult = options.tagKillItScoreMult;
				else if (pusher_is_tagged)
					mult = options.tagItKillScoreMult;
			}
			dummy.score = average_pusher_score;
			Handle_Scoring(SCORE_SHOVE_DEATH, &dummy, pl, &mult, NULL);

			strcpy(msg_ptr, ".");
			Set_message(msg);

			/* Robots will declare war on anyone who shoves them. */
			i = (int)(rfrac() * num_pushers);
			Robot_war(pl, pushers[i]);
		}
	}

	if (Player_is_killed(pl) && Get_Score(pl) < 0 && Player_is_robot(pl))
	{
		pl->home_base = Base_by_index(0);
		Pick_startpos(pl);
	}
}

static void *ralloc(void *ptr, size_t size)
{
	if (!(ptr = realloc(ptr, size)))
	{
		warn("Realloc failed.");
		exit(1);
	}
	return ptr;
}

static unsigned short *Shape_lines(shape_t *s, int dir)
{
	int i;
	static unsigned short foo[100];
	static shape_t *lastshape;
	static int lastdir;
	const int os = num_lines;
	clpos_t *pts;

	/* linet[i].group MUST BE INITIALIZED TO 0 */

	if (s == lastshape && dir == lastdir)
		return foo;

	lastshape = s;
	lastdir = dir;

	pts = Shape_get_points((shape_t *)s, dir);
	for (i = 0; i < s->num_points; i++)
	{
		clpos_t pt = pts[i];

		linet[i + os].start.cx = -pt.cx;
		linet[i + os].start.cy = -pt.cy;
	}
	for (i = 0; i < s->num_points - 1; i++)
	{
		linet[i + os].delta.cx = linet[i + os + 1].start.cx - linet[i + os].start.cx;
		linet[i + os].delta.cy = linet[i + os + 1].start.cy - linet[i + os].start.cy;
	}
	linet[i + os].delta.cx = linet[os].start.cx - linet[i + os].start.cx;
	linet[i + os].delta.cy = linet[os].start.cy - linet[i + os].start.cy;
	for (i = 0; i < s->num_points; i++)
		foo[i] = i + os;
	foo[i] = 65535;
	return foo;
}

static int Bounce_object(object_t *obj, move_t *move, int line, int point)
{
	double fx, fy;
	double c, s, wall_brake_factor = options.objectWallBounceBrakeFactor;
	int group, type;
	int mapobj_ind;

	group = linet[line >= num_lines ? point : line].group;
	type = groups[group].type;
	mapobj_ind = groups[group].mapobj_ind;

	if (obj->collmode == 1)
	{
		fx = ABS(obj->extmove.cx) + ABS(obj->extmove.cy);
		/* If fx <= 64, there is practically no movement. Object
		   collision detection can ignore the bounce. */
		if (fx > 64)
		{
			obj->wall_time = 1 -
							 (ABS(move->delta.cx) + ABS(move->delta.cy)) / fx;
			obj->collmode = 2;
		}
	}

	if (type == TREASURE)
	{
		if (obj->type == OBJ_BALL)
			Ball_hits_goal(BALL_PTR(obj), groupptr_by_id(group));
		obj->life = 0;
		return 0;
	}

	if (type == TARGET)
	{
		obj->life = 0;
		Object_hits_target(obj, Target_by_index(mapobj_ind), -1.0);
		return 0;
	}

	if (type == CANNON)
	{
		Object_crash(obj, CrashCannon, mapobj_ind);
		return 0;
	}

	if (type == WORMHOLE)
	{
		Object_crash(obj, CrashWormHole, mapobj_ind);
		return 0;
	}

	if (!BIT(mp.obj_bounce_mask, OBJ_TYPEBIT(obj->type)))
	{
		obj->life = 0;
		return 0;
	}

	if (obj->type != OBJ_BALL && obj->type != OBJ_PULSE)
	{
		obj->life *= options.objectWallBounceLifeFactor;
		if (obj->life <= 0)
			return 0;
	}

	/*
	 * Any bouncing sparks are no longer owner immune to give
	 * "reactive" thrust.  This is exactly like ground effect
	 * in the real world.  Very useful for stopping against walls.
	 */
	if (obj->type != OBJ_PULSE &&
		obj->type != OBJ_SPARK &&
		sqr(obj->vel.x) + sqr(obj->vel.y) > sqr(options.maxObjectWallBounceSpeed))
	{
		obj->life = 0;
		return 0;
	}

	if (obj->type == OBJ_SPARK &&
		sqr(obj->vel.x) + sqr(obj->vel.y) > sqr(options.maxSparkWallBounceSpeed))
	{
		obj->life = 0;
		return 0;
	}

	if (obj->type == OBJ_SPARK)
		CLR_BIT(obj->obj_status, OWNERIMMUNE);

	if (line >= num_lines)
	{
		double x, y, l2;

		x = linet[line].delta.cx;
		y = linet[line].delta.cy;
		l2 = (x * x + y * y);
		c = (x * x - y * y) / l2;
		s = 2 * x * y / l2;
	}
	else
	{
		c = linet[line].c;
		s = linet[line].s;
	}

	if (obj->type == OBJ_PULSE)
		wall_brake_factor = 1.0;
	fx = move->delta.cx * c + move->delta.cy * s;
	fy = move->delta.cx * s - move->delta.cy * c;
	move->delta.cx = (click_t)(fx * wall_brake_factor);
	move->delta.cy = (click_t)(fy * wall_brake_factor);
	fx = obj->vel.x * c + obj->vel.y * s;
	fy = obj->vel.x * s - obj->vel.y * c;
	obj->vel.x = fx * wall_brake_factor;
	obj->vel.y = fy * wall_brake_factor;
	if (obj->collmode == 2)
		obj->collmode = 3;

	/* find direction of pulse after bounce */
	if (obj->type == OBJ_PULSE)
	{
		pulseobject_t *pulse = PULSE_PTR(obj);

		pulse->pulse_dir = (int)Wrap_findDir(pulse->vel.x, pulse->vel.y);
		pulse->pulse_len = 0;
		pulse->pulse_refl = true;
	}

	return 1;
}

static void Bounce_player(player_t *pl, move_t *move, int line, int point)
{
	double c, s;   /* cosine and sine of 2 times line angle */
	double cl, sl; /* cosine and sine of line angle */
	double x, y, l2, l;
	int group, type, mapobj_ind;
	bool constant_speed_subtracted = (pl->last_wall_touch == frame_loops ? true : false);

	x = linet[line].delta.cx;
	y = linet[line].delta.cy;
	l2 = (x * x + y * y);
	c = (x * x - y * y) / l2;
	s = 2 * x * y / l2;
	l = sqrt(l2);
	cl = x / l;
	sl = y / l;

	group = linet[line >= num_lines ? point : line].group;
	type = groups[group].type;
	mapobj_ind = groups[group].mapobj_ind;
	if (type == TREASURE && options.treasureCollisionKills)
	{
		Player_crash(pl, CrashTreasure, NO_IND, 1);
		return;
	}

	if (type == WORMHOLE)
	{
		Player_crash(pl, CrashWormHole, mapobj_ind, 1);
		return;
	}

	if (type == CANNON)
	{
		Player_crash(pl, CrashCannon, mapobj_ind, 1);
		if (Player_is_killed(pl))
			return;
		/* The player may bounce from the cannon if both have shields up. */
	}

	pl->last_wall_touch = frame_loops;
	{
		double speed = VECTOR_LENGTH(pl->vel);
		double v = speed * 0.25;
		double m = pl->mass - pl->emptymass * 0.75;
		double b = 1.0 - 0.5 * options.playerWallBounceBrakeFactor;
		double cost = b * m * v;
		double max_speed = BIT(pl->used, HAS_SHIELD)
							   ? options.maxShieldedWallBounceSpeed
							   : options.maxUnshieldedWallBounceSpeed;

		if (Player_uses_emergency_shield(pl) && max_speed < 100.0)
			max_speed = 100.0;

		/* only use armor if neccessary */
		if (speed > max_speed && max_speed < options.maxShieldedWallBounceSpeed && !BIT(pl->used, HAS_SHIELD) && Player_has_armor(pl))
		{
			max_speed = options.maxShieldedWallBounceSpeed;
			Player_hit_armor(pl);
		}

		if (speed > max_speed)
		{
			if (type == TARGET)
				Player_crash(pl, CrashTarget, mapobj_ind, 1);
			else
				Player_crash(pl, CrashWallSpeed, NO_IND, 1);
			return;
		}

		if (!Player_uses_emergency_shield(pl))
			Item_damage(pl, options.wallBounceDestroyItemProb);

		sound_play_sensors(pl->pos, PLAYER_BOUNCED_SOUND);

		if (cost && type == TARGET)
			Object_hits_target(OBJ_PTR(pl),
							   Target_by_index(mapobj_ind),
							   cost / 4.0);
	}

	/*
	 * Remove constantSpeed before bounce.
	 * Otherwise constantSpeed will accelerate your ship when you
	 * slide along a wall.
	 */
	if (options.constantSpeed && !constant_speed_subtracted)
	{
		pl->vel.x -= options.constantSpeed * pl->acc.x;
		pl->vel.y -= options.constantSpeed * pl->acc.y;
	}

	if (options.playerWallBounceType == 0)
	{
		double fx, fy;

		fx = move->delta.cx * c + move->delta.cy * s;
		fy = move->delta.cx * s - move->delta.cy * c;
		move->delta.cx = (click_t)(fx * options.playerWallBounceBrakeFactor);
		move->delta.cy = (click_t)(fy * options.playerWallBounceBrakeFactor);
		fx = pl->vel.x * c + pl->vel.y * s;
		fy = pl->vel.x * s - pl->vel.y * c;
		pl->vel.x = fx * options.playerWallBounceBrakeFactor;
		pl->vel.y = fy * options.playerWallBounceBrakeFactor;
	}
	else
	{
		/*
		 * Determine new velocity vector and move->delta after bounce.
		 * The vector move->delta is the remaining amount left to move
		 * in this frame.
		 */
		vector_t vel, vel1, mvd, mvd1;

		/*
		 * i. Rotate velocity and move->delta clockwise by line angle.
		 */
		vel.x = pl->vel.x * cl + pl->vel.y * sl;
		vel.y = pl->vel.x * (-sl) + pl->vel.y * cl;
		vel1 = vel;
		mvd.x = move->delta.cx * cl + move->delta.cy * sl;
		mvd.y = move->delta.cx * (-sl) + move->delta.cy * cl;
		mvd1 = mvd;

		/* ii. Reverse direction of perpendicular component. */
		vel.y = -vel.y;
		mvd.y = -mvd.y;

		/*
		 * iii. Determine how much perpendicular and parallel components
		 * change.
		 */
		vel.y *= options.playerWallBounceBrakeFactor;
		mvd.y *= options.playerWallBounceBrakeFactor;

		if (options.playerWallBounceType == 1)
		{
			double factor = 1.0 - options.playerWallFriction;
			/*
			 * kps: simple "separate multipliers" implementation
			 */
			if (factor < 0.0)
				factor = 0.0;
			vel.x *= factor;
			mvd.x *= factor;
		}
		else if (options.playerWallBounceType == 2)
		{
			double vtotal1 = VECTOR_LENGTH(vel1);
			double vnormal1 = ABS(vel1.y);
			double wallfriction = options.playerWallFriction;
			double factor;

			/* Avoid division by 0 */
			if (vtotal1 < 0.001)
				factor = 1.0 - wallfriction;
			else
				factor = 1.0 - vnormal1 / vtotal1 * wallfriction;
			/*
			 * mara:
			 * Vtangent2 = (1-Vnormal1/Vtotal1*wallfriction)*Vtangent1;
			 */
			if (factor < 0.0)
				factor = 0.0;
			vel.x *= factor;
			mvd.x *= factor;
		}
		else if (options.playerWallBounceType == 3)
		{
			double change;
			double C1 = options.playerWallFriction;
			double C2 = 1.0 - options.playerWallBounceBrakeFactor;
			double perpendicular_change, parallel_speed;
			/*
			 * uau:
			 * change the parallel one by
			 * MIN(C1*perpendicular_change, C2*parallel_speed)
			 * if you assume the wall has a coefficient of friction C1
			 */
			perpendicular_change = ABS(vel1.y - vel.y);
			parallel_speed = ABS(vel.x);
			change = MIN(C1 * perpendicular_change, C2 * parallel_speed);
			if (vel.x > 0)
				vel.x -= change;
			else
				vel.x += change;

			perpendicular_change = ABS(mvd1.y - mvd.y);
			parallel_speed = ABS(mvd.x);
			change = MIN(C1 * perpendicular_change, C2 * parallel_speed);
			if (mvd.x > 0)
				mvd.x -= change;
			else
				mvd.x += change;
		}
		/* iv. Rotate the whole thing anti-clockwise. */
		pl->vel.x = vel.x * cl + vel.y * (-sl);
		pl->vel.y = vel.x * sl + vel.y * cl;
		move->delta.cx = (click_t)(mvd.x * cl + mvd.y * (-sl));
		move->delta.cy = (click_t)(mvd.x * sl + mvd.y * cl);
	}
}

/* Used internally by the movement routines to find the first line
 * (in the list given by *lines) that the given trajectory hits. */
static int Lines_check(int msx, int msy, int mdx, int mdy, int *mindone,
					   const unsigned short *lines, int chx, int chy,
					   int chxy, const move_t *move, int *minline,
					   int *height)
{
	int lsx, lsy, ldx, ldy, temp, mirror, start, end, i, x, sy, ey, prod;
	int mbase = mdy >> 1, hit = 0;

	while ((i = *lines++) != 65535)
	{
		if (linet[i].group && (!can_hit(&groups[linet[i].group], move)))
			continue;
		lsx = linet[i].start.cx;
		lsy = linet[i].start.cy;
		ldx = linet[i].delta.cx;
		ldy = linet[i].delta.cy;

		if (chx)
		{
			lsx = -lsx;
			ldx = -ldx;
		}
		if (chy)
		{
			lsy = -lsy;
			ldy = -ldy;
		}
		if (chxy)
		{
			temp = ldx;
			ldx = ldy;
			ldy = temp;
			temp = lsx;
			lsx = lsy;
			lsy = temp;
		}
		lsx -= msx;
		lsy -= msy;
		if (chxy)
		{
			lsx = CENTER_YCLICK(lsx);
			lsy = CENTER_XCLICK(lsy);
		}
		else
		{
			lsx = CENTER_XCLICK(lsx);
			lsy = CENTER_YCLICK(lsy);
		}
		if (*height < lsy + (ldy < 0 ? ldy : 0))
			continue;
		if (0 > lsy + (ldy < 0 ? 0 : ldy))
			continue;

		mirror = chx ^ chy ^ chxy;
		if (ldx < 0)
		{
			lsx += ldx;
			ldx = -ldx;
			lsy += ldy;
			ldy = -ldy;
			mirror ^= 1;
		}

		start = MAX(0, lsx);
		end = MIN(*mindone + 1, lsx + ldx);
		if (start > end)
			continue;

		sy = LINEY(mdx, mdy, mbase, start);
		prod = (start - lsx) * ldy - (sy - lsy) * ldx;

		if (!prod)
		{
			if (!ldx && (lsy + (ldy < 0 ? ldy : 0) > sy ||
						 lsy + (ldy < 0 ? 0 : ldy) < sy))
				continue;
			if ((prod = -lsx * ldy + lsy * ldx) > 0 == mirror || prod == 0)
				continue;
			start--;
		}
		else
		{
			if (prod > 0 == mirror)
				continue;
			ey = LINEY(mdx, mdy, mbase, end);
			if (ABS(prod) >= ldx && ABS((prod = (end - lsx) * ldy - (ey - lsy) * ldx)) >= ldx && prod > 0 != mirror)
				continue;
			{
				int schs, sche;
				double diff = ((double)(-mbase) / mdx - (double)(lsx)*ldy / ldx + lsy);
				double diff2 = (double)mdy / mdx - (double)ldy / ldx;

				if (ABS(diff2) < 1. / (50000. * 50000))
				{
					if (diff > 0 || diff < -1)
						continue;
					else
					{
						schs = start + 1;
						sche = end;
					}
				}
				/* Can this float->int conversion cause overflows?
				 * If so, calculate min/max before conversion. */
				else if (diff2 < 0)
				{
					schs = MAX(start + 1, (int)((diff + 1) / diff2 + .9));
					sche = MIN(end, (int)(diff / diff2 + 1.1));
				}
				else
				{
					schs = MAX(start + 1, (int)(diff / diff2 + .9));
					sche = MIN(end, (int)((diff + 1) / diff2 + 1.1));
				}

				for (x = schs; x <= sche; x++)
					if ((prod = (x - lsx) * ldy - (LINEY(mdx, mdy, mbase, x) - lsy) * ldx) >= 0 == mirror || prod == 0)
						goto found;
				continue;
			found:
				start = x - 1;
			}
		}

		/* delta components can be big, so (float) to avoid overflow */
		if (start < *mindone || (start == *mindone && *minline != -1 && SIDE((float)move->delta.cx, (float)move->delta.cy, i) < 0))
		{
			hit = 1;
			*mindone = start;
			*minline = i;
			*height = LINEY(mdx, mdy, mbase, start);
		}
	}
	return hit;
}

/* Try to move a pointlike object along the path determined by *move.
 * The amount moved is returned in *answer. If the movement hits a line
 * the number of that line is included. The 'point' parameter in
 * struct collans is not relevant to this function. Even if the movement
 * doesn't hit a line, the amount moved can be less than the requested
 * amount (so that this function needs to be called again for the rest).
 * If the movement hits several lines at the same click, the line returned
 * in collans will be such that it is hit "from the outside" if possible.
 * Example:
 * X
 * X inside of polygon
 * X
 * *YYYYYYYYYY
 *  3
 *   2
 *    1
 * Here the lines X and Y meet at click *, and the movement marked with
 * numbers hits the common point. The function will return line Y as the
 * one that was hit, because it was hit "from the outside" as opposed to
 * line X which was hit from the side facing the inside of the polygon.
 * See  the comments for function 'Away' to see how the trajectory can
 * hit a line even though it is moving away from the line. */
/* Do not call this with no movement. */
/* May not be called with point already on top of line.
 * Maybe I should change that to allow lines that could be crossed. */
void Move_point(const move_t *move, struct collans *answer)
{
	int minline, mindone, minheight;
	int block;
	int msx = move->start.cx, msy = move->start.cy;
	int mdx = move->delta.cx, mdy = move->delta.cy;
	int mbase;
	int chxy = 0, chx = 0, chy = 0;
	int x, temp;
	unsigned short *lines;

	block = (move->start.cx >> B_SHIFT) + mapx * (move->start.cy >> B_SHIFT);
	x = blockline[block].distance;
	lines = blockline[block].lines;

	if (mdx < 0)
	{
		mdx = -mdx;
		msx = -msx;
		chx = 1;
	}
	if (mdy < 0)
	{
		mdy = -mdy;
		msy = -msy;
		chy = 1;
	}
	if (mdx < mdy)
	{
		temp = mdx;
		mdx = mdy;
		mdy = temp;
		temp = msx;
		msx = msy;
		msy = temp;
		chxy = 1;
	}

	/* 46341*46341 overflows signed 32-bit int */
	if (mdx > 45000)
	{
		/* might overflow without float */
		mdy = (int)((float)mdy * 45000 / mdx);
		mdx = 45000;
	}

	mindone = mdx;
	minheight = mdy;
	mdx++;
	mdy++;
	mbase = mdy >> 1;

	if (mindone > x)
	{
		if (x < MAX_MOVE)
		{
			/* !@# change this so that the point always moves away from
			   the current block */
			temp = msx > 0 ? B_CLICKS - (msx & B_MASK) : (-msx) & B_MASK;
			temp = MIN(temp,
					   (msy > 0 ? B_CLICKS - (msy & B_MASK) : -msy & B_MASK));
			x += temp;
			x = MIN(x, MAX_MOVE);
		}
		if (mindone > x)
		{
			mindone = x;
			minheight = LINEY(mdx, mdy, mbase, mindone);
		}
	}
	minline = -1;

	Lines_check(msx, msy, mdx, mdy, &mindone, lines, chx, chy, chxy,
				move, &minline, &minheight);

	answer->line = minline;
	if (chxy)
	{
		temp = mindone;
		mindone = minheight;
		minheight = temp;
	}
	if (chx)
		mindone = -mindone;
	if (chy)
		minheight = -minheight;
	answer->moved.cx = mindone;
	answer->moved.cy = minheight;

	return;
}

/* Similar to Move_point above, except that it gets the shape parameter
 * (and direction of that shape), and in case of collision, the 'point'
 * field in struct collans is used. A corner in the shape can hit a map
 * line, or a corner in the map can hit a shape line. The 'line' field
 * will contain the line, the 'point' field the corner (given as a line
 * whose first point is that corner - that is always possible because
 * polygons are closed and there is at least one line ending at and one
 * line starting from a given point). */
/* Do not call this with no movement. */
/* May not be called with point already on top of line.
   maybe I should change that to allow lines which could be
   crossed. */
/* This could be sped up by a lot in several ways if needed.
 * For example, there's no need to consider all the points
 * separately if the shape is not close to a wall.
 */
static void Shape_move(const move_t *move, shape_t *s,
					   int dir, struct collans *answer)
{
	int minline, mindone, minheight, minpoint;
	int i, block;
	int msx = move->start.cx, msy = move->start.cy;
	int mdx = move->delta.cx, mdy = move->delta.cy;
	int mbase;
	int chxy = 0, chx = 0, chy = 0;
	int x, temp;
	unsigned short *lines;
	unsigned short *points;
	clpos_t *pts;

	if (mdx < 0)
	{
		mdx = -mdx;
		chx = 1;
	}
	if (mdy < 0)
	{
		mdy = -mdy;
		chy = 1;
	}
	if (mdx < mdy)
	{
		temp = mdx;
		mdx = mdy;
		mdy = temp;
		chxy = 1;
	}

	/* 46341*46341 overflows signed 32-bit int */
	if (mdx > 45000)
	{
		/* might overflow without float */
		mdy = (int)((float)mdy * 45000 / mdx);
		mdx = 45000;
	}

	mindone = mdx;
	minheight = mdy;

	mdx++;
	mdy++;
	mbase = mdy >> 1;
	minline = -1;
	minpoint = -1;

	pts = Shape_get_points((shape_t *)s, dir);
	for (i = 0; i < s->num_points; i++)
	{
		clpos_t pt = pts[i];

		msx = WRAP_XCLICK(move->start.cx + pt.cx);
		msy = WRAP_YCLICK(move->start.cy + pt.cy);
		block = (msx >> B_SHIFT) + mapx * (msy >> B_SHIFT);
		if (chx)
			msx = -msx;
		if (chy)
			msy = -msy;
		if (chxy)
		{
			temp = msx;
			msx = msy;
			msy = temp;
		}

		x = blockline[block].distance;
		lines = blockline[block].lines;

		if (mindone > x)
		{
			if (x < MAX_MOVE)
			{
				temp = msx > 0 ? B_CLICKS - (msx & B_MASK) : -msx & B_MASK;
				temp = MIN(temp,
						   (msy > 0 ? B_CLICKS - (msy & B_MASK)
									: -msy & B_MASK));
				x += temp;
				x = MIN(x, MAX_MOVE);
			}
			if (mindone > x)
			{
				mindone = x;
				minheight = LINEY(mdx, mdy, mbase, mindone);
			}
		}

		if (Lines_check(msx, msy, mdx, mdy, &mindone, lines, chx, chy,
						chxy, move, &minline, &minheight))
			minpoint = i;
	}

	block = (move->start.cx >> B_SHIFT) + mapx * (move->start.cy >> B_SHIFT);
	points = blockline[block].points;
	lines = Shape_lines(s, dir);
	x = -1;
	while ((i = *points++) != 65535)
	{
		if (linet[i].group && (!can_hit(&groups[linet[i].group], move)))
			continue;
		msx = move->start.cx - linet[i].start.cx;
		msy = move->start.cy - linet[i].start.cy;
		if (chx)
			msx = -msx;
		if (chy)
			msy = -msy;
		if (chxy)
		{
			temp = msx;
			msx = msy;
			msy = temp;
		}
		if (Lines_check(msx, msy, mdx, mdy, &mindone, lines, chx, chy,
						chxy, move, &minline, &minheight))
			minpoint = i;
	}

	answer->point = minpoint;
	answer->line = minline;
	answer->point = minpoint;
	if (chxy)
	{
		temp = mindone;
		mindone = minheight;
		minheight = temp;
	}
	if (chx)
		mindone = -mindone;
	if (chy)
		minheight = -minheight;
	answer->moved.cx = mindone;
	answer->moved.cy = minheight;

	return;
}

/* Check whether there is room at the given position (x, y) to change
 * from the shape shape1/dir1 to shape shape2/dir2. The shapes must have
 * the same number of points. The morphing of the shapes happens linearly
 * for each point. This should work correctly even if the intermediate
 * shapes are degenerate or illegal shapes (the intermediate stages are
 * not explicitly constructed in the algorithm). Return the number of a group
 * that would be hit during morphing or NO_GROUP if there is enough room. */
/* This might be useful elsewhere in the code, need not be kept static */
static int Shape_morph(shape_t *shape1, int dir1,
					   shape_t *shape2, int dir2,
					   hitmask_t hitmask, const object_t *obj,
					   int x, int y,
					   struct collans *myanswer)
{
	struct collans answer;
	int i, p, xo1, xo2, yo1, yo2, xn1, xn2, yn1, yn2, xp, yp, s, t;
	unsigned short *points;
	move_t mv;
	/*clpos_t *pts1, *pts2;*/
	int num_points;

	mv.hitmask = hitmask;
	mv.obj = obj;
	/*pts1 = Shape_get_points((shape_t *)shape1, dir1);
	  pts2 = Shape_get_points((shape_t *)shape2, dir2);*/

	/* kps - can this happen ?? */
	if (shape1->num_points != shape2->num_points)
		warn("Shape_morph: shapes have different number of points!");

	num_points = shape1->num_points;

	for (i = 0; i < num_points; i++)
	{
		clpos_t pt1, pt2;
		/*clpos_t ptx1, ptx2;

		  ptx1 = pts1[i];
		  ptx2 = pts2[i];*/
		pt1 = Ship_get_point_clpos((shipshape_t *)shape1, i, dir1);
		pt2 = Ship_get_point_clpos((shipshape_t *)shape2, i, dir2);

		/*assert(ptx1.cx == pt1.cx);
		  assert(ptx1.cy == pt1.cy);
		  assert(ptx2.cx == pt2.cx);
		  assert(ptx2.cy == pt2.cy);*/

		mv.start.cx = x + pt1.cx;
		mv.start.cy = y + pt1.cy;
		mv.delta.cx = x + pt2.cx - mv.start.cx;
		mv.delta.cy = y + pt2.cy - mv.start.cy;
		mv.start.cx = WRAP_XCLICK(mv.start.cx);
		mv.start.cy = WRAP_YCLICK(mv.start.cy);
		while (mv.delta.cx || mv.delta.cy)
		{
			Move_point(&mv, &answer);
			if (answer.line != -1)
			{
				/* report what lines/points/vectors caused the move to fail*/
				myanswer->line = answer.line;
				myanswer->point = i;
				myanswer->moved.cx = mv.delta.cx;
				myanswer->moved.cy = mv.delta.cy;
				return linet[answer.line].group;
			}
			mv.start.cx = WRAP_XCLICK(mv.start.cx + answer.moved.cx);
			mv.start.cy = WRAP_YCLICK(mv.start.cy + answer.moved.cy);
			mv.delta.cx -= answer.moved.cx;
			mv.delta.cy -= answer.moved.cy;
		}
	}

	/* Convex shapes would be much easier. */
	points = blockline[(x >> B_SHIFT) + mapx * (y >> B_SHIFT)].points;
	while ((p = *points++) != 65535)
	{
		clpos_t pto1, ptn1;

		if (linet[p].group && (!can_hit(&groups[linet[p].group], &mv)))
			continue;
		xp = CENTER_XCLICK(linet[p].start.cx - x);
		yp = CENTER_YCLICK(linet[p].start.cy - y);

		/*pto1 = pts1[num_points - 1];
		  ptn1 = pts2[num_points - 1];*/
		pto1 = Ship_get_point_clpos((shipshape_t *)shape1, num_points - 1, dir1);
		ptn1 = Ship_get_point_clpos((shipshape_t *)shape2, num_points - 1, dir2);

		xo1 = pto1.cx - xp;
		yo1 = pto1.cy - yp;
		xn1 = ptn1.cx - xp;
		yn1 = ptn1.cy - yp;
		t = 0;

		for (i = 0; i < num_points; i++)
		{
			clpos_t pto2, ptn2;

			/*pto2 = pts1[i];
			  ptn2 = pts2[i];*/
			pto2 = Ship_get_point_clpos((shipshape_t *)shape1, i, dir1);
			ptn2 = Ship_get_point_clpos((shipshape_t *)shape2, i, dir2);

			xo2 = pto2.cx - xp;
			yo2 = pto2.cy - yp;
			xn2 = ptn2.cx - xp;
			yn2 = ptn2.cy - yp;

#define TEMPFUNC(X1, Y1, X2, Y2)                                             \
	myanswer->line = -1;                                                     \
	myanswer->point = i;                                                     \
	myanswer->moved.cx = (X2) - (X1);                                        \
	myanswer->moved.cy = (Y2) - (Y1);                                        \
	if ((X1) < 0)                                                            \
	{                                                                        \
		if ((X2) >= 0)                                                       \
		{                                                                    \
			if ((Y1) > 0 && (Y2) >= 0)                                       \
				t++;                                                         \
			else if (((Y1) >= 0 || (Y2) >= 0) &&                             \
					 (s = (X1) * ((Y1) - (Y2)) - (Y1) * ((X1) - (X2))) >= 0) \
			{                                                                \
				if (s == 0)                                                  \
				{                                                            \
					return linet[p].group;                                   \
				}                                                            \
				else                                                         \
					t++;                                                     \
			}                                                                \
		}                                                                    \
	}                                                                        \
	else                                                                     \
	{                                                                        \
		if ((X2) <= 0)                                                       \
		{                                                                    \
			if ((X2) == 0)                                                   \
			{                                                                \
				if ((Y2) == 0 || ((X1) == 0 && (((Y1) <= 0 && (Y2) >= 0) ||  \
												((Y1) >= 0 && (Y2) <= 0))))  \
				{                                                            \
					return linet[p].group;                                   \
				}                                                            \
			}                                                                \
			else if ((Y1) > 0 && (Y2) >= 0)                                  \
				t++;                                                         \
			else if (((Y1) >= 0 || (Y2) >= 0) &&                             \
					 (s = (X1) * ((Y1) - (Y2)) - (Y1) * ((X1) - (X2))) <= 0) \
			{                                                                \
				if (s == 0)                                                  \
					return linet[p].group;                                   \
				else                                                         \
					t++;                                                     \
			}                                                                \
		}                                                                    \
	}

			TEMPFUNC(xo1, yo1, xn1, yn1);
			TEMPFUNC(xn1, yn1, xn2, yn2);
			TEMPFUNC(xn2, yn2, xo2, yo2);
			TEMPFUNC(xo2, yo2, xo1, yo1);
#undef TEMPFUNC

			if (t & 1)
			{
				myanswer->line = -1; /*p not a line, but point*/
				myanswer->point = i;
				myanswer->moved.cx = p;
				myanswer->moved.cy = 0;
				return linet[p].group;
			}
			xo1 = xo2;
			yo1 = yo2;
			xn1 = xn2;
			yn1 = yn2;
		}
	}
	return NO_GROUP;
}

/* Try to move one click away from a line after a collision. Needed because
 * otherwise we could keep hitting it even though direction of movement
 * was away from the line. Example case:
 * XXX
 *   1*XX
 *     34XXX
 *       56 XXX
 *         78  XXX
 * Line (marked with 'X') and path of object (marked with numbers) collide
 * at '*' because of discreteness even though the object is moving away
 * from the line.
 * Return -1 if successful, number of another line blocking movement
 * otherwise (the number of the line is not used when writing this). */
static int Away(move_t *move, int line)
{
	int dx, dy, lsx, lsy;
	clvec_t delta_saved;
	struct collans ans;

	lsx = linet[line].start.cx - move->start.cx;
	lsy = linet[line].start.cy - move->start.cy;
	lsx = CENTER_XCLICK(lsx);
	lsy = CENTER_YCLICK(lsy);

	if (ABS(linet[line].delta.cx) >= ABS(linet[line].delta.cy))
	{
		dx = 0;
		dy = -SIGN(linet[line].delta.cx);
	}
	else
	{
		dy = 0;
		dx = SIGN(linet[line].delta.cy);
	}

	if ((ABS(lsx) > SEPARATION_DIST || ABS(lsy) > SEPARATION_DIST) && (ABS(lsx + linet[line].delta.cx) > SEPARATION_DIST || ABS(lsy + linet[line].delta.cy) > SEPARATION_DIST))
	{
		move->start.cx = WRAP_XCLICK(move->start.cx + dx);
		move->start.cy = WRAP_YCLICK(move->start.cy + dy);
		return -1;
	}

	delta_saved = move->delta;
	move->delta.cx = dx;
	move->delta.cy = dy;
	Move_point(move, &ans);
	move->start.cx = WRAP_XCLICK(move->start.cx + ans.moved.cx);
	move->start.cy = WRAP_YCLICK(move->start.cy + ans.moved.cy);
	move->delta = delta_saved;
	return ans.line;
}

/* Used internally to get a point out of a tight corner where there is
 * no room to move it. Situation like this:
 * E***X
 *     Y***XX
 *         YY**XXX
 *             YYY*XXXX
 *                 YYYYXXXXX
 *                     YYYY XXXXX
 *                         YYYY  XXXXX
 *                             YYYY   XXXXX
 *                                 YYYY !  XXXXX
 *                                     YYYY     XXXXX
 *                                         YYYY      XXXXX
 *                                             YYYY       XXXXX
 * X and Y are lines meeting at E. '*' is where lines overlap. An object
 * can get to position !, but is hard to move out of there (and the situation
 * can be worse than shown here - the angles of the lines don't need to
 * differ so much that the gap would get 1 click bigger for each step of
 * the y coordinate). This function is used to move the object a bit
 * outwards from the corner so discreteness doesn't make moving it so hard. */
/* This function should get called only rarely, so it doesn't need to
 * be too efficient. */
static int Clear_corner(move_t *move, object_t *obj, int l1, int l2)
{
	int x, y, xm, ym;
	int l1sx, l2sx, l1sy, l2sy;

	l1sx = move->start.cx - linet[l1].start.cx;
	l1sy = move->start.cy - linet[l1].start.cy;
	l1sx = CENTER_XCLICK(l1sx);
	l1sy = CENTER_YCLICK(l1sy);
	l2sx = move->start.cx - linet[l2].start.cx;
	l2sy = move->start.cy - linet[l2].start.cy;
	l2sx = CENTER_XCLICK(l2sx);
	l2sy = CENTER_YCLICK(l2sy);

	for (;;)
	{
		if (SIDE(obj->vel.x, obj->vel.y, l1) < 0)
		{
			if (!Bounce_object(obj, move, l1, 0))
				return 0;
		}
		if (SIDE(obj->vel.x, obj->vel.y, l2) < 0)
		{
			if (!Bounce_object(obj, move, l2, 0))
				return 0;
			continue;
		}
		break;
	}

	xm = SIGN(obj->vel.x);
	ym = SIGN(obj->vel.y);

#define TMPFUNC(X, Y) (SIDE((X) + l1sx, (Y) + l1sy, l1) <= 0 || SIDE((X) + l2sx, (Y) + l2sy, l2) <= 0)

	if (ABS(obj->vel.x) >= ABS(obj->vel.y))
	{
		x = xm;
		y = 0;
		for (;;)
		{
			if (TMPFUNC(x, y))
			{
				y += ym;
				if (!TMPFUNC(x, y + ym))
					break;
				else
					x += xm;
			}
			else
			{
				if (TMPFUNC(x, y + 1) && TMPFUNC(x, y - 1))
					x += xm;
				else
					break;
			}
		}
		move->delta.cx -= x;
		move->delta.cy -= y;
		if ((obj->vel.x >= 0) ^ (move->delta.cx >= 0))
		{
			move->delta.cx = 0;
			move->delta.cy = 0;
		}
	}
	else
	{
		x = 0;
		y = ym;
		for (;;)
		{
			if (TMPFUNC(x, y))
			{
				x += xm;
				if (!TMPFUNC(x + xm, y))
					break;
				else
					y += ym;
			}
			else
			{
				if (TMPFUNC(x + 1, y) && TMPFUNC(x - 1, y))
					y += ym;
				else
					break;
			}
		}

#undef TMPFUNC

		move->delta.cx -= x;
		move->delta.cy -= y;
		if ((obj->vel.y >= 0) ^ (move->delta.cy >= 0))
		{
			move->delta.cx = 0;
			move->delta.cy = 0;
		}
	}
	move->start.cx = WRAP_XCLICK(move->start.cx + x);
	move->start.cy = WRAP_YCLICK(move->start.cy + y);
	return 1;
}

/* Move a shape away from a line after a collision. Needed for the same
 * reason as Away(). */
static int Shape_away(move_t *move, shape_t *s,
					  int dir, int line, struct collans *ans)
{
	int dx, dy;
	clvec_t delta_saved;

	if (ABS(linet[line].delta.cx) >= ABS(linet[line].delta.cy))
	{
		dx = 0;
		dy = -SIGN(linet[line].delta.cx);
	}
	else
	{
		dy = 0;
		dx = SIGN(linet[line].delta.cy);
	}

	delta_saved = move->delta;
	move->delta.cx = dx;
	move->delta.cy = dy;
	Shape_move(move, s, dir, ans);
	move->start.cx = WRAP_XCLICK(move->start.cx + ans->moved.cx);
	move->start.cy = WRAP_YCLICK(move->start.cy + ans->moved.cy);
	move->delta = delta_saved;
	return ans->line == -1 && ans->point == -1;
}

static void store_byte(int value, unsigned char **start, int *offset, int *sz)
{
	(*start)[(*offset)++] = value;
	if (*offset == *sz)
	{
		*sz *= 2;
		*start = (unsigned char *)ralloc(*start, *sz);
	}
}

static void store_2byte(int value, unsigned char **start, int *offset, int *sz)
{
	store_byte(value >> 8, start, offset, sz);
	store_byte(value & 0xff, start, offset, sz);
}

static void store_4byte(int value, unsigned char **start, int *offset, int *sz)
{
	store_2byte(value >> 16, start, offset, sz);
	store_2byte(value & 0xffff, start, offset, sz);
}

int Polys_to_client(unsigned char **start)
{
	int i, j, startx, starty, dx, dy;
	int *edges;
	int size, offset;
#define STORE1(x) store_byte(x, start, &offset, &size)
#define STORE2(x) store_2byte(x, start, &offset, &size)
#define STORE4(x) store_4byte(x, start, &offset, &size)

	*start = (unsigned char *)ralloc(NULL, 100);
	size = 100;
	offset = 0;

	STORE1(num_pstyles);
	STORE1(num_estyles);
	STORE1(num_bstyles);
	for (i = 0; i < num_pstyles; i++)
	{
		STORE4(pstyles[i].color);
		STORE1(pstyles[i].texture_id);
		STORE1(pstyles[i].defedge_id);
		STORE1(pstyles[i].flags);
	}
	for (i = 0; i < num_estyles; i++)
	{
		STORE1(estyles[i].width);
		STORE4(estyles[i].color);
		STORE1(estyles[i].style);
	}
	for (i = 0; i < num_bstyles; i++)
	{
		j = 0;
		while (1)
		{
			STORE1(bstyles[i].filename[j]);
			if (!bstyles[i].filename[j])
				break;
			j++;
		}
		STORE1(bstyles[i].flags);
	}
	STORE2(num_polys);
	for (i = 0; i < num_polys; i++)
	{
		STORE1(pdata[i].style);
		j = pdata[i].num_points;
		STORE2(pdata[i].num_echanges);
		edges = estyleptr + pdata[i].estyles_start;
		while (*edges != INT_MAX)
			STORE2(*edges++);
		startx = pdata[i].pos.cx;
		starty = pdata[i].pos.cy;
		edges = edgeptr + pdata[i].edges;
		STORE2(j);
		STORE2(startx >> CLICK_SHIFT);
		STORE2(starty >> CLICK_SHIFT);
		dx = startx;
		dy = starty;
		for (; j > 0; j--)
		{
			dx += *edges++;
			dy += *edges++;
			if (j != 1)
			{
				STORE2((dx >> CLICK_SHIFT) - (startx >> CLICK_SHIFT));
				STORE2((dy >> CLICK_SHIFT) - (starty >> CLICK_SHIFT));
			}
			startx = dx;
			starty = dy;
		}
	}
	STORE1(Num_bases());
	for (i = 0; i < Num_bases(); i++)
	{
		base_t *base = Base_by_index(i);
		if (base->team == TEAM_NOT_SET)
			STORE1(0);
		else
			STORE1(base->team);
		STORE2(base->pos.cx >> CLICK_SHIFT);
		STORE2(base->pos.cy >> CLICK_SHIFT);
		STORE1(base->dir);
	}
	STORE2(Num_fuels());
	for (i = 0; i < Num_fuels(); i++)
	{
		fuel_t *fs = Fuel_by_index(i);

		STORE2(fs->pos.cx >> CLICK_SHIFT);
		STORE2(fs->pos.cy >> CLICK_SHIFT);
	}
	STORE1(world->NumChecks);
	for (i = 0; i < world->NumChecks; i++)
	{
		STORE2(world->checks[i].pos.cx >> CLICK_SHIFT);
		STORE2(world->checks[i].pos.cy >> CLICK_SHIFT);
	}
	return offset;
}

struct tempy
{
	short y;
	struct tempy *next;
};

struct templine
{
	short x1, x2, y1, y2;
	struct templine *next;
};

/* Check whether the given position (cx, cy) is such that it is inside
 * a polygon belonging to a group that could be hit by the given
 * hitmask/object.
 * Return the number of a group that would be hit or NO_GROUP. */
int is_inside(int cx, int cy, hitmask_t hitmask, const object_t *obj)
{
	short *ptr;
	int inside, cx1, cx2, cy1, cy2, s;
	struct inside_block *gblock;
	move_t mv;

#if 0
    /* kps - is_inside seems to assume that cx and cy are inside the map */
    clpos_t pos;

    pos.cx = cx;
    pos.cy = cy;

    assert(World_contains_clpos(&World, pos));
#endif

	mv.hitmask = hitmask;
	mv.obj = obj;
	gblock = &inside_table[(cx >> B_SHIFT) + mapx * (cy >> B_SHIFT)];
	if (gblock->group == NO_GROUP)
		return NO_GROUP;
	do
	{
		if (gblock->group && (!can_hit(&groups[gblock->group], &mv)))
		{
			gblock = gblock->next;
			continue;
		}
		inside = gblock->base_value;
		if (gblock->lines == NULL)
		{
			if (inside)
				return gblock->group;
			else
			{
				gblock = gblock->next;
				continue;
			}
		}
		cx &= B_MASK;
		cy &= B_MASK;
		ptr = gblock->y;
		if (ptr)
			while (cy > *ptr++)
				inside++;
		ptr = gblock->lines;
		while (*ptr != 32767)
		{
			cx1 = *ptr++ - cx;
			cy1 = *ptr++ - cy;
			cx2 = *ptr++ - cx;
			cy2 = *ptr++ - cy;
			if (cy1 < 0)
			{
				if (cy2 >= 0)
				{
					if (cx1 > 0 && cx2 >= 0)
						inside++;
					else if ((cx1 >= 0 || cx2 >= 0) &&
							 (s = cy1 * (cx1 - cx2) - cx1 * (cy1 - cy2)) >= 0)
					{
						if (s == 0)
							return gblock->group;
						else
							inside++;
					}
				}
			}
			else if (cy2 <= 0)
			{
				if (cy2 == 0)
				{
					if (cx2 == 0 || (cy1 == 0 && ((cx1 <= 0 && cx2 >= 0) ||
												  (cx1 >= 0 && cx2 <= 0))))
						return gblock->group;
				}
				else if (cx1 > 0 && cx2 >= 0)
					inside++;
				else if ((cx1 >= 0 || cx2 >= 0) &&
						 (s = cy1 * (cx1 - cx2) - cx1 * (cy1 - cy2)) <= 0)
				{
					if (s == 0)
						return gblock->group;
					else
						inside++;
				}
			}
		}
		if (inside & 1)
			return gblock->group;
		gblock = gblock->next;
	} while (gblock);
	return NO_GROUP;
}

/* Similar to the above, except check whether any part of the shape
 * (edge or inside) would hit the group. */
int shape_is_inside(int cx, int cy, hitmask_t hitmask, const object_t *obj,
					shape_t *s, int dir)
{
	static clpos_t zeropos;
	static shape_t zeroshape;
	int i, group;
	struct collans ans;

	/* Implemented by first checking whether the middle point of the
	 * shape is on top of something. If not, check whether it is possible
	 * to enlarge a degenerate shape where all points are on top of each
	 * other (at the middle point) to the given one. (So it relies on the
	 * rule that the shape must contain the middle point. */

	if ((group = is_inside(cx, cy, hitmask, obj)) != NO_GROUP)
		return group;

	/*
	 * kps - Ship numpoints can be > MAX_SHIP_PTS because of
	 * SSHACK. This should somehow be fixed.
	 */
	zeroshape.num_points = s->num_points;

	if (zeroshape.pts[0] == NULL)
	{
		for (i = 0; i < MAX_SHIP_PTS2; i++)
			zeroshape.pts[i] = &zeropos;
	}

	return Shape_morph(&zeroshape, 0, s, dir, hitmask, obj, cx, cy, &ans);
}

static void closest_line(int bx, int by, double dist, int inside)
{
	if (dist <= temparray[bx + mapx * by].distance)
	{
		if (dist == temparray[bx + mapx * by].distance)
			/* Must be joined polygons(s) if the map is legal
			 * (the same line appears in both directions).
			 * Both sides of this line are inside. */
			/* These lines could be removed from the table as a minor
			 * optimization. */
			inside = 1;
		temparray[bx + mapx * by].distance = dist;
		temparray[bx + mapx * by].inside = inside;
	}
}

static void insert_y(int block, int y)
{
	struct tempy *ptr;
	struct tempy **prev;

	ptr = temparray[block].y;
	prev = &temparray[block].y;
	while (ptr && ptr->y < y)
	{
		prev = &ptr->next;
		ptr = ptr->next;
	}
	if (ptr && ptr->y == y)
	{
		*prev = ptr->next;
		free(ptr);
		return;
	}
	*prev = (struct tempy *)ralloc(NULL, sizeof(struct tempy));
	(*prev)->y = y;
	(*prev)->next = ptr;
}

static void store_inside_line(int bx, int by, int ox, int oy, int dx, int dy)
{
	int block;
	struct templine *s;

	block = bx + mapx * by;
	ox = CENTER_XCLICK(ox - bx * B_CLICKS);
	oy = CENTER_YCLICK(oy - by * B_CLICKS);
	if (oy >= 0 && oy < B_CLICKS && ox >= B_CLICKS)
		insert_y(block, oy);
	if (oy + dy >= 0 && oy + dy < B_CLICKS && ox + dx >= B_CLICKS)
		insert_y(block, oy + dy);
	s = (struct templine *)ralloc(NULL, sizeof(struct templine));
	s->x1 = ox;
	s->x2 = ox + dx;
	s->y1 = oy;
	s->y2 = oy + dy;
	s->next = temparray[block].lines;
	temparray[block].lines = s;
}

static void finish_inside(int block, int group)
{
	int inside;
	struct inside_block *gblock;
	short *ptr;
	int cx1, cx2, cy1, cy2, s, j;
	struct tempy *yptr;
	struct templine *lptr;
	void *tofree;

	gblock = &inside_table[block];
	if (gblock->group != NO_GROUP)
	{
		while (gblock->next) /* Maintain group order*/
			gblock = gblock->next;
		gblock->next = (struct inside_block *)ralloc(NULL, sizeof(struct inside_block));
		gblock = gblock->next;
	}
	gblock->group = group;
	gblock->next = NULL;
	j = 0;
	yptr = temparray[block].y;
	while (yptr)
	{
		j++;
		yptr = yptr->next;
	}
	if (j > 0)
	{
		ptr = (short *)ralloc(NULL, (j + 1) * sizeof(short));
		gblock->y = ptr;
		yptr = temparray[block].y;
		while (yptr)
		{
			*ptr++ = yptr->y;
			tofree = yptr;
			yptr = yptr->next;
			free(tofree);
		}
		*ptr = 32767;
	}
	else
		gblock->y = NULL;
	j = 0;
	lptr = temparray[block].lines;
	while (lptr)
	{
		j++;
		lptr = lptr->next;
	}
	if (j > 0)
	{
		ptr = (short *)ralloc(NULL, (j * 4 + 1) * sizeof(short));
		gblock->lines = ptr;
		lptr = temparray[block].lines;
		while (lptr)
		{
			*ptr++ = lptr->x1;
			*ptr++ = lptr->y1;
			*ptr++ = lptr->x2;
			*ptr++ = lptr->y2;
			tofree = lptr;
			lptr = lptr->next;
			free(tofree);
		}
		*ptr = 32767;
	}
	else
		gblock->lines = NULL;
	inside = temparray[block].inside;
	if ((ptr = gblock->lines) != NULL)
	{
		while (*ptr != 32767)
		{
			cx1 = *ptr++ * 2 - B_CLICKS * 2 + 1;
			cy1 = *ptr++ * 2 + 1;
			cx2 = *ptr++ * 2 - B_CLICKS * 2 + 1;
			cy2 = *ptr++ * 2 + 1;
			if (cy1 < 0)
			{
				if (cy2 >= 0)
				{
					if (cx1 > 0 && cx2 >= 0)
						inside++;
					else if ((cx1 >= 0 || cx2 >= 0) &&
							 (s = cy1 * (cx1 - cx2) - cx1 * (cy1 - cy2)) > 0)
						inside++;
				}
			}
			else if (cy2 <= 0)
			{
				if (cx1 > 0 && cx2 >= 0)
					inside++;
				else if ((cx1 >= 0 || cx2 >= 0) &&
						 (s = cy1 * (cx1 - cx2) - cx1 * (cy1 - cy2)) < 0)
					inside++;
			}
		}
	}
	gblock->base_value = inside & 1;
	temparray[block].y = NULL;
	temparray[block].lines = NULL;
	temparray[block].inside = 2;
	temparray[block].distance = 1e20;
}

static void allocate_inside(void)
{
	int i;

	inside_table = (struct inside_block *)
		ralloc(NULL, mapx * mapy * sizeof(struct inside_block));
	temparray = (struct test *)
		ralloc(NULL, mapx * mapy * sizeof(struct test));
	for (i = 0; i < mapx * mapy; i++)
	{
		temparray[i].distance = 1e20;
		temparray[i].inside = 2;
		temparray[i].y = NULL;
		temparray[i].lines = NULL;
		inside_table[i].y = NULL;
		inside_table[i].lines = NULL;
		inside_table[i].base_value = 0;
		inside_table[i].group = NO_GROUP;
		inside_table[i].next = NULL;
	}
}

/* Calculate distance of intersection from lower right corner of the
 * block counterclockwise along the edge. We don't return the true lengths
 * but values which compare the same with each other.
 * 'dir' is used to return whether the block is left through a horizontal
 * or a vertical side or a corner. */
static double edge_distance(int bx, int by, int ox, int oy, int dx, int dy,
							int *dir)
{
	int last_width = (world->cwidth - 1) % B_CLICKS + 1;
	int last_height = (world->cheight - 1) % B_CLICKS + 1;
	double xdist, ydist, dist;

	ox = CENTER_XCLICK(ox - bx * B_CLICKS);
	oy = CENTER_YCLICK(oy - by * B_CLICKS);
	if (dx > 0)
		xdist = ((bx == mapx - 1) ? last_width : B_CLICKS) - .5 - ox;
	else if (dx < 0)
		xdist = ox + .5;
	else
		xdist = 1e20; /* Something big enough to be > ydist, dx */
	if (dy > 0)
		ydist = ((by == mapy - 1) ? last_height : B_CLICKS) - .5 - oy;
	else if (dy < 0)
		ydist = oy + .5;
	else
		ydist = 1e20;
	if (xdist > ABS(dx) && ydist > ABS(dy))
		return -1; /* Doesn't cross box boundary */
	if (ABS(dy) * xdist == ABS(dx) * ydist)
		*dir = 3;
	else if (ABS(dy) * xdist < ABS(dx) * ydist)
		*dir = 1;
	else
		*dir = 2;
	if (*dir == 1)
		if (dx > 0)
			dist = oy + dy * xdist / dx;
		else
			dist = 5 * B_CLICKS - oy + dy * xdist / dx;
	else if (dy > 0)
		dist = 3 * B_CLICKS - ox - dx * ydist / dy;
	else
		dist = 6 * B_CLICKS + ox - dx * ydist / dy;
	return dist;
}

#define POSMOD(x, y) ((x) >= 0 ? (x) % (y) : ((x) + 1) % (y) + (y) - 1)
static void Inside_init(void)
{
	int dx, dy, bx, by, ox, oy, startx, starty;
	int i, j, num_points, minx = -1, miny = -1, poly, group;
	int bx2, by2, maxx = -1, maxy = -1, dir;
	double dist;
	int *edges;

	allocate_inside();
	for (group = 0; group < num_groups; group++)
	{
		minx = -1;
		for (poly = 0; poly < num_polys; poly++)
		{
			if (pdata[poly].is_decor || pdata[poly].group != group)
				continue;
			num_points = pdata[poly].num_points;
			dx = 0;
			dy = 0;
			startx = pdata[poly].pos.cx;
			starty = pdata[poly].pos.cy;
			/* Better wrapping for bx2/by2 could be selected for speed here,
			 * but this keeping track of min/max at all is probably
			 * unnoticeable in practice. */
			bx2 = bx = startx >> B_SHIFT;
			by2 = by = starty >> B_SHIFT;
			if (minx == -1)
			{
				minx = maxx = bx2;
				miny = maxy = by2;
			}
			edges = edgeptr + pdata[poly].edges;
			closest_line(bx, by, 1e10, 0); /* For polygons within one block */
			for (j = 0; j < num_points; j++)
			{
				if (((startx >> B_SHIFT) != bx) || ((starty >> B_SHIFT) != by))
				{
					warn("Inside_init: went into infinite loop...");
					while (1)
						;
				}
				ox = startx & B_MASK;
				oy = starty & B_MASK;
				dx = *edges++;
				dy = *edges++;
				while (1)
				{ /* All blocks containing a part of this line */
					store_inside_line(bx, by, startx, starty, dx, dy);
					dist = edge_distance(bx, by, WRAP_XCLICK(startx + dx),
										 WRAP_YCLICK(starty + dy), -dx, -dy, &dir);
					if (dist != -1)
						closest_line(bx, by, dist, 1);
					dist = edge_distance(bx, by, startx, starty, dx, dy, &dir);
					if (dist == -1)
						break;
					closest_line(bx, by, dist, 0);
					if (dir == 1 || dir == 3)
						bx2 += (dx > 0) ? 1 : -1;
					if (bx2 > maxx)
						maxx = bx2;
					if (bx2 < minx)
						minx = bx2;
					bx = POSMOD(bx2, mapx);
					if (dir == 2 || dir == 3)
						by2 += (dy > 0) ? 1 : -1;
					if (by2 > maxy)
						maxy = by2;
					if (by2 < miny)
						miny = by2;
					by = POSMOD(by2, mapy);
				}
				startx = WRAP_XCLICK(startx + dx);
				starty = WRAP_YCLICK(starty + dy);
			}
		}
		if (minx == -1)
			continue;
		bx = maxx - minx + 1;
		if (bx > 2 * mapx)
			bx = 2 * mapx;
		by = maxy - miny + 1;
		if (by > mapy)
			by = mapy;
		for (i = POSMOD(miny, mapy); by-- > 0; i++)
		{
			if (i == mapy)
				i = 0;
			bx2 = bx;
			dir = 0;
			for (j = POSMOD(minx, mapx); bx2-- > 0; j++)
			{
				if (j == mapx)
					j = 0;
				if (temparray[j + mapx * i].inside < 2)
				{
					dir = temparray[j + mapx * i].distance > B_CLICKS &&
						  temparray[j + mapx * i].inside == 1;
				}
				else
				{
					if (dir)
						temparray[i * mapx + j].inside = 1;
				}
				if (bx2 < mapx)
					finish_inside(j + mapx * i, group);
			}
		}
	}
	return;
}

/* Include NCLLIN - 1 closest lines or all closer than CUTOFF (whichever
 * is less) in the line table for this block.
 * Include all lines closer than DICLOSE, however many there are.
 * LINSIZE tells the amout of temporary memory to reserve for the algorithm.
 * If it is not large enough to hold the required lines, print error and
 * exit.
 */

#define DICLOSE (5 * CLICK)
#define LINSIZE 100
#define NCLLIN (10 + 1)
static void Distance_init(void)
{
	int cx, cy;
	int *lineno, *dis;
	int lsx, lsy, ldx, ldy, temp, dist, n, i, bx, by, j, k;
	int base, height2, by2, width, height;
	int distbound, size;
	unsigned short *lptr;

	/* max line delta 30000 */

	blockline = (struct blockinfo *)
		ralloc(NULL, mapx * mapy * sizeof(struct blockinfo));
	lineno = (int *)ralloc(NULL, mapx * mapy * LINSIZE * sizeof(int));
	dis = (int *)ralloc(NULL, mapx * mapy * LINSIZE * sizeof(int));
	size = 1; /* start with end marker */
	for (bx = 0; bx < mapx; bx++)
		for (by = 0; by < mapy; by++)
			for (i = 0; i < LINSIZE; i++)
			{
				dis[(by * mapx + bx) * LINSIZE + i] = MAX_MOVE + B_CLICKS / 2;
				lineno[(by * mapx + bx) * LINSIZE + i] = 65535;
			}
	for (i = 0; i < num_lines; i++)
	{
		bx = linet[i].start.cx;
		by = linet[i].start.cy;
		width = linet[i].delta.cx;
		height = linet[i].delta.cy;
		if (width < 0)
		{
			bx += width;
			width = -width;
			bx = WRAP_XCLICK(bx);
		}
		if (height < 0)
		{
			by += height;
			height = -height;
			by = WRAP_YCLICK(by);
		}
		width = (width + 2 * MAX_MOVE) / B_CLICKS + 5;
		if (width >= mapx)
			width = mapx;
		height = (height + 2 * MAX_MOVE) / B_CLICKS + 5;
		if (height >= mapy)
			height = mapy;
		bx = (bx - MAX_MOVE) / B_CLICKS - 2;
		by = (by - MAX_MOVE) / B_CLICKS - 2;
		while (bx < 0)
			bx += mapx;
		while (by < 0)
			by += mapy;
		height2 = height;
		by2 = by;
		for (; width-- > 0; bx = bx == mapx - 1 ? 0 : bx + 1)
			for (by = by2, height = height2;
				 height-- > 0;
				 by = by == mapy - 1 ? 0 : by + 1)
			{
				cx = bx * B_CLICKS + B_CLICKS / 2;
				cy = by * B_CLICKS + B_CLICKS / 2;
				base = (by * mapx + bx) * LINSIZE;
				lsx = CENTER_XCLICK(linet[i].start.cx - cx);
				if (ABS(lsx) > 32767 + MAX_MOVE + B_CLICKS / 2)
					continue;
				lsy = CENTER_YCLICK(linet[i].start.cy - cy);
				if (ABS(lsy) > 32767 + MAX_MOVE + B_CLICKS / 2)
					continue;
				ldx = linet[i].delta.cx;
				ldy = linet[i].delta.cy;
				if (MAX(ABS(lsx), ABS(lsy)) > MAX(ABS(lsx + ldx),
												  ABS(lsy + ldy)))
				{
					lsx += ldx;
					ldx = -ldx;
					lsy += ldy;
					ldy = -ldy;
				}
				if (ABS(lsx) < ABS(lsy))
				{
					temp = lsx;
					lsx = lsy;
					lsy = temp;
					temp = ldx;
					ldx = ldy;
					ldy = temp;
				}
				if (lsx < 0)
				{
					lsx = -lsx;
					ldx = -ldx;
				}
				if (ldx >= 0)
					dist = lsx - 1;
				else
				{
					if (lsy + ldy < 0)
					{
						lsy = -lsy;
						ldy = -ldy;
					}
					temp = lsy - lsx;
					lsx += lsy;
					lsy = temp;
					temp = ldy - ldx;
					ldx += ldy;
					ldy = temp;
					dist = lsx - ldx * lsy / ldy;
					if (lsx + ldx < 0)
						dist = MIN(ABS(dist), ABS(lsy - ldy * lsx / ldx));
					dist = dist / 2 - 3; /* 3? didn't bother to get the right value */
				}
				if (dist < CUTOFF + B_CLICKS / 2)
				{
					if (dist < B_CLICKS / 2 + DICLOSE)
						distbound = LINSIZE;
					else
						distbound = NCLLIN;
					for (j = 1; j < distbound; j++)
					{
						if (dis[base + j] <= dist)
							continue;
						k = dis[base + j];
						n = j;
						for (j++; j < distbound; j++)
							if (dis[base + j] > k)
							{
								k = dis[base + j];
								n = j;
							}
						if (dis[base + 0] > dis[base + n])
							dis[base + 0] = dis[base + n];
						if (lineno[base + n] == 65535)
						{
							size++; /* more saved lines */
							if (n == 1)
								size++; /* first this block, for 65535 */
						}
						dis[base + n] = dist;
						lineno[base + n] = i;
						goto stored;
					}
				}
				if (dist < dis[base + 0])
					dis[base + 0] = dist;
				if (dist < B_CLICKS / 2 + DICLOSE)
				{
					printf("Not enough space in line table. "
						   "Fix allocation in walls.c\n");
					exit(1);
				}
			stored:; /* semicolon for ansi compatibility */
			}
	}
	llist = (unsigned short *)ralloc(NULL, size * sizeof(unsigned short));
	lptr = llist;
	*lptr++ = 65535; /* All blocks with no lines stored point to this. */
	for (bx = 0; bx < mapx; bx++)
		for (by = 0; by < mapy; by++)
		{
			base = (by * mapx + bx) * LINSIZE;
			k = bx + mapx * by;
			blockline[k].distance = dis[base + 0] - B_CLICKS / 2;
			if (lineno[base + 1] == 65535)
				blockline[k].lines = llist;
			else
			{
				blockline[k].lines = lptr;
				for (j = 1; j < LINSIZE && lineno[base + j] != 65535; j++)
					*lptr++ = lineno[base + j];
				*lptr++ = 65535;
			}
		}
	free(lineno);
	free(dis);
}

/*
  cut and paste from #xpilot irc channel:

<uau> the current values used for creating the wall tables are unoptimal
<kps> what wall tables ?
<kps> corners, etc ?
<uau> especially after you made the "ship size" value bigger
	  (for asteroids IIRC?) it can be bad
<kps> do i make that ship size smaller or what do you suggest ?
<kps> or how should this be done if one wanted to make it right ?
<uau> the server creates a table for "nearby" lines for each 32x32 pixel block
<uau> where "nearby" depends on how close closest lines are
<uau> and then later creates a table of corners for the same distance+shipsize
<kps> is it struct blockinfo *blockline; ?
<uau> if shipsize is big then the latter can include lots of corners
<uau> blockline.distance is the distance for which features are listed for
	  that block
<uau> blockline.lines contains all lines that are within that distance of
	  the block (away from the block edges, whole inside has value 0)
<uau> distance is measured as MIN of x,y distance
<uau> blockline.points contains all corners (identified as a line starting
	  from that point) within distance blockline.distance+C from that block
<uau> where C was some constant which depends on ship size
<uau> the heuristic for choosing a suitable blockline.distance could be
	  improved
<kps> that would help how ?
<uau> now it always includes some lines IIRC; it would probably be better
	  to choose the maximum distance such that no lines are included instead
	  if that is big enough
<uau> MAX_SHAPE_OFFSET being big is a separate problem
<uau> now blockline.distance is chosen based on the lines only,
	  blockline.points is then calculated afterwards to allow shapes to be
	  moved "compatibly"
<uau> if MAX_SHAPE_OFFSET is big that means a value for blockline.distance
	  which is good for moving objects can create inefficiently big corner
	  lists for in blockline.points
<kps> maybe it says there is not enough corner space since my xp map to
	  polygon conversion function creates many polygons where one could
	  have only one
<kps> so there is a lot of corners close to each other
<uau> the problem place is one where there is a lot of free space around,
	  so that blockline.distance can be chosen large
<uau> but then there are suddenly a lot of corners inside
	  distance+MAX_SHAPE_OFFSET
 */

static void Corner_init(void)
{
	int bx, by, cx, cy, dist, i;
	unsigned short *ptr, *temp;
	int block, size = mapx * mapy;
	int height, height2, width, by2;

#define DISIZE 350
	temp = (unsigned short *)
		ralloc(NULL, mapx * mapy * DISIZE * sizeof(unsigned short)); /* !@# */
	for (i = 0; i < mapx * mapy; i++)
		temp[i * DISIZE] = 0;
	for (i = 0; i < num_lines; i++)
	{
		bx = linet[i].start.cx;
		by = linet[i].start.cy;
		width = height = (2 * MAX_MOVE) / B_CLICKS + 7;
		if (width >= mapx)
			width = mapx;
		if (height >= mapy)
			height = mapy;
		bx = (bx - MAX_MOVE) / B_CLICKS - 3;
		by = (by - MAX_MOVE) / B_CLICKS - 3;
		while (bx < 0)
			bx += mapx;
		while (by < 0)
			by += mapy;
		height2 = height;
		by2 = by;
		for (; width-- > 0; bx = bx == mapx - 1 ? 0 : bx + 1)
			for (by = by2, height = height2;
				 height-- > 0;
				 by = by == mapy - 1 ? 0 : by + 1)
			{
				block = bx + mapx * by;
				dist = blockline[block].distance + MAX_SHAPE_OFFSET + B_CLICKS / 2;
				cx = bx * B_CLICKS + B_CLICKS / 2;
				cy = by * B_CLICKS + B_CLICKS / 2;
				if (ABS(CENTER_XCLICK(linet[i].start.cx - cx)) > dist)
					continue;
				if (ABS(CENTER_YCLICK(linet[i].start.cy - cy)) > dist)
					continue;
				temp[++temp[DISIZE * block] + DISIZE * block] = i;
				size++;
			}
	}
	plist = (unsigned short *)ralloc(NULL, size * sizeof(unsigned short));
	ptr = plist;
	for (block = 0; block < mapx * mapy; block++)
	{
		blockline[block].points = ptr;
		i = temp[block * DISIZE];
		if (i > DISIZE - 1)
		{
			warn("Not enough corner space in walls.c, add more.");
			exit(1);
		}
		while (i > 0)
		{
			*ptr++ = temp[block * DISIZE + i];
			i--;
		}
		*ptr++ = 65535;
	}
	free(temp);
#undef DISIZE
}

void Ball_line_init(void)
{
	int i;
	static clpos_t coords[MAX_SHIP_PTS];

	LIMIT(options.ballRadius, 0, BALL_RADIUS);
	ball_wire.num_points = MAX_SHIP_PTS;
	for (i = 0; i < MAX_SHIP_PTS; i++)
	{
		ball_wire.pts[i] = coords + i;
		coords[i].cx = (click_t)(cos(i * 2 * PI / MAX_SHIP_PTS) * options.ballRadius * CLICK);
		coords[i].cy = (click_t)(sin(i * 2 * PI / MAX_SHIP_PTS) * options.ballRadius * CLICK);
	}

	return;
}

static void Poly_to_lines(void)
{
	int i, np, j, startx, starty, dx, dy, group, *styleptr, style;
	int *edges;

	num_lines = 0;
	for (i = 0; i < num_polys; i++)
	{
		if (pdata[i].is_decor)
			continue;
		group = pdata[i].group;
		np = pdata[i].num_points;
		styleptr = estyleptr + pdata[i].estyles_start;
		style = pstyles[pdata[i].style].defedge_id;
		dx = 0;
		dy = 0;
		startx = pdata[i].pos.cx;
		starty = pdata[i].pos.cy;
		edges = edgeptr + pdata[i].edges;
		for (j = 0; j < np; j++)
		{
			if (j == *styleptr)
			{
				styleptr++;
				style = *styleptr++;
			}
			if (style == 0)
			{
				dx += *edges++;
				dy += *edges++;
				continue;
			}
			if (!(num_lines % 2000))
				linet = (struct bline *)
					ralloc(linet, (num_lines + 2000) * sizeof(struct bline));
			linet[num_lines].group = group;
			linet[num_lines].start.cx = TWRAP_XCLICK(startx + dx);
			linet[num_lines].start.cy = TWRAP_YCLICK(starty + dy);
			linet[num_lines].delta.cx = *edges;
			dx += *edges++;
			linet[num_lines++].delta.cy = *edges;
			dy += *edges++;
		}
		if (dx || dy)
		{
			warn("Broken map: Polygon %d (%d points) doesn't form a "
				 "closed loop",
				 i + 1, np);
			exit(1);
		}
	}
	linet = (struct bline *)
		ralloc(linet, (num_lines + S_LINES) * sizeof(struct bline));
	for (i = num_lines; i < num_lines + S_LINES; i++)
		linet[i].group = 0; /* initialize for Shape_lines */
	return;
}

void Walls_init(void)
{
	double x, y, l2;
	int i;

	mapx = (world->cwidth + B_MASK) >> B_SHIFT;
	mapy = (world->cheight + B_MASK) >> B_SHIFT;

	/* Break polygons down to a list of separate lines. */
	Poly_to_lines();

	/* For each B_CLICKS x B_CLICKS rectangle on the map, find a list of
	 * nearby lines that need to be checked for collision when moving
	 * in that area. */
	Distance_init();

	/* Like above, except list the map corners that could be hit by the
	 * sides of a moving polygon shape. */
	Corner_init();

	Ball_line_init();

	/* Initialize the data structures used when determining whether a given
	 * arbitrary point on the map is inside something. */
	Inside_init();

	/* Precalculate the .c and .s values used when calculating a bounce
	 * from the line. */
	for (i = 0; i < num_lines; i++)
	{
		x = linet[i].delta.cx;
		y = linet[i].delta.cy;
		l2 = (x * x + y * y);
		linet[i].c = (x * x - y * y) / l2;
		linet[i].s = 2 * x * y / l2;
	}

	if (is_polygon_map)
	{
		if (options.mapData)
		{
			warn("Option mapData is not supported on polygon maps.");
			warn("Server automatically creates block map from polygons.");
		}
		Create_blockmap_from_polygons();
	}
}

static void Move_asteroid(object_t *obj)
{
	move_t mv;
	struct collans ans;
	wireobject_t *asteroid = (wireobject_t *)obj;

	mv.delta.cx = FLOAT_TO_CLICK(obj->vel.x * timeStep);
	mv.delta.cy = FLOAT_TO_CLICK(obj->vel.y * timeStep);
	mv.obj = obj;
	obj->extmove.cx = mv.delta.cx;
	obj->extmove.cy = mv.delta.cy;

	/* asteroid can't phaze */

	mv.hitmask = NONBALL_BIT; /* hit everything for now */

	mv.start.cx = obj->pos.cx;
	mv.start.cy = obj->pos.cy;
	while (mv.delta.cx || mv.delta.cy)
	{
		shape_t *shape = Asteroid_get_shape_by_size(asteroid->wire_size);

		assert(shape);
		Shape_move(&mv, shape, 0, &ans);
		mv.start.cx = WRAP_XCLICK(mv.start.cx + ans.moved.cx);
		mv.start.cy = WRAP_YCLICK(mv.start.cy + ans.moved.cy);
		mv.delta.cx -= ans.moved.cx;
		mv.delta.cy -= ans.moved.cy;
		if (ans.line != -1)
		{
			if (SIDE(obj->vel.x, obj->vel.y, ans.line) < 0)
			{
				if (!Bounce_object(obj, &mv, ans.line, ans.point))
					break;
			}
			else if (!Shape_away(&mv, shape, 0, ans.line, &ans))
			{
				if (SIDE(obj->vel.x, obj->vel.y, ans.line) < 0)
				{
					if (!Bounce_object(obj, &mv, ans.line, ans.point))
						break;
				}
				else
				{
					/* This case could be handled better,
					 * I'll write the code for that if this
					 * happens too often. */
					mv.delta.cx = 0;
					mv.delta.cy = 0;
					obj->vel.x = 0;
					obj->vel.y = 0;
				}
			}
		}
	}
	Object_position_set_clvec(obj, mv.start);
	Cell_add_object(obj);
	return;
}

static void Move_ball(object_t *obj)
{
	move_t mv;
	struct collans ans;
	int owner;

	mv.delta.cx = FLOAT_TO_CLICK(obj->vel.x * timeStep);
	mv.delta.cy = FLOAT_TO_CLICK(obj->vel.y * timeStep);
	mv.obj = obj;
	obj->extmove.cx = mv.delta.cx;
	obj->extmove.cy = mv.delta.cy;

	if (obj->id != NO_ID && Player_is_phasing(Player_by_id(obj->id)))
	{
		clpos_t pos;

		pos.cx = obj->pos.cx + mv.delta.cx;
		pos.cy = obj->pos.cy + mv.delta.cy;
		pos = World_wrap_clpos(pos);
		Object_position_set_clpos(obj, pos);
		Cell_add_object(obj);
		return;
	}
	owner = BALL_PTR(obj)->ball_owner;
	if (owner == NO_ID)
		mv.hitmask = BALL_BIT | NOTEAM_BIT;
	else
		mv.hitmask = BALL_BIT | HITMASK(Player_by_id(owner)->team);
	mv.start.cx = obj->pos.cx;
	mv.start.cy = obj->pos.cy;
	while (mv.delta.cx || mv.delta.cy)
	{
		Shape_move(&mv, &ball_wire, 0, &ans);
		mv.start.cx = WRAP_XCLICK(mv.start.cx + ans.moved.cx);
		mv.start.cy = WRAP_YCLICK(mv.start.cy + ans.moved.cy);
		mv.delta.cx -= ans.moved.cx;
		mv.delta.cy -= ans.moved.cy;
		if (ans.line != -1)
		{
			if (SIDE(obj->vel.x, obj->vel.y, ans.line) < 0)
			{
				if (!Bounce_object(obj, &mv, ans.line, ans.point))
					break;
			}
			else if (!Shape_away(&mv, &ball_wire, 0, ans.line, &ans))
			{
				if (SIDE(obj->vel.x, obj->vel.y, ans.line) < 0)
				{
					if (!Bounce_object(obj, &mv, ans.line, ans.point))
						break;
				}
				else
				{
					/* This case could be handled better,
					 * I'll write the code for that if this
					 * happens too often. */
					mv.delta.cx = 0;
					mv.delta.cy = 0;
					obj->vel.x = 0;
					obj->vel.y = 0;
				}
			}
		}
	}
	Object_position_set_clvec(obj, mv.start);
	Cell_add_object(obj);
	return;
}

void Move_object(object_t *obj)
{
	int t;
	move_t mv;
	struct collans ans;
	int trycount = 5000;
	int team; /* !@# should make TEAM_NOT_SET 0 */

	mv.obj = obj;
	Object_position_remember(obj);

	obj->collmode = 1;

	if (obj->type == OBJ_ASTEROID)
	{
		Move_asteroid(obj);
		return;
	}

#if 1
	if (obj->type == OBJ_BALL)
	{
		Move_ball(obj);
		return;
	}
#else
	if (obj->type == OBJ_BALL)
	{
		if (obj->owner != NO_ID)
			team = Player_by_id(obj->owner).team;
		else
			team = TEAM_NOT_SET;
		mv.hitmask = BALL_BIT;
	}
	else
#endif
	{
		mv.hitmask = NONBALL_BIT;
		team = obj->team;
	}
	mv.hitmask |= HITMASK(team);
	mv.start.cx = obj->pos.cx;
	mv.start.cy = obj->pos.cy;
	mv.delta.cx = FLOAT_TO_CLICK(obj->vel.x * timeStep);
	mv.delta.cy = FLOAT_TO_CLICK(obj->vel.y * timeStep);
	obj->extmove.cx = mv.delta.cx;
	obj->extmove.cy = mv.delta.cy;
	while (mv.delta.cx || mv.delta.cy)
	{
		if (!trycount--)
		{
			sprintf(msg, "COULDN'T MOVE OBJECT!!!! Type = %s, x = %d, y = %d. "
						 "Object was DELETED. [*DEBUG*]",
					Object_typename(obj), mv.start.cx, mv.start.cy);
			warn(msg);
			Set_message(msg);
			obj->life = 0;
			return;
		}
		Move_point(&mv, &ans);
		mv.delta.cx -= ans.moved.cx;
		mv.delta.cy -= ans.moved.cy;
		mv.start.cx = WRAP_XCLICK(mv.start.cx + ans.moved.cx);
		mv.start.cy = WRAP_YCLICK(mv.start.cy + ans.moved.cy);
		if (ans.line != -1)
		{
			if (SIDE(obj->vel.x, obj->vel.y, ans.line) < 0)
			{
				if (!Bounce_object(obj, &mv, ans.line, 0))
					break; /* Object destroyed by bounce */
			}
			else if ((t = Away(&mv, ans.line)) != -1)
			{
				if (!Clear_corner(&mv, obj, ans.line, t))
					break; /* Object destroyed by bounces */
			}
		}
	}
	Object_position_set_clvec(obj, mv.start);
	Cell_add_object(obj);
	return;
}

bool in_move_player = false;

void Move_player(player_t *pl)
{
	clpos_t pos;
	move_t mv;
	struct collans ans;
	double fric = friction;
	vector_t oldv;

	if (!Player_is_alive(pl))
	{
		pos.cx = pl->pos.cx + FLOAT_TO_CLICK(pl->vel.x * timeStep);
		pos.cy = pl->pos.cy + FLOAT_TO_CLICK(pl->vel.y * timeStep);
		pos.cx = WRAP_XCLICK(pos.cx);
		pos.cy = WRAP_YCLICK(pos.cy);
		if (pos.cx != pl->pos.cx || pos.cy != pl->pos.cy)
		{
			Object_position_remember(OBJ_PTR(pl));
			Object_position_set_clpos(OBJ_PTR(pl), pos);
		}
		pl->velocity = VECTOR_LENGTH(pl->vel);
		return;
	}

	/* Figure out which friction to use. */
	if (Player_is_phasing(pl))
		fric = friction;
	else if (Num_frictionAreas() > 0)
	{
		int group, i;

		in_move_player = true;

		group = is_inside(pl->pos.cx, pl->pos.cy, NONBALL_BIT, (object_t *)pl);
		if (group != NO_GROUP)
		{
			for (i = 0; i < Num_frictionAreas(); i++)
			{
				friction_area_t *fa = FrictionArea_by_index(i);

				if (fa->group == group)
				{
					fric = fa->friction;
					break;
				}
			}
		}

		in_move_player = false;
	}

	/* Velocity vector might change as a result of friction and options.coriolis. */
	if (options.coriolis != 0.0)
	{
		oldv = pl->vel;
		pl->vel.x = oldv.x * coriolisCosine + oldv.y * coriolisSine;
		pl->vel.y = oldv.y * coriolisCosine - oldv.x * coriolisSine;
	}

	if (fric != 0.0)
	{
		pl->vel.x *= (1.0 - fric);
		pl->vel.y *= (1.0 - fric);
	}

	Object_position_remember(OBJ_PTR(pl));

	pl->collmode = 1;

	mv.obj = OBJ_PTR(pl);
	mv.delta.cx = FLOAT_TO_CLICK(pl->vel.x * timeStep);
	mv.delta.cy = FLOAT_TO_CLICK(pl->vel.y * timeStep);
#if 0
    pl->extmove.cx = mv.delta.cx;
    pl->extmove.cy = mv.delta.cy;
#endif

	if (Player_is_phasing(pl))
	{
		pos.cx = pl->pos.cx + mv.delta.cx;
		pos.cy = pl->pos.cy + mv.delta.cy;
		pos = World_wrap_clpos(pos);
		Object_position_set_clpos(OBJ_PTR(pl), pos);
	}
	else
	{
		mv.hitmask = NONBALL_BIT | HITMASK(pl->team);
		mv.start.cx = pl->pos.cx;
		mv.start.cy = pl->pos.cy;
		while (mv.delta.cx || mv.delta.cy)
		{
			Shape_move(&mv, (shape_t *)pl->ship, pl->dir, &ans);
			mv.start.cx = WRAP_XCLICK(mv.start.cx + ans.moved.cx);
			mv.start.cy = WRAP_YCLICK(mv.start.cy + ans.moved.cy);
			mv.delta.cx -= ans.moved.cx;
			mv.delta.cy -= ans.moved.cy;
			if (ans.line != -1)
			{
				if (SIDE(pl->vel.x, pl->vel.y, ans.line) < 0)
				{
					Bounce_player(pl, &mv, ans.line, ans.point);
					if (Player_is_killed(pl))
						break;
				}
				else if (!Shape_away(&mv, (shape_t *)pl->ship, pl->dir,
									 ans.line, &ans))
				{
					if (SIDE(pl->vel.x, pl->vel.y, ans.line) < 0)
					{
						Bounce_player(pl, &mv, ans.line, ans.point);
						if (Player_is_killed(pl))
							break;
					}
					else
					{
						/* This case could be handled better,
						 * I'll write the code for that if this
						 * happens too often. */
						/* At the moment it can be caused by illegal shipshapes
						 * too, because they're not checked */
#if 0
			sprintf(msg, "%s got stuck (Illegal shape? "
				"Shapes aren't checked) [*Notice*]", pl->name);
			Set_message(msg);
#endif
						mv.delta.cx = 0;
						mv.delta.cy = 0;
						pl->vel.x = 0;
						pl->vel.y = 0;
					}
				}
			}
		}
		Object_position_set_clvec(OBJ_PTR(pl), mv.start);
	}
	pl->velocity = VECTOR_LENGTH(pl->vel);
	/* !@# Better than ignoring collisions after wall touch for players,
	 * but might cause some erroneous hits */
	pl->extmove.cx = CENTER_XCLICK(pl->pos.cx - pl->prevpos.cx);
	pl->extmove.cy = CENTER_YCLICK(pl->pos.cy - pl->prevpos.cy);
	return;
}

void Turn_player(player_t *pl, bool push)
{
	int new_dir = MOD2((int)(pl->float_dir + 0.5), RES);
	int next_dir, sign, group;
	hitmask_t hitmask;
	struct collans ans;
	double length, relturn;
	clpos_t p, p2;

	if (recOpt)
	{
		if (record)
			*playback_data++ = new_dir;
		else if (playback)
			new_dir = *playback_data++;
	}

	if (new_dir == pl->dir)
		return;

	if (!Player_is_alive(pl))
	{
		/* kps - what is the point of this ??? */
		/* virus - it prevents you from turning ship while ur dead ;) */
		/* kps - why is pl->dir set to new_dir then ? */
		pl->dir = new_dir;
		return;
	}

	if (Player_is_phasing(pl))
	{
		pl->dir = new_dir;
		return;
	}

	if (new_dir > pl->dir)
	{
		if (new_dir - pl->dir <= RES + pl->dir - new_dir)
		{
			sign = 1;
			relturn = (new_dir - pl->dir) / (double)RES;
		}
		else
		{
			sign = -1;
			relturn = (RES + pl->dir - new_dir) / (double)RES;
		}
	}
	else
	{
		if (pl->dir - new_dir <= RES + new_dir - pl->dir)
		{
			sign = -1;
			relturn = (pl->dir - new_dir) / (double)RES;
		}
		else
		{
			sign = 1;
			relturn = (RES + new_dir - pl->dir) / (double)RES;
		}
	}

	hitmask = NONBALL_BIT | HITMASK(pl->team);

	while (pl->dir != new_dir)
	{
		next_dir = MOD2(pl->dir + sign, RES);
		group = Shape_morph((shape_t *)pl->ship, pl->dir, (shape_t *)pl->ship,
							next_dir, hitmask, OBJ_PTR(pl),
							pl->pos.cx, pl->pos.cy, &ans);
		if (group != NO_GROUP)
		{
			double /*fact, */ velon, velot;
			double cl, sl;	 /* cosine and sine of line angle    	    */
			double cln, sln; /* cosine and sine of line normal   	    */
			double pc, ps;	 /* cosine and sine of the points    	    */
			double pdc, pds; /* cosine and sine of the points direction  */
			double x, y, l /*, v*/;
			double power = pl->power;
			int a = (BIT(pl->used, USES_EMERGENCY_THRUST)
						 ? MAX_AFTERBURNER
						 : pl->item[ITEM_AFTERBURNER]);
			double inert = pl->mass;
			double cx, cy;
			static move_t move;

			Player_set_float_dir(pl, (double)pl->dir);

			if (!push)
				break;

			length = 0;
			l = 0.0;

			p = Ship_get_point_clpos((shipshape_t *)pl->ship, ans.point, pl->dir);
			p2 = Ship_get_point_clpos((shipshape_t *)pl->ship, (ans.point + 1) % (((shape_t *)pl->ship)->num_points), pl->dir);

			/* x,y defines the direction of the line that prevented turning */
			if (ans.line != -1)
			{
				length = Wrap_length(linet[ans.line].delta.cx,
									 linet[ans.line].delta.cy);
				x = linet[ans.line].delta.cx;
				y = linet[ans.line].delta.cy;
			}
			else
			{
				x = p2.cx - p.cx;
				y = p2.cy - p.cy;
			}

			l = sqrt(x * x + y * y);

			if (l == 0.0)
			{
				warn("A zero length line somehow prevented turning!");
				break;
			}

			/* cosine and sine of the line's tangent */
			cl = x / l;
			sl = y / l;

			/* cosine and sine of the line's normal */
			cln = sl;
			sln = -cl;

			/* pc and ps defines cosine and sine of the point vector */
			if (ans.line != -1)
			{
				l = sqrt(p.cx * p.cx + p.cy * p.cy);
				pc = p.cx / l;
				ps = p.cy / l;
			}
			else
			{
				cx = CENTER_XCLICK(linet[ans.moved.cx].start.cx - pl->pos.cx);
				cy = CENTER_YCLICK(linet[ans.moved.cx].start.cy - pl->pos.cy);
				l = sqrt(cx * cx + cy * cy);
				pc = cx / l;
				ps = cy / l;
			}

			/*
			 * pdc pds defines cosine and sine of the points direction
			 * (if it is a wall point, its the direction of the spot
			 * of the shipline that hit it)
			 */
			if (sign == 1)
			{
				pdc = -ps;
				pds = pc;
			}
			else
			{
				pdc = ps;
				pds = -pc;
			}

			/*
			 * Shipshapes can be hit from the "inside", fixing this
			 */
			if ((ans.line == -1) && ((pdc * cln + pds * sln) > 0))
			{
				cln = -cln;
				sln = -sln;
			}

			/*
			 * Push strength depends on burner power, ship mass and
			 * constantSpeed
			 */
			if (a)
			{
				power = AFTER_BURN_POWER(power, a);
			}

			velon = (power / inert) * (1 + options.constantSpeed);

			/*
			 * If we are approaching the wall that stops our turn we
			 * need to bounce the ship. (we do this with no movement
			 * since we just want to bounce it, not move it)
			 */
			if ((pl->vel.x * cln + pl->vel.y * sln) < 0)
			{
				move.delta.cx = 0;
				move.delta.cy = 0;
				if (ans.line != -1)
					Bounce_player(pl, &move, ans.line, 0);
				else
					Bounce_player(pl, &move, ans.point, 0);
			}

			velot = velon * options.playerWallFriction * ((-pdc) * cl + (-pds) * sl);

			/*
			 * First store away the old speed vector, then do the turnpush move
			 * and then restore speed
			 */
			x = pl->vel.x;
			y = pl->vel.y;
			pl->vel.x = velon * cln + options.turnGrip * velot * cl;
			pl->vel.y = velon * sln + options.turnGrip * velot * sl;
			Move_player(pl);
			pl->vel.x = x + options.turnPushPersistence * pl->vel.x;
			pl->vel.y = y + options.turnPushPersistence * pl->vel.y;

			break;
		}
		pl->dir = next_dir;
	}

	return;
}
