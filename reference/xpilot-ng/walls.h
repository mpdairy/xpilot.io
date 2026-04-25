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

#ifndef WALLS_H
#define WALLS_H

#include <stdint.h>

#ifndef CLICK_H
#include "click.h"
#endif

#ifndef OBJECT_H
#include "object.h"
#endif

/*
 * Wall collision detection and bouncing.
 *
 * The wall collision detection routines depend on repeatability
 * (getting the same result even after some "neutral" calculations)
 * and an exact determination whether a point is in space,
 * inside the wall (crash!) or on the edge.
 * This will be hard to achieve if only floating point would be used.
 * However, a resolution of a pixel is a bit rough and ugly.
 * Therefore a fixed point sub-pixel resolution is used called clicks.
 */

#define FLOAT_TO_INT(F) ((F) < 0 ? -(int)(0.5f - (F)) : (int)((F) + 0.5f))
#define DOUBLE_TO_INT(D) ((D) < 0 ? -(int)(0.5 - (D)) : (int)((D) + 0.5))

typedef enum
{
    NotACrash = 0,
    CrashUniverse = 0x01,
    CrashWall = 0x02,
    CrashTarget = 0x04,
    CrashTreasure = 0x08,
    CrashCannon = 0x10,
    CrashUnknown = 0x20,
    CrashWormHole = 0x40,
    CrashWallSpeed = 0x80,
    CrashWallNoFuel = 0x100,
    CrashWallAngle = 0x200
} move_crash_t;

typedef struct
{
    int edge_wrap;
    int edge_bounce;
    int wall_bounce;
    int cannon_crashes;
    int target_crashes;
    int treasure_crashes;
    int wormhole_warps;
    int phased;
    object_t *obj;
    player_t *pl;
} move_info_t;

typedef struct
{
    const move_info_t *mip;
    move_crash_t crash;
    clpos_t pos;
    vector_t vel;
    clvec_t todo;
    clvec_t done;
    int dir;
    int cannon;
    int wormhole;
    int target;
    int treasure;
} move_state_t;

struct move_parameters
{
    click_t click_width;  /* Map width in clicks */
    click_t click_height; /* Map width in clicks */

    int max_shielded_angle;   /* max player bounce angle */
    int max_unshielded_angle; /* max player bounce angle */

    unsigned long obj_bounce_mask;   /* which objects bounce? */
    unsigned long obj_cannon_mask;   /* objects crash cannons? */
    unsigned long obj_target_mask;   /* object target hit? */
    unsigned long obj_treasure_mask; /* objects treasure crash? */
};

/* kps change 100, 30 etc to something sane */
struct polystyle
{
    char id[100];
    int color;
    int texture_id;
    int defedge_id;
    int flags;
};

struct edgestyle
{
    char id[100];
    int width;
    int color;
    int style;
};

struct bmpstyle
{
    char id[100];
    char filename[32];
    int flags;
};

typedef struct
{
    int style;
    int current_style;
    int destroyed_style;
    int group;
    int edges;
    clpos_t pos;
    int num_points;
    int estyles_start;
    int num_echanges;
    int is_decor;
    unsigned update_mask;
    long last_change;
} poly_t;

/*
 * Hitmasks are 32 bits.
 */
#define ALL_BITS 0xffffffffU
#define BALL_BIT (1U << 11)
#define NONBALL_BIT (1U << 12)
#define NOTEAM_BIT (1U << 10)
#define HITMASK(team) ((team) == TEAM_NOT_SET ? NOTEAM_BIT : 1U << (team))
typedef uint32_t hitmask_t;

typedef struct move
{
    clvec_t start;
    clvec_t delta;
    hitmask_t hitmask;
    const object_t *obj;
} move_t;

typedef struct group group_t;

struct group
{
    int type;
    int team;
    hitmask_t hitmask;
    bool (*hitfunc)(group_t *groupptr, const move_t *move);
    int mapobj_ind;
};

extern struct polystyle pstyles[256];
extern struct edgestyle estyles[256];
extern struct bmpstyle bstyles[256];
extern poly_t *pdata;
extern int *estyleptr;
extern int *edgeptr;
extern group_t *groups;
extern int num_groups, max_groups;

static inline group_t *groupptr_by_id(int group)
{
    if (group >= 0 && group < num_groups)
        return &groups[group];
    return NULL;
}

extern int num_polys, num_pstyles, num_estyles, num_bstyles;

struct collans
{
    int line;
    int point;
    clvec_t moved;
};

#endif
