/* $Id: paintdata.c,v 5.9 2002/04/13 16:10:59 bertg Exp $
 *
 * XPilot, a multiplayer gravity war game.  Copyright (C) 1991-2001 by
 *
 *      Bjørn Stabell        <bjoern@xpilot.org>
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
 * Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <math.h>

#ifndef _WINDOWS
# include <unistd.h>
# include <X11/Xlib.h>
#endif

#ifdef _WINDOWS
# include "NT/winX.h"
#endif

#include "version.h"
#include "config.h"
#include "const.h"
#include "error.h"
#include "bit.h"
#include "client.h"
#include "setup.h"
#include "rules.h"
#include "paint.h"
#include "paintdata.h"
#include "record.h"
#include "xinit.h"
#include "protoclient.h"
#include "dbuff.h"
#include "commonproto.h"

char paintdata_version[] = VERSION;

refuel_t	*refuel_ptr;
int		 num_refuel, max_refuel;
connector_t	*connector_ptr;
int		 num_connector, max_connector;
laser_t		*laser_ptr;
int		 num_laser, max_laser;
missile_t	*missile_ptr;
int		 num_missile, max_missile;
ball_t		*ball_ptr;
int		 num_ball, max_ball;
ship_t		*ship_ptr;
int		 num_ship, max_ship;
mine_t		*mine_ptr;
int		 num_mine, max_mine;
itemtype_t	*itemtype_ptr;
int		 num_itemtype, max_itemtype;
ecm_t		*ecm_ptr;
int		 num_ecm, max_ecm;
trans_t		*trans_ptr;
int		 num_trans, max_trans;
paused_t	*paused_ptr;
int		 num_paused, max_paused;
radar_t		*radar_ptr;
int		 num_radar, max_radar;
vcannon_t	*vcannon_ptr;
int		 num_vcannon, max_vcannon;
vfuel_t		*vfuel_ptr;
int		 num_vfuel, max_vfuel;
vbase_t		*vbase_ptr;
int		 num_vbase, max_vbase;
debris_t	*debris_ptr[DEBRIS_TYPES];
int		 num_debris[DEBRIS_TYPES],
		 max_debris[DEBRIS_TYPES];
debris_t	*fastshot_ptr[DEBRIS_TYPES * 2];
int		 num_fastshot[DEBRIS_TYPES * 2],
		 max_fastshot[DEBRIS_TYPES * 2];
vdecor_t	*vdecor_ptr;
int		 num_vdecor, max_vdecor;
wreckage_t	*wreckage_ptr;
int		 num_wreckage, max_wreckage;
asteroid_t	*asteroid_ptr;
int		 num_asteroids, max_asteroids;
wormhole_t	*wormhole_ptr;
int		 num_wormholes, max_wormholes;

long		time_left = -1;
long		start_loops, end_loops;

XRectangle	*rect_ptr[MAX_COLORS];
int		num_rect[MAX_COLORS], max_rect[MAX_COLORS];
XArc		*arc_ptr[MAX_COLORS];
int		num_arc[MAX_COLORS], max_arc[MAX_COLORS];
XSegment	*seg_ptr[MAX_COLORS];
int		num_seg[MAX_COLORS], max_seg[MAX_COLORS];

int		eyesId;		/* Player we get frame updates for */
short		snooping;	/* are we snooping on someone else? */

unsigned long	current_foreground;

erase_t		erase[2], *erp;

void Erase_do_start(void)
{
    int			i;

    if (damaged > 0) {
	error("BUG: Erase_start while damaged");
	return;
    }

    if (erase[0].flags == 0) {
	printf("ERASE is On!\n");
	erp = &erase[0];
    }
    if (BIT(erp->flags, ERASE_INITIALIZED) == 0) {
	SET_FG(colors[BLACK].pixel);
	XFillRectangle(dpy, p_draw, gc, 0, 0, draw_width, draw_height);
	SET_BIT(erp->flags, ERASE_INITIALIZED);
    }
    erp->num_rect = 0;
    erp->num_arc = 0;
    for (i = 0; i <= MAX_LINE_WIDTH; i++) {
	erp->num_seg[i] = 0;
    }
}

void Erase_do_end(void)
{
    int			i,
			linewidth = false;

    if (damaged > 0) {
	error("BUG: Erase_do_end while damaged");
	return;
    }
#ifndef _WINDOWS
    /* BG fix 2000-03-19 use same erase buffer if not color switching. */
    if (dbuf_state->type == COLOR_SWITCH) {
	if (erp == &erase[0]) {
	    erp = &erase[1];
	} else {
	    erp = &erase[0];
	}
    }
#endif
    SET_FG(colors[BLACK].pixel);

    if (erp->num_rect != 0) {
	XFillRectangles(dpy, p_draw, gc, erp->rect_ptr, erp->num_rect);
	UNEXPAND(erp->rect_ptr, erp->num_rect, erp->max_rect);
    }
    if (erp->num_arc != 0) {
	XDrawArcs(dpy, p_draw, gc, erp->arc_ptr, erp->num_arc);
	UNEXPAND(erp->arc_ptr, erp->num_arc, erp->max_arc);
    }
    for (i = 0; i <= MAX_LINE_WIDTH; i++) {
	if (erp->num_seg[i] != 0) {
	    XSetLineAttributes(dpy, gc, i,
			       LineSolid, CapProjecting, JoinMiter);
	    linewidth = true;
	    XDrawSegments(dpy, p_draw, gc, erp->seg_ptr[i], erp->num_seg[i]);
	    UNEXPAND(erp->seg_ptr[i], erp->num_seg[i], erp->max_seg[i]);
	}
    }
    if (linewidth == true) {
	XSetLineAttributes(dpy, gc, 0,
			   LineSolid, CapButt, JoinMiter);
    }
}

void Erase_do_rectangle(int x, int y, int width, int height)
{
    XRectangle		*p;

    EXPAND(erp->rect_ptr, erp->num_rect, erp->max_rect, XRectangle, 1);
    p = &erp->rect_ptr[erp->num_rect++];
    p->x = x;
    p->y = y;
    p->width = width;
    p->height = height;
}

void Erase_do_rectangles(XRectangle *rectp, int n)
{
    EXPAND(erp->rect_ptr, erp->num_rect, erp->max_rect, XRectangle, n);
    memcpy(&erp->rect_ptr[erp->num_rect], rectp, n * sizeof(XRectangle));
    erp->num_rect += n;
}

void Erase_do_arc(int x, int y, int width, int height,
		      int angle1, int angle2)
{
    XArc		*p;

    EXPAND(erp->arc_ptr, erp->num_arc, erp->max_arc, XArc, 1);
    p = &erp->arc_ptr[erp->num_arc++];
    p->x = x;
    p->y = y;
    p->width = width;
    p->height = height;
    p->angle1 = angle1;
    p->angle2 = angle2;
}

void Erase_do_arcs(XArc *arcp, int n)
{
    EXPAND(erp->arc_ptr, erp->num_arc, erp->max_arc, XArc, n);
    memcpy(&erp->arc_ptr[erp->num_arc], arcp, n * sizeof(XArc));
    erp->num_arc += n;
}

void Erase_do_segment(int width, int x1, int y1, int x2, int y2)
{
    XSegment		*p;

    EXPAND(erp->seg_ptr[width], erp->num_seg[width], erp->max_seg[width],
	   XSegment, 1);
    p = &erp->seg_ptr[width][erp->num_seg[width]++];
    p->x1 = x1;
    p->y1 = y1;
    p->x2 = x2;
    p->y2 = y2;
}

void Erase_do_segments(XSegment *segp, int n)
{
    EXPAND(erp->seg_ptr[0], erp->num_seg[0], erp->max_seg[0],
	   XSegment, n);
    memcpy(&erp->seg_ptr[0][erp->num_seg[0]], segp, n * sizeof(XSegment));
    erp->num_seg[0] += n;
}

void Erase_do_points(int width, XPoint *pointp, int n)
{
    XSegment		*p;
    int			i;

    EXPAND(erp->seg_ptr[width], erp->num_seg[width], erp->max_seg[width],
	   XSegment, n - 1);
    p = &erp->seg_ptr[width][erp->num_seg[width]];
    for (i = 1; i < n; i++) {
	p->x1 = pointp->x;
	p->y1 = pointp->y;
	pointp++;
	p->x2 = pointp->x;
	p->y2 = pointp->y;
	p++;
    }
    erp->num_seg[width] += n - 1;
}

void Erase_do_4point(int x, int y, int width, int height)
{
    XSegment		*p;

    EXPAND(erp->seg_ptr[0], erp->num_seg[0], erp->max_seg[0],
	   XSegment, 4);
    p = &erp->seg_ptr[0][erp->num_seg[0]];
    p->x1 = x;
    p->y1 = y;
    p->x2 = x + width;
    p->y2 = y;
    p++;
    p->x1 = x + width;
    p->y1 = y;
    p->x2 = x + width;
    p->y2 = y + height;
    p++;
    p->x1 = x + width;
    p->y1 = y + height;
    p->x2 = x;
    p->y2 = y + height;
    p++;
    p->x1 = x;
    p->y1 = y + height;
    p->x2 = x;
    p->y2 = y;
    p++;
    erp->num_seg[0] += 4;
}

void Rectangle_start(void)
{
    int i;

    for (i = 0; i < maxColors; i++) {
	num_rect[i] = 0;
    }
}

void Rectangle_end(void)
{
    int i;

    for (i = 0; i < maxColors; i++) {
	if (num_rect[i] > 0) {
	    SET_FG(colors[i].pixel);
	    rd.fillRectangles(dpy, p_draw, gc, rect_ptr[i], num_rect[i]);
	    Erase_rectangles(rect_ptr[i], num_rect[i]);
	    RELEASE(rect_ptr[i], num_rect[i], max_rect[i]);
	}
    }
}

int Rectangle_add(int color, int x, int y, int width, int height)
{
    XRectangle		t;

    t.x = WINSCALE(x);
    t.y = WINSCALE(y);
    t.width = WINSCALE(width);
    t.height = WINSCALE(height);

    STORE(XRectangle, rect_ptr[color], num_rect[color], max_rect[color], t);
    return 0;
}

void Arc_start(void)
{
    int i;

    for (i = 0; i < maxColors; i++) {
	num_arc[i] = 0;
    }
}

void Arc_end(void)
{
    int i;

    for (i = 0; i < maxColors; i++) {
	if (num_arc[i] > 0) {
	    SET_FG(colors[i].pixel);
	    rd.drawArcs(dpy, p_draw, gc, arc_ptr[i], num_arc[i]);
	    Erase_arcs(arc_ptr[i], num_arc[i]);
	    RELEASE(arc_ptr[i], num_arc[i], max_arc[i]);
	}
    }
}

int Arc_add(int color,
	    int x, int y,
	    int width, int height,
	    int angle1, int angle2)
{
    XArc t;

#ifndef WINDOWSCALING
    t.x = x;
    t.y = y;
    t.width = width;
    t.height = height;
#else
    t.x = WINSCALE(x);
    t.y = WINSCALE(y);
    t.width = WINSCALE(width+x) - t.x;
    t.height = WINSCALE(height+y) - t.y;
#endif
    t.angle1 = angle1;
    t.angle2 = angle2;
    STORE(XArc, arc_ptr[color], num_arc[color], max_arc[color], t);
    return 0;
}

void Segment_start(void)
{
    int i;

    for (i = 0; i < maxColors; i++) {
	num_seg[i] = 0;
    }
}

void Segment_end(void)
{
    int i;

    for (i = 0; i < maxColors; i++) {
	if (num_seg[i] > 0) {
	    SET_FG(colors[i].pixel);
	    rd.drawSegments(dpy, p_draw, gc,
		seg_ptr[i], num_seg[i]);
	    Erase_segments(seg_ptr[i], num_seg[i]);
	    RELEASE(seg_ptr[i], num_seg[i], max_seg[i]);
	}
    }
}

int Segment_add(int color, int x1, int y1, int x2, int y2)
{
    XSegment t;

    t.x1 = WINSCALE(x1);
    t.y1 = WINSCALE(y1);
    t.x2 = WINSCALE(x2);
    t.y2 = WINSCALE(y2);
    STORE(XSegment, seg_ptr[color], num_seg[color], max_seg[color], t);
    return 0;
}

int Handle_start(long server_loops)
{
    int			i;

    start_loops = server_loops;

    num_refuel = 0;
    num_connector = 0;
    num_missile = 0;
    num_ball = 0;
    num_ship = 0;
    num_mine = 0;
    num_itemtype = 0;
    num_ecm = 0;
    num_trans = 0;
    num_paused = 0;
    num_radar = 0;
    num_vcannon = 0;
    num_vfuel = 0;
    num_vbase = 0;
    num_vdecor = 0;
    for (i = 0; i < DEBRIS_TYPES; i++) {
	num_debris[i] = 0;
    }

    damaged = 0;
    destruct = 0;
    shutdown_delay = 0;
    shutdown_count = -1;
    eyesId = (self != NULL) ? self->id : 0;
    thrusttime = -1;
    shieldtime = -1;
    phasingtime = -1;
    return 0;
}

int Handle_end(long server_loops)
{
    end_loops = server_loops;
    snooping = self && (eyesId != self->id);
    Paint_frame();
    return 0;
}

int Handle_self_items(u_byte *newNumItems)
{
    memcpy(numItems, newNumItems, NUM_ITEMS * sizeof(u_byte));
    return 0;
}

int Handle_self(int x, int y, int vx, int vy, int newHeading,
		float newPower, float newTurnspeed, float newTurnresistance,
		int newLockId, int newLockDist, int newLockBearing,
		int newNextCheckPoint, int newAutopilotLight,
		u_byte *newNumItems, int newCurrentTank,
		int newFuelSum, int newFuelMax, int newPacketSize)
{
    pos.x = x;
    pos.y = y;
    vel.x = vx;
    vel.y = vy;
    heading = newHeading;
    displayedPower = newPower;
    displayedTurnspeed = newTurnspeed;
    displayedTurnresistance = newTurnresistance;
    lock_id = newLockId;
    lock_dist = newLockDist;
    lock_dir = newLockBearing;
    nextCheckPoint = newNextCheckPoint;
    autopilotLight = newAutopilotLight;
    memcpy(numItems, newNumItems, NUM_ITEMS * sizeof(u_byte));
    fuelCurrent = newCurrentTank;
    if (newFuelSum > fuelSum && selfVisible != 0) {
	fuelCount = FUEL_NOTIFY;
    }
    fuelSum = newFuelSum;
    fuelMax = newFuelMax;
    selfVisible = 0;
    if (newPacketSize + 16 < packet_size) {
	packet_size -= 16;
    } else {
	packet_size = newPacketSize;
    }

    world.x = pos.x - (ext_view_width / 2);
    world.y = pos.y - (ext_view_height / 2);
    realWorld = world;
    if (BIT(Setup->mode, WRAP_PLAY)) {
	if (world.x < 0 && world.x + ext_view_width < Setup->width) {
	    world.x += Setup->width;
	}
	else if (world.x > 0 && world.x + ext_view_width >= Setup->width) {
	    realWorld.x -= Setup->width;
	}
	if (world.y < 0 && world.y + ext_view_height < Setup->height) {
	    world.y += Setup->height;
	}
	else if (world.y > 0 && world.y + ext_view_height >= Setup->height) {
	    realWorld.y -= Setup->height;
	}
    }
    return 0;
}


int Handle_eyes(int id)
{
    eyesId = id;
    return 0;
}

int Handle_damaged(int dam)
{
    damaged = dam;
    return 0;
}

int Handle_modifiers(char *m)
{
    strlcpy(mods, m, MAX_CHARS);
    return 0;
}

int Handle_destruct(int count)
{
    destruct = count;
    return 0;
}


int Handle_shutdown(int count, int delay)
{
    shutdown_count = count;
    shutdown_delay = delay;
    return 0;
}

int Handle_thrusttime(int count, int max)
{
    thrusttime = count;
    thrusttimemax = max;
    return 0;
}

int Handle_shieldtime(int count, int max)
{
    shieldtime = count;
    shieldtimemax = max;
    return 0;
}

int Handle_phasingtime(int count, int max)
{
    phasingtime = count;
    phasingtimemax = max;
    return 0;
}

int Handle_rounddelay(int count, int max)
{
	roundDelay = count;
	roundDelayMax = max;
	return(0);
}

int Handle_refuel(int x0, int y0, int x1, int y1)
{
    refuel_t	t;

    t.x0 = x0;
    t.x1 = x1;
    t.y0 = y0;
    t.y1 = y1;
    STORE(refuel_t, refuel_ptr, num_refuel, max_refuel, t);
    return 0;
}

int Handle_connector(int x0, int y0, int x1, int y1, int tractor)
{
    connector_t	t;

    t.x0 = x0;
    t.x1 = x1;
    t.y0 = y0;
    t.y1 = y1;
    t.tractor = tractor;
    STORE(connector_t, connector_ptr, num_connector, max_connector, t);
    return 0;
}

int Handle_laser(int color, int x, int y, int len, int dir)
{
    laser_t	t;

    t.color = color;
    t.x = x;
    t.y = y;
    t.len = len;
    t.dir = dir;
    STORE(laser_t, laser_ptr, num_laser, max_laser, t);
    return 0;
}

int Handle_missile(int x, int y, int len, int dir)
{
    missile_t	t;

    t.x = x;
    t.y = y;
    t.dir = dir;
    t.len = len;
    STORE(missile_t, missile_ptr, num_missile, max_missile, t);
    return 0;
}

int Handle_ball(int x, int y, int id)
{
    ball_t	t;

    t.x = x;
    t.y = y;
    t.id = id;
    STORE(ball_t, ball_ptr, num_ball, max_ball, t);
    return 0;
}

int Handle_ship(int x, int y, int id, int dir, int shield, int cloak, int eshield, 
				int phased, int deflector)
{
    ship_t	t;

    t.x = x;
    t.y = y;
    t.id = id;
    t.dir = dir;
    t.shield = shield;
    t.cloak = cloak;
    t.eshield = eshield;
    t.phased = phased;
    t.deflector = deflector;
    STORE(ship_t, ship_ptr, num_ship, max_ship, t);

    /* if we see a ship in the center of the display, we may be watching
     * it, especially if it's us!  consider any ship there to be our eyes
     * until we see a ship that really is us.
     * BG: XXX there was a bug here.  self was dereferenced at "self->id"
     * while self could be NULL here.
     */
    if (!selfVisible && ((x == pos.x && y == pos.y) || (self && id == self->id))) {
	int radarx, radary;
        eyesId = id;
	selfVisible = (self && (id == self->id));
	radarx = (int)((double)(x * RadarWidth) / Setup->width + 0.5);
	radary = (int)((double)(y * RadarHeight) / Setup->height + 0.5);
	return Handle_radar(radarx, radary, 3);
    }

    return 0;
}

int Handle_mine(int x, int y, int teammine, int id)
{
    mine_t	t;

    t.x = x;
    t.y = y;
    t.teammine = teammine;
    t.id = id;
    STORE(mine_t, mine_ptr, num_mine, max_mine, t);
    return 0;
}

int Handle_item(int x, int y, int type)
{
    itemtype_t	t;

    t.x = x;
    t.y = y;
    t.type = type;
    STORE(itemtype_t, itemtype_ptr, num_itemtype, max_itemtype, t);
    return 0;
}

#define STORE_DEBRIS(typ_e, _p, _n) \
    if (_n > max_) {						\
	if (max_ == 0) {						\
	    ptr_ = (debris_t *)malloc(n * sizeof(*ptr_));		\
	} else {						\
	    ptr_ = (debris_t *)realloc(ptr_, _n * sizeof(*ptr_));	\
	}							\
	if (ptr_ == NULL) {					\
	    error("No memory for debris");			\
	    num_ = max_ = 0;					\
	    return -1;						\
	}							\
	max_ = _n;						\
    }								\
    else if (_n <= 0) {						\
	printf("debris %d < 0\n", _n);				\
	return 0;						\
    }								\
    num_ = _n;							\
    memcpy(ptr_, _p, _n * sizeof(*ptr_));				\
    return 0;


int Handle_fastshot(int type, u_byte *p, int n)
{
#define num_		(num_fastshot[type])
#define max_		(max_fastshot[type])
#define ptr_		(fastshot_ptr[type])
    STORE_DEBRIS(type, p, n);
#undef num_
#undef max_
#undef ptr_
}

int Handle_debris(int type, u_byte *p, int n)
{
#define num_		(num_debris[type])
#define max_		(max_debris[type])
#define ptr_		(debris_ptr[type])
    STORE_DEBRIS(type, p, n);
#undef num_
#undef max_
#undef ptr_
}

int Handle_wreckage(int x, int y, int wrecktype, int size, int rotation)
{
    wreckage_t	t;

    t.x = x;
    t.y = y;
    t.wrecktype = wrecktype;
    t.size = size;
    t.rotation = rotation;
    STORE(wreckage_t, wreckage_ptr, num_wreckage, max_wreckage, t);
    return 0;
}

int Handle_asteroid(int x, int y, int type, int size, int rotation)
{
    asteroid_t	t;

    t.x = x;
    t.y = y;
    t.type = type;
    t.size = size;
    t.rotation = rotation;
    STORE(asteroid_t, asteroid_ptr, num_asteroids, max_asteroids, t);
    return 0;
}

int Handle_wormhole(int x, int y)
{
    wormhole_t	t;

    t.x = x - BLOCK_SZ / 2;
    t.y = y - BLOCK_SZ / 2;
    STORE(wormhole_t, wormhole_ptr, num_wormholes, max_wormholes, t);
    return 0;
}

int Handle_ecm(int x, int y, int size)
{
    ecm_t	t;

    t.x = x;
    t.y = y;
    t.size = size;
    STORE(ecm_t, ecm_ptr, num_ecm, max_ecm, t);
    return 0;
}

int Handle_trans(int x1, int y1, int x2, int y2)
{
    trans_t	t;

    t.x1 = x1;
    t.y1 = y1;
    t.x2 = x2;
    t.y2 = y2;
    STORE(trans_t, trans_ptr, num_trans, max_trans, t);
    return 0;
}

int Handle_paused(int x, int y, int count)
{
    paused_t	t;

    t.x = x;
    t.y = y;
    t.count = count;
    STORE(paused_t, paused_ptr, num_paused, max_paused, t);
    return 0;
}

int Handle_radar(int x, int y, int size)
{
    radar_t	t;

    t.x = x;
    t.y = y;
    t.size = size;
    STORE(radar_t, radar_ptr, num_radar, max_radar, t);
    return 0;
}

int Handle_message(char *msg)
{
    Add_message(msg);
    return 0;
}

int Handle_time_left(long sec)
{
    if (sec >= 0 && sec < 10 && (time_left > sec || sec == 0)) {
	XBell(dpy, 0);
	XFlush(dpy);
    }
    time_left = (sec >= 0) ? sec : 0;
    return 0;
}

int Handle_vcannon(int x, int y, int type)
{
    vcannon_t	t;

    t.x = x;
    t.y = y;
    t.type = type;
    STORE(vcannon_t, vcannon_ptr, num_vcannon, max_vcannon, t);
    return 0;
}

int Handle_vfuel(int x, int y, long fuel)
{
    vfuel_t	t;

    t.x = x;
    t.y = y;
    t.fuel = fuel;
    STORE(vfuel_t, vfuel_ptr, num_vfuel, max_vfuel, t);
    return 0;
}

int Handle_vbase(int x, int y, int xi, int yi, int type)
{
    vbase_t	t;

    t.x = x;
    t.y = y;
    t.xi = xi;
    t.yi = yi;
    t.type = type;
    STORE(vbase_t, vbase_ptr, num_vbase, max_vbase, t);
    return 0;
}

int Handle_vdecor(int x, int y, int xi, int yi, int type)
{
    vdecor_t	t;

    t.x = x;
    t.y = y;
    t.xi = xi;
    t.yi = yi;
    t.type = type;
    STORE(vdecor_t, vdecor_ptr, num_vdecor, max_vdecor, t);
    return 0;
}

void paintdataCleanup(void)
{
    int i;

    for (i = 0; i < MAX_COLORS; i++) {
	if (max_rect[i] > 0 && rect_ptr[i]) {
	    max_rect[i] = 0;
	    free(rect_ptr[i]);
	}
	if (max_arc[i] > 0 && arc_ptr[i]) {
	    max_arc[i] = 0;
	    free(arc_ptr[i]);
	}
	if (max_seg[i] > 0 && seg_ptr[i]) {
	    max_seg[i] = 0;
	    free(seg_ptr[i]);
	}
    }
    if (max_refuel > 0 && refuel_ptr) {
	max_refuel = 0;
	free(refuel_ptr);
	refuel_ptr = 0;
    }
    if (max_connector > 0 && connector_ptr) {
	max_connector = 0;
	free(connector_ptr);
	connector_ptr = 0;
    }
    if (max_laser > 0 && laser_ptr) {
	max_laser = 0;
	free(laser_ptr);
	laser_ptr = 0;
    }
    if (max_missile > 0 && missile_ptr) {
	max_missile = 0;
	free(missile_ptr);
	missile_ptr = 0;
    }
    if (max_ball > 0 && ball_ptr) {
	max_ball = 0;
	free(ball_ptr);
	ball_ptr = 0;
    }
    if (max_ship > 0 && ship_ptr) {
	max_ship = 0;
	free(ship_ptr);
	ship_ptr = 0;
    }
    if (max_mine > 0 && mine_ptr) {
	max_mine = 0;
	free(mine_ptr);
	mine_ptr = 0;
    }
    if (max_ecm > 0 && ecm_ptr) {
	max_ecm = 0;
	free(ecm_ptr);
	ecm_ptr = 0;
    }
    if (max_trans > 0 && trans_ptr) {
	max_trans = 0;
	free(trans_ptr);
	trans_ptr = 0;
    }
    if (max_paused > 0 && paused_ptr) {
	max_paused = 0;
	free(paused_ptr);
	paused_ptr = 0;
    }
    if (max_radar > 0 && radar_ptr) {
	max_radar = 0;
	free(radar_ptr);
	radar_ptr = 0;
    }
    if (max_vcannon > 0 && vcannon_ptr) {
	max_vcannon = 0;
	free(vcannon_ptr);
	vcannon_ptr = 0;
    }
    if (max_vfuel > 0 && vfuel_ptr) {
	max_vfuel = 0;
	free(vfuel_ptr);
	vfuel_ptr = 0;
    }
    if (max_vbase > 0 && vbase_ptr) {
	max_vbase = 0;
	free(vbase_ptr);
	vbase_ptr = 0;
    }
    if (max_vdecor > 0 && vdecor_ptr) {
	max_vdecor = 0;
	free(vdecor_ptr);
	vdecor_ptr = 0;
    }
    if (max_itemtype > 0 && itemtype_ptr) {
	max_itemtype = 0;
	free(itemtype_ptr);
	itemtype_ptr = 0;
    }
    if (max_wreckage > 0 && wreckage_ptr) {
	max_wreckage = 0;
	free(wreckage_ptr);
	wreckage_ptr = 0;
    }
    if (max_asteroids > 0 && asteroid_ptr) {
	max_asteroids = 0;
	free(asteroid_ptr);
	asteroid_ptr = 0;
    }
    if (max_wormholes > 0 && wormhole_ptr) {
	max_wormholes = 0;
	free(wormhole_ptr);
	wormhole_ptr = 0;
    }
}

#ifdef	WINDOWSCALING
#define SCALE_ARRAY_SIZE	32768
short	scaleArray[SCALE_ARRAY_SIZE];

void Init_scale_array(void)
{
    int		i, start, end, n;
    double	scaleMultFactor;

    if (scaleFactor == 0.0)
	scaleFactor = 1.0;
    if (scaleFactor < 0.1)
	scaleFactor = 0.1;
    if (scaleFactor > 10.0)
	scaleFactor = 10.0;
    scaleMultFactor = 1.0 / scaleFactor;

    scaleArray[0] = 0;

    for (i = 1; i < NELEM(scaleArray); i++) {
	n = (int)floor(i * scaleMultFactor + 0.5);
	if (n == 0) {
	    /* keep values for non-zero indices at least 1. */
	    scaleArray[i] = 1;
	} else {
	    break;
	}
    }
    start = i;

    for (i = NELEM(scaleArray) - 1; i >= 0; i--) {
	n = (int)floor(i * scaleMultFactor + 0.5);
	if (n > 32767) {
	    /* keep values lower or equal to max short. */
	    scaleArray[i] = 32767;
	} else {
	    break;
	}
    }
    end = i;

    for (i = start; i <= end; i++) {
	scaleArray[i] = (int)floor(i * scaleMultFactor + 0.5);
    }

    /* verify correct calculations, because of reported gcc optimization bugs. */
    for (i = 1; i < NELEM(scaleArray); i++) {
	if (scaleArray[i] < 1) {
	    break;
	}
    }

    if (i != SCALE_ARRAY_SIZE) {
	fprintf(stderr,
		"Error: Illegal value %d in scaleArray[%d].\n"
		"\tThis error may be due to a bug in your compiler.\n"
		"\tEither try a lower optimization level,\n"
		"\tor a different compiler version or vendor.\n",
		scaleArray[i], i);
	exit(1);
    }
}
#endif
