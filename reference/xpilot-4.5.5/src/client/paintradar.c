/* $Id: paintradar.c,v 5.1 2001/05/25 00:22:50 bertg Exp $
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
#include <string.h>
#include <stdio.h>
#include <errno.h>

#ifndef _WINDOWS
# include <unistd.h>
# include <X11/Xlib.h>
# include <X11/Xos.h>
#endif

#ifdef _WINDOWS
# include "NT/winX.h"
# include "NT/winXXPilot.h"
#endif

#include "version.h"
#include "config.h"
#include "const.h"
#include "error.h"
#include "bit.h"
#include "keys.h"
#include "rules.h"
#include "setup.h"
#include "paint.h"
#include "paintdata.h"
#include "xinit.h"

char paintradar_version[] = VERSION;

extern DFLOAT		tbl_sin[];
extern DFLOAT		tbl_cos[];
extern setup_t		*Setup;
extern int		RadarHeight;

Window	radar;			/* Radar window */
Pixmap	p_radar, s_radar;	/* Pixmaps for the radar (implements */
				/* the planes hack on the radar for */
				/* monochromes) */
long	dpl_1[2], dpl_2[2];	/* Used by radar hack */
int	wallRadarColor;		/* Color index for walls on radar. */
int	targetRadarColor;	/* Color index for targets on radar. */
int	decorRadarColor;	/* Color index for decorations on radar. */
int	radar_exposures;
int	(*radarDrawRectanglePtr)	/* Function to draw player on radar */
	(Display *disp, Drawable d, GC gc,
	 int x, int y, unsigned width, unsigned height);


static int	slidingradar_x;	/* sliding radar offsets for windows */
static int	slidingradar_y;


static void Copy_static_radar(void)
{
#ifndef _WINDOWS
    if (s_radar != p_radar) {
	/* Draw static radar onto radar */
	XCopyArea(dpy, s_radar, p_radar, gc,
		  0, 0, 256, RadarHeight, 0, 0);
    } else {
	/* Clear radar */
	XSetForeground(dpy, radarGC, colors[BLACK].pixel);
	XFillRectangle(dpy, p_radar,
		       radarGC, 0, 0, 256, RadarHeight);
    }
#else
    WinXBltPixToWin(s_radar, radar, 0, 0, 256, RadarHeight, 0, 0);
    p_radar = radar;
#endif
    XSetForeground(dpy, radarGC, colors[WHITE].pixel);
}


#ifdef _WINDOWS
static void Windows_copy_sliding_radar(float xf, float yf)
{
    slidingradar_x = (int)((pos.x * xf + 0.5) + 128) % 256;
    slidingradar_y = (RadarHeight - (int)(pos.y * yf + 0.5) - 1 + RadarHeight/2)
		    % RadarHeight;

    /*
     * Draw slidingradar in four chunks onto the screen.
     */
    WinXBltPixToWin(s_radar, radar,
		    slidingradar_x , slidingradar_y,
		    256-slidingradar_x, RadarHeight-slidingradar_y,
		    0, 0);
    WinXBltPixToWin(s_radar, radar,
		    0, slidingradar_y,
		    slidingradar_x, RadarHeight-slidingradar_y,
		    256-slidingradar_x, 0);
    WinXBltPixToWin(s_radar, radar,
		    slidingradar_x, 1,
		    256-slidingradar_x, slidingradar_y
		    , 0,
		    RadarHeight-slidingradar_y);
    WinXBltPixToWin(s_radar, radar,
		    0, 1,
		    slidingradar_x, slidingradar_y,
		    256-slidingradar_x, RadarHeight-slidingradar_y);
    p_radar = radar;

    XSetForeground(dpy, radarGC, colors[WHITE].pixel);
}
#endif


static void Paint_checkpoint_radar(float xf, float yf)
{
    int			x, y;
    XPoint		points[5];

    if (BIT(Setup->mode, TIMING)) {
	Check_pos_by_index(nextCheckPoint, &x, &y);
	x = ((int)(x * BLOCK_SZ * xf + 0.5) ) - slidingradar_x;
	y = (RadarHeight - (int)(y * BLOCK_SZ * yf + 0.5) + DSIZE - 1) - slidingradar_y;
	if (x <= 0) {
	    x += 256;
	}
	if (y <= 0) {
	    y += RadarHeight;
	}
	/* top */
	points[0].x = x ;
	points[0].y = y ;
	/* right */
	points[1].x = x + DSIZE ;
	points[1].y = y - DSIZE ;
	/* bottom */
	points[2].x = x ;
	points[2].y = y - 2*DSIZE ;
	/* left */
	points[3].x = x - DSIZE ;
	points[3].y = y - DSIZE ;
	/* top */
	points[4].x = x ;
	points[4].y = y ;
	XDrawLines(dpy, p_radar, radarGC,
		   points, 5, 0);
    }
}

static void Paint_self_radar(float xf, float yf)
{
    int		x, y, x1, y1, xw, yw;

    if (selfVisible != 0 && loops % 16 < 13) {
	x = (int)(pos.x * xf + 0.5) - slidingradar_x;
	y = RadarHeight - (int)(pos.y * yf + 0.5) - 1 - slidingradar_y;
	if (x <= 0) {
	    x += 256;
	}
	if (y <= 0) {
	    y += RadarHeight;
	}

	x1 = (int)(x + 8 * tcos(heading));
	y1 = (int)(y - 8 * tsin(heading));
	XDrawLine(dpy, p_radar, radarGC,
		  x, y, x1, y1);
	if (BIT(Setup->mode, WRAP_PLAY)) {
	    xw = x1 - (x1 + 256) % 256;
	    yw = y1 - (y1 + RadarHeight) % RadarHeight;
	    if (xw != 0) {
		XDrawLine(dpy, p_radar, radarGC,
			  x - xw, y, x1 - xw, y1);
	    }
	    if (yw != 0) {
		XDrawLine(dpy, p_radar, radarGC,
			  x, y - yw, x1, y1 - yw);
		if (xw != 0) {
		    XDrawLine(dpy, p_radar, radarGC,
			      x - xw, y - yw, x1 - xw, y1 - yw);
		}
	    }
	}
    }
}

static void Paint_objects_radar(void)
{
    int			i, x, y, xw, yw;

    for (i = 0; i < num_radar; i++) {
	int s = radar_ptr[i].size;

	/* draw players from the same team in a different color. */
	if ((s & 0x80) != 0) {
	    if (maxColors > 4) {
		XSetForeground(dpy, radarGC, colors[4].pixel);
	    }
	    s &= ~0x80;
	}

	if (s <= 0) {
	    s = 1;
	}
	x = radar_ptr[i].x - s / 2 - slidingradar_x;
	y = RadarHeight - radar_ptr[i].y - 1 - s / 2 - slidingradar_y;
	if (x <= 0) {
	    x += 256;
	}
	if (y <= 0) {
	    y += RadarHeight;
	}

	(*radarDrawRectanglePtr)(dpy, p_radar, radarGC, x, y, s, s);
	if (BIT(Setup->mode, WRAP_PLAY)) {
	    xw = (x < 0) ? -256 : (x + s >= 256) ? 256 : 0;
	    yw = (y < 0) ? -RadarHeight
			     : (y + s >= RadarHeight) ? RadarHeight : 0;
	    if (xw != 0) {
		(*radarDrawRectanglePtr)(dpy, p_radar, radarGC,
					 x - xw, y, s, s);
	    }
	    if (yw != 0) {
		(*radarDrawRectanglePtr)(dpy, p_radar, radarGC,
					 x, y - yw, s, s);

		if (xw != 0) {
		    (*radarDrawRectanglePtr)(dpy, p_radar, radarGC,
					     x - xw, y - yw, s, s);
		}
	    }
	}
	XSetForeground(dpy, radarGC, colors[WHITE].pixel);
    }
    if (num_radar) {
	RELEASE(radar_ptr, num_radar, max_radar);
    }
}


void Paint_radar(void)
{
    const float		xf = 256.0f / (float)Setup->width,
			yf = (float)RadarHeight / (float)Setup->height;

    if (radar_exposures == 0) {
	return;
    }

    slidingradar_x = 0;
    slidingradar_y = 0;

#ifdef _WINDOWS
    if (BIT(instruments, SHOW_SLIDING_RADAR) != 0) {
	/*
	 * Hack to fix slidingradar in windows.
	 */
	Windows_copy_sliding_radar(xf, yf);
    }
    else
    {
	Copy_static_radar();
    }
#else
    Copy_static_radar();
#endif

    /* Checkpoints */
    Paint_checkpoint_radar(xf, yf);

    Paint_self_radar(xf, yf);
    Paint_objects_radar();
}


void Paint_sliding_radar(void)
{
    if (BIT(Setup->mode, WRAP_PLAY) == 0) {
	return;
    }
    if (p_radar != s_radar) {
	return;
    }
    if (BIT(instruments, SHOW_SLIDING_RADAR) != 0) {
	if (s_radar != radar) {
	    return;
	}
	s_radar = XCreatePixmap(dpy, radar,
				256, RadarHeight,
				dispDepth);
	p_radar = s_radar;
	if (radar_exposures > 0) {
	    Paint_world_radar();
	}
    } else {
	if (s_radar == radar) {
	    return;
	}
	XFreePixmap(dpy, s_radar);
	s_radar = radar;
	p_radar = radar;
	if (radar_exposures > 0) {
	    Paint_world_radar();
	}
    }
}


void Paint_world_radar(void)
{
    int			i, xi, yi, xm, ym, xp, yp = 0;
    int			xmoff, xioff;
    int			type, vis, damage;
    float		xs, ys;
    int			npoint = 0, nsegment = 0;
    int			start, end;
    int			currColor, visibleColorChange;
    const int		max = 256;
    u_byte		visible[256];
    u_byte		visibleColor[256];
    XSegment		segments[256];
    XPoint		points[256];

    radar_exposures = 2;

#ifdef _WINDOWS
    XSetForeground(dpy, s_radar, colors[BLACK].pixel);
    XFillRectangle(dpy, s_radar, radarGC, 0, 0, 256, RadarHeight);
#else
    if (s_radar == p_radar) {
	XSetPlaneMask(dpy, radarGC,
		      AllPlanes & ~(dpl_1[0] | dpl_1[1]));
    }
    if (s_radar != radar) {
	/* Clear radar */
	XSetForeground(dpy, radarGC, colors[BLACK].pixel);
	XFillRectangle(dpy, s_radar, radarGC, 0, 0, 256, RadarHeight);
    } else {
	XClearWindow(dpy, radar);
    }
#endif

    /*
     * Calculate an array which is later going to be indexed
     * by map block type.  The indexing should return a true
     * value when the map block should be visible on the radar
     * and a false value otherwise.
     */
    memset(visible, 0, sizeof visible);
    visible[SETUP_FILLED] = 1;
    visible[SETUP_FILLED_NO_DRAW] = 1;
    visible[SETUP_REC_LU] = 1;
    visible[SETUP_REC_RU] = 1;
    visible[SETUP_REC_LD] = 1;
    visible[SETUP_REC_RD] = 1;
    visible[SETUP_FUEL] = 1;
    for (i = 0; i < 10; i++) {
	visible[SETUP_TARGET+i] = 1;
    }
    for (i = BLUE_BIT; i < sizeof visible; i++) {
	visible[i] = 1;
    }
    if (BIT(instruments, SHOW_DECOR)) {
	visible[SETUP_DECOR_FILLED] = 1;
	visible[SETUP_DECOR_LU] = 1;
	visible[SETUP_DECOR_RU] = 1;
	visible[SETUP_DECOR_LD] = 1;
	visible[SETUP_DECOR_RD] = 1;
    }

    /*
     * Calculate an array which returns the color to use
     * for drawing when indexed with a map block type.
     */
    memset(visibleColor, 0, sizeof visibleColor);
    visibleColor[SETUP_FILLED] =
	visibleColor[SETUP_FILLED_NO_DRAW] =
	visibleColor[SETUP_REC_LU] =
	visibleColor[SETUP_REC_RU] =
	visibleColor[SETUP_REC_LD] =
	visibleColor[SETUP_REC_RD] =
	visibleColor[SETUP_FUEL] = wallRadarColor;
    for (i = 0; i < 10; i++) {
	visibleColor[SETUP_TARGET+i] = targetRadarColor;
    }
    for (i = BLUE_BIT; i < sizeof visible; i++) {
	visibleColor[i] = wallRadarColor;
    }
    if (BIT(instruments, SHOW_DECOR)) {
	visibleColor[SETUP_DECOR_FILLED] =
	    visibleColor[SETUP_DECOR_LU] =
	    visibleColor[SETUP_DECOR_RU] =
	    visibleColor[SETUP_DECOR_LD] =
	    visibleColor[SETUP_DECOR_RD] = decorRadarColor;
    }

    /* The following code draws the map on the radar.  Segments and
     * points arrays are use to build lists of things to be drawn.
     * Normally the segments and points are drawn when the arrays are
     * full, but now they are also drawn when the color changes.  The
     * visibleColor array is used to determine the color to be used
     * for the given visible block type.
     *
     * Another (and probably better) way to do this would be use
     * different segments and points arrays for each visible color.
     */
    if (Setup->x >= 256) {
	xs = (float)(256 - 1) / (Setup->x - 1);
	ys = (float)(RadarHeight - 1) / (Setup->y - 1);
	currColor = -1;
	for (xi = 0; xi < Setup->x; xi++) {
	    start = end = -1;
	    xp = (int)(xi * xs + 0.5);
	    xioff = xi * Setup->y;
	    for (yi = 0; yi < Setup->y; yi++) {
		visibleColorChange = 0;
		type = Setup->map_data[xioff + yi];
		if (type >= SETUP_TARGET && type < SETUP_TARGET + 10) {
		    vis = (Target_alive(xi, yi, &damage) == 0);
		}
		else {
		    vis = visible[type];
		}
		if (vis) {
		    yp = (int)(yi * ys + 0.5);
		    if (start == -1) {
			if ((nsegment > 0 || npoint > 0)
			    && currColor != visibleColor[type]) {
			    if (nsegment > 0) {
				XDrawSegments(dpy, s_radar, radarGC,
					      segments, nsegment);
				nsegment = 0;
			    }
			    if (npoint > 0) {
				XDrawPoints(dpy, s_radar, radarGC,
					    points, npoint, CoordModeOrigin);
				npoint = 0;
			    }
			}
			start = end = yp;
			currColor = visibleColor[type];
#ifdef _WINDOWS
			XSetForeground(dpy, s_radar, currColor);
#else
			XSetForeground(dpy, radarGC, colors[currColor].pixel);
#endif
		    } else {
			end = yp;
			visibleColorChange = (visibleColor[type] != currColor);
		    }
		}

		if (start != -1
		    && (!vis || yi == Setup->y - 1 || visibleColorChange)) {
		    if (end > start) {
			segments[nsegment].x1 = xp;
			segments[nsegment].y1 = RadarHeight - 1 - start;
			segments[nsegment].x2 = xp;
			segments[nsegment].y2 = RadarHeight - 1 - end;
			nsegment++;
			if (nsegment >= max || yi == Setup->y - 1) {
			    XDrawSegments(dpy, s_radar, radarGC,
					  segments, nsegment);
			    nsegment = 0;
			}
		    } else {
			points[npoint].x = xp;
			points[npoint].y = RadarHeight - 1 - start;
			npoint++;
			if (npoint >= max || yi == Setup->y - 1) {
			    XDrawPoints(dpy, s_radar, radarGC,
					points, npoint, CoordModeOrigin);
			    npoint = 0;
			}
		    }
		    start = end = -1;
		}

		if (visibleColorChange) {
		    if (nsegment > 0) {
			XDrawSegments(dpy, s_radar, radarGC,
				      segments, nsegment);
			nsegment = 0;
		    }
		    if (npoint > 0) {
			XDrawPoints(dpy, s_radar, radarGC,
				    points, npoint, CoordModeOrigin);
			npoint = 0;
		    }
		    start = end = yp;
		    currColor = visibleColor[type];
		    XSetForeground(dpy, radarGC, colors[currColor].pixel);
		}
	    }
	}
    } else {
	xs = (float)(Setup->x - 1) / (256 - 1);
	ys = (float)(Setup->y - 1) / (RadarHeight - 1);
	currColor = -1;
	for (xi = 0; xi < 256; xi++) {
	    xm = (int)(xi * xs + 0.5);
	    xmoff = xm * Setup->y;
	    start = end = -1;
	    xp = xi;
	    for (yi = 0; yi < RadarHeight; yi++) {
		visibleColorChange = 0;
		ym = (int)(yi * ys + 0.5);
		type = Setup->map_data[xmoff + ym];
		vis = visible[type];
		if (type >= SETUP_TARGET && type < SETUP_TARGET + 10) {
		    vis = (Target_alive(xm, ym, &damage) == 0);
		}
		if (vis) {
		    yp = yi;
		    if (start == -1) {
			if ((nsegment > 0 || npoint > 0)
			    && currColor != visibleColor[type]) {
			    if (nsegment > 0) {
				XDrawSegments(dpy, s_radar, radarGC,
					      segments, nsegment);
				nsegment = 0;
			    }
			    if (npoint > 0) {
				XDrawPoints(dpy, s_radar, radarGC,
					    points, npoint, CoordModeOrigin);
				npoint = 0;
			    }
			}
			start = end = yp;
			currColor = visibleColor[type];
#ifdef _WINDOWS
			XSetForeground(dpy, s_radar, currColor);
#else
			XSetForeground(dpy, radarGC, colors[currColor].pixel);
#endif
		    } else {
			end = yp;
			visibleColorChange = visibleColor[type] != currColor;
		    }
		}

		if (start != -1
		    && (!vis || yi == RadarHeight - 1 || visibleColorChange)) {
		    if (end > start) {
			segments[nsegment].x1 = xp;
			segments[nsegment].y1 = RadarHeight - 1 - start;
			segments[nsegment].x2 = xp;
			segments[nsegment].y2 = RadarHeight - 1 - end;
			nsegment++;
			if (nsegment >= max || yi == RadarHeight - 1) {
			    XDrawSegments(dpy, s_radar, radarGC,
					  segments, nsegment);
			    nsegment = 0;
			}
		    } else {
			points[npoint].x = xp;
			points[npoint].y = RadarHeight - 1 - start;
			npoint++;
			if (npoint >= max || yi == RadarHeight - 1) {
			    XDrawPoints(dpy, s_radar, radarGC,
					points, npoint, CoordModeOrigin);
			    npoint = 0;
			}
		    }
		    start = end = -1;
		}

		if (visibleColorChange) {
		    if (nsegment > 0) {
			XDrawSegments(dpy, s_radar, radarGC,
				      segments, nsegment);
			nsegment = 0;
		    }
		    if (npoint > 0) {
			XDrawPoints(dpy, s_radar, radarGC,
				    points, npoint, CoordModeOrigin);
			npoint = 0;
		    }
		    start = end = yp;
		    currColor = visibleColor[type];
#ifdef _WINDOWS
		    XSetForeground(dpy, s_radar, currColor);
#else
		    XSetForeground(dpy, radarGC, colors[currColor].pixel);
#endif
		}
	    }
	}
    }
    if (nsegment > 0) {
	XDrawSegments(dpy, s_radar, radarGC,
		      segments, nsegment);
    }
    if (npoint > 0) {
	XDrawPoints(dpy, s_radar, radarGC,
		    points, npoint, CoordModeOrigin);
    }

    if (s_radar == p_radar) {
	XSetPlaneMask(dpy, radarGC,
		      AllPlanes & ~(dpl_2[0] | dpl_2[1]));
    }

    for (i = 0;; i++) {
	int dead_time, damage;
	if (Target_by_index(i, &xi, &yi, &dead_time, &damage) == -1) {
	    break;
	}
	if (dead_time) {
	    continue;
	}
	Paint_radar_block(xi, yi, targetRadarColor);
    }
}


/*
 * Try and draw an area of the radar which represents block position
 * `xi' `yi'.  If `draw' is zero the area is cleared.
 */
void Paint_radar_block(int xi, int yi, int color)
{
    float	xs, ys;
    int		xp, yp, xw, yw;

    if (s_radar == p_radar) {
	XSetPlaneMask(dpy, radarGC, AllPlanes & ~(dpl_1[0] | dpl_1[1]));
    }
    XSetForeground(dpy, radarGC, colors[color].pixel);

    if (Setup->x >= 256) {
	xs = (float)(256 - 1) / (Setup->x - 1);
	ys = (float)(RadarHeight - 1) / (Setup->y - 1);
	xp = (int)(xi * xs + 0.5);
	yp = RadarHeight - 1 - (int)(yi * ys + 0.5);
	XDrawPoint(dpy, s_radar, radarGC, xp, yp);
    } else {
	xs = (float)(Setup->x - 1) / (256 - 1);
	ys = (float)(Setup->y - 1) / (RadarHeight - 1);
	/*
	 * Calculate the min and max points on the radar that would show
	 * block position `xi' and `yi'.  Note `xp' is the minimum x coord
	 * for `xi',which is one more than the previous xi value would give,
	 * and `xw' is the maximum, which is then changed to a width value.
	 * Similarly for `yw' and `yp' (the roles are reversed because the
	 * radar is upside down).
	 */
	xp = (int)((xi - 0.5) / xs) + 1;
	xw = (int)((xi + 0.5) / xs);
	yw = (int)((yi - 0.5) / ys) + 1;
	yp = (int)((yi + 0.5) / ys);
	xw -= xp;
	yw = yp - yw;
	yp = RadarHeight - 1 - yp;
	XFillRectangle(dpy, s_radar, radarGC, xp, yp, xw+1, yw+1);
    }
    if (s_radar == p_radar) {
	XSetPlaneMask(dpy, radarGC,
		      AllPlanes & ~(dpl_2[0] | dpl_2[1]));
    }
}

