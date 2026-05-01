/* $Id: paint.c,v 5.7 2002/01/30 21:29:39 bertg Exp $
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
#include <limits.h>
#include <time.h>
#include <sys/types.h>

#ifndef _WINDOWS
# include <unistd.h>
# include <X11/Xlib.h>
# include <X11/Xos.h>
#endif

#ifdef _WINDOWS
# include "NT/winX.h"
# include "NT/winClient.h"
# include "NT/winXXPilot.h"
# include "netclient.h"
#endif

#include "version.h"
#include "config.h"
#include "const.h"
#include "error.h"
#include "bit.h"
#include "types.h"
#include "keys.h"
#include "rules.h"
#include "setup.h"
#include "dbuff.h"
#include "texture.h"
#include "paint.h"
#include "paintdata.h"
#include "record.h"
#include "xinit.h"
#include "blockbitmaps.h"
#include "portability.h"
#include "client.h"
#include "commonproto.h"

char paint_version[] = VERSION;

extern setup_t		*Setup;
extern int		RadarHeight;

/*
 * Globals.
 */
XFontStruct* gameFont;		/* The fonts used in the game */
XFontStruct* messageFont;
XFontStruct* scoreListFont;
XFontStruct* buttonFont;
XFontStruct* textFont;
XFontStruct* talkFont;
XFontStruct* motdFont;
char	gameFontName[FONT_LEN];	/* The fonts used in the game */
char	messageFontName[FONT_LEN];
char	scoreListFontName[FONT_LEN];
char	buttonFontName[FONT_LEN];
char	textFontName[FONT_LEN];
char	talkFontName[FONT_LEN];
char	motdFontName[FONT_LEN];

Display	*dpy;			/* Display of player (pointer) */
Display	*kdpy;			/* Keyboard display */
short	about_page;		/* Which page is the player on? */
unsigned short	team;		/* What team is the player on? */

GC	gc;			/* GC for the game area */
GC	messageGC;		/* GC for messages in the game area */
GC	radarGC;		/* GC for the radar */
GC	buttonGC;		/* GC for the buttons */
GC	scoreListGC;		/* GC for the player list */
GC	textGC;			/* GC for the info text */
GC	talkGC;			/* GC for the message window */
GC	motdGC;			/* GC for the motd text */
XGCValues	gcv;

Window	top;			/* Top-level window (topshell) */
Window	draw;			/* Main play window */
Window	keyboard;		/* Keyboard window */
#ifdef _WINDOWS		/* Windows needs some dummy windows (size 0,0) */
				/* so we can store the active fonts.  Windows only */
				/* supports 1 active font per window */
Window	textWindow;		/* for the GC into the config window */
Window	msgWindow;		/* for meesages into the playfield */
Window	buttonWindow;		/* to calculate size of buttons */
#endif

Pixmap	p_draw;			/* Saved pixmap for the drawing */
				/* area (monochromes use this) */
Window	players;		/* Player list window */
				/* monochromes) */
int	maxMessages;		/* Max. number of messages to display */
int	messagesToStdout;	/* Send messages to standard output */
Window	about_w;		/* About window */
Window	about_close_b;		/* About window's close button */
Window	about_next_b;		/* About window's next button */
Window	about_prev_b;		/* About window's previous button */
Window	keys_close_b;		/* Help window's close button */
Window	talk_w;			/* Talk window */
XColor	colors[MAX_COLORS];	/* Colors */
Colormap	colormap;	/* Private colormap */
int	maxColors;		/* Max. number of colors to use */
bool	gotFocus;
bool	players_exposed;
short	ext_view_width;		/* Width of extended visible area */
short	ext_view_height;	/* Height of extended visible area */
int	active_view_width;	/* Width of active map area displayed. */
int	active_view_height;	/* Height of active map area displayed. */
int	ext_view_x_offset;	/* Offset ext_view_width */
int	ext_view_y_offset;	/* Offset ext_view_height */

int	titleFlip;		/* Do special title bar flipping? */
int	shieldDrawMode = -1;	/* Either LineOnOffDash or LineSolid */
char	modBankStr[NUM_MODBANKS][MAX_CHARS];	/* modifier banks */
char	*texturePath = NULL;		/* Path list of texture directories */
bool	useErase;		/* use Erase hack for slow X */

int		maxKeyDefs;
keydefs_t	*keyDefs = NULL;

other_t     *self;          /* player info */

long        loops = 0;

int	cacheShips = 0;		/* cache some ship bitmaps every frame */

static void Paint_clock(int redraw);

void Game_over_action(u_byte stat)
{
    static u_byte old_stat = 0;

    if (BIT(old_stat, GAME_OVER) && !BIT(stat, GAME_OVER)
	&& !BIT(stat,PAUSE)) {
	XMapRaised(dpy, top);
    }
    /* GAME_OVER -> PLAYING */
    if (BIT(old_stat, PLAYING|PAUSE|GAME_OVER) != PLAYING) {
	if (BIT(stat, PLAYING|PAUSE|GAME_OVER) == PLAYING) {
	    Reset_shields();
	}
    }

    old_stat = stat;
}

void Paint_frame(void)
{
    static long		scroll_i = 0;
    static int		prev_damaged = 0;
    static int		prev_prev_damaged = 0;

#ifdef _WINDOWS	/* give any outgoing data a head start to the server */
    Net_flush();	/* send anything to the server before returning to Windows */
#endif

    if (start_loops != end_loops) {
	errno = 0;
	error("Start neq. End (%ld,%ld,%ld)", start_loops, end_loops, loops);
    }
    loops = end_loops;

    /*
     * Switch between two different window titles.
     */
    if (titleFlip && (loops % TITLE_DELAY) == 0) {
	scroll_i = !scroll_i;
	if (scroll_i)
	    XStoreName(dpy, top, COPYRIGHT);
	else
	    XStoreName(dpy, top, TITLE);

    }
    /* This seems to have a bug (in Windows) 'cause last frame we ended
       with an XSetForeground(white) confusing SET_FG */
    SET_FG(colors[BLACK].pixel);

#ifdef _WINDOWS
    p_draw = draw;		/* let's try this */
    XSetForeground(dpy, gc, colors[BLACK].pixel);
    XFillRectangle(dpy, p_draw, gc, 0, 0, draw_width, draw_height);
#endif

    rd.newFrame();


    /*
     * Do we really need to draw all this if the player is damaged?
     */
    if (damaged <= 0) {
	if (prev_damaged || prev_prev_damaged) {
	    /* clean up ecm damage */
	    SET_FG(colors[BLACK].pixel);
	    XFillRectangle(dpy, draw, gc, 0, 0, draw_width, draw_height);
	}

	Erase_start();

	Arc_start();

	Rectangle_start();
	Segment_start();

	Paint_world();

	Segment_end();
	Rectangle_end();

	Rectangle_start();
	Segment_start();

	Paint_vfuel();
	Paint_vdecor();
	Paint_vcannon();
	Paint_vbase();
	Paint_shots();

	Rectangle_end();
	Segment_end();

	Rectangle_start();
	Segment_start();

	Paint_ships();
	Paint_meters();
	Paint_HUD();
	Paint_recording();

	Rectangle_end();
	Segment_end();

	Arc_end();

	Paint_messages();
	Paint_radar();
	Paint_score_objects();
    }
    else {
	/* Damaged. */

	XSetFunction(dpy, gc, GXxor);
	SET_FG(colors[BLACK].pixel ^ colors[BLUE].pixel);
	XFillRectangle(dpy, draw, gc, 0, 0, draw_width, draw_height);
	XSetFunction(dpy, gc, GXcopy);
	SET_FG(colors[BLACK].pixel);
    }
    if (cacheShips && blockBitmaps) {
	Cache_ships(draw);
    }
    prev_prev_damaged = prev_damaged;
    prev_damaged = damaged;

    rd.endFrame();

    if (radar_exposures == 1) {
	Paint_world_radar();
    }

    /*
     * Now switch planes and clear the screen.
     */
    if (p_radar != radar && radar_exposures > 0) {
	if (BIT(instruments, SHOW_SLIDING_RADAR) == 0
	    || BIT(Setup->mode, WRAP_PLAY) == 0) {
#ifndef _WINDOWS
	    XCopyArea(dpy, p_radar, radar, gc,
		      0, 0, 256, RadarHeight, 0, 0);
#else
	    WinXBltPixToWin(p_radar, radar, 
			    0, 0, 256, RadarHeight, 0, 0);
#endif
	} else {

	    int x, y, w, h;
	    float xp, yp, xo, yo;

	    xp = (float) (pos.x * 256) / Setup->width;
	    yp = (float) (pos.y * RadarHeight) / Setup->height;
	    xo = (float) 256 / 2;
	    yo = (float) RadarHeight / 2;
	    if (xo <= xp) {
		x = (int) (xp - xo + 0.5);
	    } else {
		x = (int) (256 + xp - xo + 0.5);
	    }
	    if (yo <= yp) {
		y = (int) (yp - yo + 0.5);
	    } else {
		y = (int) (RadarHeight + yp - yo + 0.5);
	    }
	    y = RadarHeight - y - 1;
	    w = 256 - x;
	    h = RadarHeight - y;

#ifndef _WINDOWS	
	    XCopyArea(dpy, p_radar, radar, gc,
		      0, 0, x, y, w, h);
	    XCopyArea(dpy, p_radar, radar, gc,
		      x, 0, w, y, 0, h);
	    XCopyArea(dpy, p_radar, radar, gc,
		      0, y, x, h, w, 0);
	    XCopyArea(dpy, p_radar, radar, gc,
		      x, y, w, h, 0, 0);
#else
	    Paint_world_radar();			  
#endif
	}
    }
    else if (radar_exposures > 2) {
	Paint_world_radar();
    }

#ifndef _WINDOWS
    if (dbuf_state->type == PIXMAP_COPY) {
	XCopyArea(dpy, p_draw, draw, gc,
		  0, 0, ext_view_width, ext_view_height, 0, 0);
    }

    dbuff_switch(dbuf_state);

    if (dbuf_state->type == COLOR_SWITCH) {
	XSetPlaneMask(dpy, gc, dbuf_state->drawing_planes);
	XSetPlaneMask(dpy, messageGC, dbuf_state->drawing_planes);
    }
#endif

    if (!damaged) {
	/* Prepare invisible buffer for next frame by clearing. */
	if (useErase) {
	    Erase_end();
	}
	else {
	    /*
	     * DBE's XdbeBackground switch option is
	     * probably faster than XFillRectangle.
	     */
#ifndef _WINDOWS
	    if (dbuf_state->multibuffer_type != MULTIBUFFER_DBE) {
		SET_FG(colors[BLACK].pixel);
		XFillRectangle(dpy, p_draw, gc, 0, 0, draw_width, draw_height);
	    }
#endif
	}
    }

#ifndef _WINDOWS
    if (talk_mapped == true) {
	static bool toggle;
	static long last_toggled;

	if (loops >= last_toggled + FPS / 2 || loops < last_toggled) {
	    toggle = (toggle == false) ? true : false;
	    last_toggled = loops;
	}
	Talk_cursor(toggle);
    }
#endif

#ifdef _WINDOWS
    Client_score_table();
    PaintWinClient();
#endif

    Paint_clock(0);

    XFlush(dpy);
}


#define SCORE_BORDER		6


static void Paint_score_background(int thisLine)
{
    if (!blockBitmaps) {
	XClearWindow(dpy, players);
    } else {
	XSetForeground(dpy, scoreListGC, colors[BLACK].pixel);

	IFWINDOWS( XFillRectangle(dpy, players, scoreListGC, 
				  0, 0, 
				  players_width, BG_IMAGE_HEIGHT); )

	PaintBitmap(players, BM_SCORE_BG,
		    0, 0,
		    players_width, BG_IMAGE_HEIGHT,
		    0);

	if (players_height > BG_IMAGE_HEIGHT + LOGO_HEIGHT) {
	    XFillRectangle(dpy, players, scoreListGC, 
			   0, BG_IMAGE_HEIGHT, 
			   players_width,
			   players_height - (BG_IMAGE_HEIGHT + LOGO_HEIGHT));
	}
	PaintBitmap(players, BM_LOGO, 
		    0, players_height - LOGO_HEIGHT, 
		    players_width, LOGO_HEIGHT,
		    0);

	XFlush(dpy);
    }

}


void Paint_score_start(void)
{
    char	headingStr[MSG_LEN];
    static int thisLine;

    thisLine = SCORE_BORDER + scoreListFont->ascent;

    if (showRealName) {
	strlcpy(headingStr, "NICK=USER@HOST", sizeof(headingStr));
    } else {
	strlcpy(headingStr, "  ", sizeof(headingStr));
	if (BIT(Setup->mode, TIMING)) {
	    if (version >= 0x3261) {
		strcat(headingStr, "LAP ");
	    }
	}
	if (BIT(Setup->mode, TEAM_PLAY)) {
	    strlcpy(headingStr, " TM ", sizeof(headingStr));
	} else {
	    strlcpy(headingStr, " AL ", sizeof(headingStr));
	}
	strcat(headingStr, "  SCORE  ");
	if (BIT(Setup->mode, LIMITED_LIVES)) {
	    strlcat(headingStr, "LIFE", sizeof(headingStr));
	}
	strlcat(headingStr, " NAME", sizeof(headingStr));
    }
    Paint_score_background(thisLine);

    ShadowDrawString(dpy, players, scoreListGC,
		     SCORE_BORDER, thisLine,
		     headingStr,
		     colors[WHITE].pixel,
		     colors[BLACK].pixel);

    gcv.line_style = LineSolid;
    XChangeGC(dpy, scoreListGC, GCLineStyle, &gcv);
    XDrawLine(dpy, players, scoreListGC,
	      SCORE_BORDER, thisLine,
	      players_width - SCORE_BORDER, thisLine);

    gcv.line_style = LineOnOffDash;
    XChangeGC(dpy, scoreListGC, GCLineStyle, &gcv);

    Paint_clock(1);
}


void Paint_score_entry(int entry_num,
		       other_t* other,
		       bool best)
{
    static char		raceStr[8], teamStr[4], lifeStr[8], label[MSG_LEN];
    static int		lineSpacing = -1, firstLine;
    int			thisLine;
    char		scoreStr[16];

    /*
     * First time we're here, set up miscellaneous strings for
     * efficiency and calculate some other constants.
     */
    if (lineSpacing == -1) {
	memset(raceStr, '\0', sizeof raceStr);
	memset(teamStr, '\0', sizeof teamStr);
	memset(lifeStr, '\0', sizeof lifeStr);
	teamStr[1] = ' ';
	raceStr[2] = ' ';

	lineSpacing
	    = scoreListFont->ascent + scoreListFont->descent + 3;
	firstLine
	    = 2*SCORE_BORDER + scoreListFont->ascent + lineSpacing;
    }
    thisLine = firstLine + lineSpacing * entry_num;

    /*
     * Setup the status line
     */
    if (showRealName) {
	sprintf(label, "%s=%s@%s", other->name, other->real, other->host);
    } else {
	other_t*	war = Other_by_id(other->war_id);

	if (BIT(Setup->mode, TIMING)) {
	    raceStr[0] = ' ';
	    raceStr[1] = ' ';
	    if (version >= 0x3261) {
		if ((other->mychar == ' ' || other->mychar == 'R')
		    && other->round + other->check > 0) {
		    if (other->round > 99) {
			sprintf(raceStr, "%3d", other->round);
		    }
		    else {
			sprintf(raceStr, "%d.%c",
				other->round, other->check + 'a');
		    }
		}
	    }
	}
	if (BIT(Setup->mode, TEAM_PLAY)) {
	    teamStr[0] = other->team + '0';
	} else {
	    sprintf(teamStr, "%c", other->alliance);
	}

	if (BIT(Setup->mode, LIMITED_LIVES))
	    sprintf(lifeStr, " %3d", other->life);

	if (showScoreDecimals > 0 && version >= 0x4500) {
	    sprintf(scoreStr, "%*.*f",
		    8 - showScoreDecimals, showScoreDecimals,
		    other->score);
	}
	else {
	    sprintf(scoreStr, "%5d", (int) rint(other->score));
	}
	sprintf(label, "%c %s%s%s%s  %s",
		other->mychar, raceStr, teamStr,
		scoreStr, lifeStr,
		other->name);
	if (war) {
	    if (strlen(label) + strlen(war->name) + 5 < sizeof(label)) {
		sprintf(label + strlen(label), " (%s)", war->name);
	    }
	}
    }

    /*
     * Draw the line
     */
    if ((other->mychar == 'D'
	|| other->mychar == 'P'
	|| other->mychar == 'W')
	&& !mono) {

	if (!blockBitmaps) {
	    XSetForeground(dpy, scoreListGC, colors[BLACK].pixel);
	} else { 
	    /*
	    ** hm, this grey color is pretty, but am i guaranteed that there is 
	    ** 16 standard colors just because blockBitmaps = true?
	    */
	    XSetForeground(dpy, scoreListGC, colors[12].pixel);
	}	
	XDrawString(dpy, players, scoreListGC,
		    SCORE_BORDER, thisLine,
		    label, strlen(label));
    } else {
	ShadowDrawString(dpy, players, scoreListGC,
			 SCORE_BORDER, thisLine,
			 label,
			 colors[WHITE].pixel,
			 colors[BLACK].pixel);
    }

    /*
     * Underline the best player
     */
    if (best) {
	XDrawLine(dpy, players, scoreListGC,
		  SCORE_BORDER, thisLine,
		  players_width - SCORE_BORDER, thisLine);
    }
}


static void Paint_clock(int redraw)
{
    int			minute,
			hour,
			height = scoreListFont->ascent + scoreListFont->descent
				+ 3,
			border = 3;
    time_t		t;
    struct tm		*m;
    char		buf[16];
    static long		prev_loops;
    static int		width;

    if (BIT(instruments, SHOW_CLOCK) == 0) {
	if (width != 0) {
	    XSetForeground(dpy, scoreListGC, colors[windowColor].pixel);
	    XFillRectangle(dpy, players, scoreListGC,
			   256 - (width + 2 * border), 0,
			   width + 2 * border, height);
	    width = 0;
	}
	return;
    }
    if (redraw == 0
	&& loops > prev_loops
	&& loops - prev_loops < (FPS << 5)) {
	return;
    }
    prev_loops = loops;
    time(&t);
    m = localtime(&t);

    /* round seconds up to next minute. */
    minute = m->tm_min;
    hour = m->tm_hour;
    if (minute++ == 59) {
	minute = 0;
	if (hour++ == 23) {
	    hour = 0;
	}
    }
    if (!BIT(instruments, SHOW_CLOCK_AMPM_FORMAT)) {
	sprintf(buf, "%02d:%02d", hour, minute);
    } else {
	char tmpchar = 'A';
	/* strftime(buf, sizeof(buf), "%l:%M%p", m); */
	if (m->tm_hour > 12){
	    tmpchar = 'P';
	    m->tm_hour %= 12;
	}
	sprintf(buf, "%2d:%02d%cM", m->tm_hour, m->tm_min, tmpchar);
    }
    width = XTextWidth(scoreListFont, buf, strlen(buf));
    XSetForeground(dpy, scoreListGC, colors[windowColor].pixel);
    XFillRectangle(dpy, players, scoreListGC,
		   256 - (width + 2 * border), 0,
		   width + 2 * border, height);
    ShadowDrawString(dpy, players, scoreListGC,
		     256 - (width + border),
		     scoreListFont->ascent + 4,
		     buf,
		     colors[WHITE].pixel,
		     colors[BLACK].pixel);
}


void ShadowDrawString(Display* dpy, Window w, GC gc,
		      int x, int y, const char* str,
		      unsigned long fg, unsigned long bg)
{
    if (!mono) {
	XSetForeground(dpy, gc, bg);
	XDrawString(dpy, w, gc, x+1, y+1, str, strlen(str));
	x--; y--;
    }
    XSetForeground(dpy, gc, fg);
    XDrawString(dpy, w, gc, x, y, str, strlen(str));
}


