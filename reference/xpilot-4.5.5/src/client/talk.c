/* $Id: talk.c,v 5.4 2004/04/21 06:34:17 dik Exp $
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
#include <ctype.h>
#include <string.h>
#include <errno.h>

#ifndef _WINDOWS
# include <unistd.h>
# include <X11/Xlib.h>
# include <X11/Xutil.h>
# include <X11/Xatom.h>
# include <X11/Xmd.h>
#endif

#include "version.h"
#include "config.h"
#include "const.h"
#include "paint.h"
#include "xinit.h"
#include "error.h"
#include "netclient.h"
#include "protoclient.h"
#include "keys.h"
#include "bit.h"
#include "commonproto.h"

char talk_version[] = VERSION;


/* avoid trouble with Atoms and 64 bit archs */
typedef CARD32  Atom32;

/* Information window dimensions */
#define TALK_TEXT_HEIGHT	(textFont->ascent + textFont->descent)
#define TALK_OUTSIDE_BORDER	2
#define TALK_INSIDE_BORDER	3
#define TALK_WINDOW_HEIGHT	(TALK_TEXT_HEIGHT + 2 * TALK_INSIDE_BORDER)
#define TALK_WINDOW_X		(50 - TALK_OUTSIDE_BORDER)
#define TALK_WINDOW_Y		(draw_height*3/4 - TALK_WINDOW_HEIGHT/2)
#define TALK_WINDOW_WIDTH	(draw_width \
				    - 2*(TALK_WINDOW_X + TALK_OUTSIDE_BORDER))

#define CTRL(c)			((c) & 0x1F)

/*
 * Globals.
 */
bool			talk_mapped;

static bool		talk_created;
static char		talk_str[MAX_CHARS];
static struct {
    bool		visible;
    short		offset;
    short		point;
} talk_cursor;

/* position in history when browsing in the talk window */
static int		history_pos = 0;

/* faster cursor movement in talk window after pressing a key for some time */
#define CRS_START_HOPPING	7
#define CRS_HOP			4

/* selections in draw and talk window */
selection_t		selection;
bool			save_talk_str = false; /* see Get_msg_from_history */

extern keys_t Lookup_key(XEvent *event, KeySym ks, bool reset);
extern void Add_pending_messages(void);

extern message_t	*TalkMsg[MAX_MSGS], *GameMsg[MAX_MSGS];
extern char		*HistoryMsg[MAX_HIST_MSGS];

static void Talk_create_window(void)
{
    /*
     * Create talk window.
     */
    talk_w
	= XCreateSimpleWindow(dpy, draw,
			      TALK_WINDOW_X, TALK_WINDOW_Y,
			      TALK_WINDOW_WIDTH, TALK_WINDOW_HEIGHT,
			      TALK_OUTSIDE_BORDER, colors[WHITE].pixel,
			      colors[BLACK].pixel);

    if (!selectionAndHistory) {
	XSelectInput(dpy, talk_w, KeyPressMask | KeyReleaseMask | ExposureMask);
    } else {
	XSelectInput(dpy, talk_w, ButtonPressMask | ButtonReleaseMask |
				KeyPressMask | KeyReleaseMask | ExposureMask);
    }
}


void Talk_cursor(bool visible)
{
    if (talk_mapped == false || visible == talk_cursor.visible) {
	return;
    }
    if (visible == false) {
	XSetForeground(dpy, talkGC, colors[BLACK].pixel);
	XDrawString(dpy, talk_w, talkGC,
		    talk_cursor.offset + TALK_INSIDE_BORDER,
		    talkFont->ascent + TALK_INSIDE_BORDER,
		    "_", 1);
	XSetForeground(dpy, talkGC, colors[WHITE].pixel);
	if (talk_cursor.point < strlen(talk_str)) {
	    /* cursor _in message */
	    if (selectionAndHistory && selection.talk.state == SEL_EMPHASIZED
		  && talk_cursor.point >= selection.talk.x1
		  && talk_cursor.point < selection.talk.x2) {
		/* cursor in a selection? redraw the character emphasized */
		XSetForeground(dpy, talkGC, colors[DRAW_EMPHASIZED].pixel);
	    }
	    XDrawString(dpy, talk_w, talkGC,
			talk_cursor.offset + TALK_INSIDE_BORDER,
			talkFont->ascent + TALK_INSIDE_BORDER,
			&talk_str[talk_cursor.point], 1);
	    XSetForeground(dpy, talkGC, colors[WHITE].pixel);
	}
	talk_cursor.visible = false;
    } else {
	/* visible */
	talk_cursor.offset = XTextWidth(talkFont, talk_str, talk_cursor.point);
	/*
	 * goodie: `inverse' cursor (an underscore) if there is already an
	 * unemphasized underscore
	 */
	if (selectionAndHistory
	    && talk_cursor.point < strlen(talk_str)
	    && talk_str[talk_cursor.point] == '_'
	    && ( selection.talk.state != SEL_EMPHASIZED
		|| talk_cursor.point < selection.talk.x1
		|| talk_cursor.point >= selection.talk.x2) ) {
	    XSetForeground(dpy, talkGC, colors[DRAW_EMPHASIZED].pixel);
	}
	XDrawString(dpy, talk_w, talkGC,
		    talk_cursor.offset + TALK_INSIDE_BORDER,
		    talkFont->ascent + TALK_INSIDE_BORDER,
		    "_", 1);
	talk_cursor.visible = true;
    }
}


void Talk_map_window(bool map)
{
    static Window	root;
    static int		root_x, root_y;

    if (map == true) {
	Window child;
	int win_x, win_y;
	unsigned int keys_buttons;

	if (talk_created == false) {
	    Talk_create_window();
	    talk_created = true;
	}
	XMapWindow(dpy, talk_w);
	talk_mapped = true;

	XQueryPointer(dpy, DefaultRootWindow(dpy),
		      &root, &child, &root_x, &root_y, &win_x, &win_y,
		      &keys_buttons);
	XWarpPointer(dpy, None, talk_w,
		     0, 0, 0, 0,
		     TALK_WINDOW_WIDTH - (TALK_WINDOW_HEIGHT / 2),
		     TALK_WINDOW_HEIGHT / 2);
	XFlush(dpy);	/* warp pointer ASAP. */
    }
    else if (talk_created == true) {
	XUnmapWindow(dpy, talk_w);
	XWarpPointer(dpy, None, root, 0, 0, 0, 0, root_x, root_y);
	XFlush(dpy);	/* warp pointer ASAP. */
	talk_mapped = false;
	/* reset browsing */
	history_pos = -1;
    }
    talk_cursor.visible = false;
}

/*
 * redraw a possible selection [un]emphasized.
 * to unemphasize a selection, `selection.txt' is needed.
 * thus selection.talk.state == SEL_SELECTED indicates that it
 * should not be drawn emphasized
 */
static void Talk_refresh(void)
{
    int len;

    if (!selectionAndHistory || !talk_mapped)
	return;

    len = strlen(talk_str);
    if ( selection.talk.x1 > len || selection.talk.x2 > len) {
	/* don't redraw. it's not there anymore */
	return;
    } else if (len == 0) {
	XClearWindow(dpy, talk_w);
	return;
    }

    if (selection.talk.state == SEL_EMPHASIZED) {
	XSetForeground(dpy, talkGC, colors[DRAW_EMPHASIZED].pixel);
    } else {
	XSetForeground(dpy, talkGC, colors[WHITE].pixel);
    }
    XDrawString(dpy, talk_w, talkGC,
		selection.talk.x1 * XTextWidth(talkFont, talk_str, 1)
		 + TALK_INSIDE_BORDER,
		talkFont->ascent + TALK_INSIDE_BORDER,
		&talk_str[selection.talk.x1], selection.talk.x2 - selection.talk.x1 );
    XSetForeground(dpy, talkGC, colors[WHITE].pixel);
}

/*
 * add a line to the history.
 */
static void Add_msg_to_history(char *message)
{
    char *tmp;
    char **msg_set;
    int i;

    /*always*/
    save_talk_str = false;

    if (!selectionAndHistory || strlen(message) == 0) {
	return; /* unexpected. nothing to add */
    }

    msg_set = HistoryMsg;
    /* pipe the msgs through the buffer, the latest getting into [0] */
    tmp = msg_set[maxLinesInHistory - 1];
    for (i = maxLinesInHistory - 1; i > 0; i--) {
	msg_set[i] = msg_set[i - 1];
    }
    msg_set[0] = tmp; /* memory recycling */

    strlcpy(msg_set[0], message, MAX_CHARS);
    history_pos = 0;

    return;
}

/*
 * Fetch a message from the `history' by returning a pointer to it.
 * Choice depends on current poition (*pos, call by ref for modification here)
 * and `direction' of browsing
 *
 * if we are here _and the gobal `save_talk_str' is set, then the submitted
 * message is unfinished - call Add_msg_to_history(), but don't return
 * the next line.
 * Purpose: gives ability to abort writing a message and resume later.
 * The global `save_talk' can be set anywhere else in the code whenever
 * a line from the history gets modified
 * (thus save_talk not as parameter here)
 *
 */
static char *Get_msg_from_history(int* pos, char *message, keys_t direction)
{
    int i;
    char **msg_set;

    if (!selectionAndHistory
	|| (direction != KEY_TALK_CURSOR_UP && direction != KEY_TALK_CURSOR_DOWN
	    && direction != KEY_DUMMY)) {
	return NULL;
    }

    if (direction == KEY_DUMMY && (*pos < 0 || *pos > maxLinesInHistory-1)) {
	*pos = 0;
    }

    msg_set = HistoryMsg;

    if (save_talk_str) {
	if (strlen(message) > 0) {
	    Add_msg_to_history(message);
	}
	save_talk_str = false;
	return NULL;
    }

    /* search for the next message, return it */
    for (i=0; i < maxLinesInHistory; i++) {
	if (direction == KEY_TALK_CURSOR_UP) {
            (*pos)++;
            if (*pos >= maxLinesInHistory) {
                *pos = 0; /* wrap */
            }
	} else if (direction == KEY_TALK_CURSOR_DOWN) {
	    (*pos)--;
	    if (*pos < 0) {
		*pos = maxLinesInHistory - 1; /*wrap*/
	    }
	}
	if (strlen(msg_set[*pos]) > 0) {
	    return (msg_set[*pos]);
	}
    }
    return NULL; /* no history */
}

/*
 * Print all available messages to stdout.
 */
void Print_messages_to_stdout(void)
{
    int i, k;
    int direction, offset;

    if (!selectionAndHistory)
	return;

    if (BIT(instruments, SHOW_REVERSE_SCROLL)) {
	direction	= -1;
	offset		= maxMessages - 1;
    } else {
	direction	= 1;
	offset		= 0;
    }

    xpprintf("[talk messages]\n");
    for (k = 0; k < maxMessages; k++) {
	i = direction * k + offset;
	if (TalkMsg[i] && TalkMsg[i]->len > 0) {
	    xpprintf("  %s\n", TalkMsg[i]->txt);
	}
    }

    xpprintf("[server messages]\n");
    for (k = maxMessages - 1; k >= 0; k--) {
	i = direction * k + offset;
	if (GameMsg[i] && GameMsg[i]->len > 0) {
	    xpprintf("  %s\n", GameMsg[i]->txt);
	}
    }
    xpprintf("\n");
}

/*
 * Pressing a key while there is an emphasized text in the talk window
 * substitutes this text, means: delete the text before inserting the
 * new character and place the character at this `gap'.
 */
static void Talk_delete_emphasized_text(void)
{

    int		oldlen, newlen;
    int		onewidth = XTextWidth(talkFont, talk_str, 1);
    char	new_str[MAX_CHARS];

    if (! (selectionAndHistory && selection.talk.state == SEL_EMPHASIZED)) {
	return;
    }

    Talk_cursor(false);

    strlcpy(new_str, talk_str, MAX_CHARS);
    oldlen = strlen(talk_str);
    newlen = oldlen;

    if (oldlen > 0) {
	strncpy(&new_str[selection.talk.x1], &new_str[selection.talk.x2],
		oldlen - selection.talk.x2);
	new_str[selection.talk.x1 + oldlen - selection.talk.x2] = '\0';
	talk_cursor.point = selection.talk.x1;
	newlen -= (selection.talk.x2 - selection.talk.x1);
	selection.talk.state = SEL_NONE;
	new_str[newlen] = '\0';
	if (talk_cursor.point > newlen) {
	    talk_cursor.point = newlen;
	}
    }
    new_str[newlen] = '\0';
    if (talk_cursor.point > newlen) {
	talk_cursor.point = newlen;
    }

    /*
     * Now reflect the text changes onto the screen.
     */
    if (newlen < oldlen) {
	XSetForeground(dpy, talkGC, colors[BLACK].pixel);
	XDrawString(dpy, talk_w, talkGC,
		    talk_cursor.point * onewidth + TALK_INSIDE_BORDER,
		    talkFont->ascent + TALK_INSIDE_BORDER,
		    &talk_str[talk_cursor.point],
		    oldlen - talk_cursor.point);
	XSetForeground(dpy, talkGC, colors[WHITE].pixel);
    }
    if (talk_cursor.point < newlen) {
	XDrawString(dpy, talk_w, talkGC,
		    talk_cursor.point * onewidth + TALK_INSIDE_BORDER,
		    talkFont->ascent + TALK_INSIDE_BORDER,
		    &new_str[talk_cursor.point],
		    newlen - talk_cursor.point);
    }
    Talk_cursor(true);

    strlcpy(talk_str, new_str, MAX_CHARS);
}

int Talk_do_event(XEvent *event)
{
    char		ch;
    bool		cursor_visible = talk_cursor.visible;
    int			oldlen, newlen, onewidth, oldwidth;
    KeySym		keysym;
    char		new_str[MAX_CHARS];
    bool		result = true;
    /* int			str_len; of a history message */
    static int		talk_crs_repeat_count;

    /* according to Ralf Berger <gotan@physik.rwth-aachen.de>
     * compose should be static:
     * the value of 'compose' has to be preserved across calls to
     * 'XLookupString' (the man page also mentioned that a portable program
     * should pass NULL but i don't know if that's specific to dec-alpha ?).
     * To fix this declare 'compose' static (can't hurt anyway).
     */
    static XComposeStatus	compose;

    switch (event->type) {

    case Expose:
	XClearWindow(dpy, talk_w);
	XDrawString(dpy, talk_w, talkGC,
		    TALK_INSIDE_BORDER, talkFont->ascent + TALK_INSIDE_BORDER,
		    talk_str, strlen(talk_str));
	if (selectionAndHistory && selection.talk.state == SEL_EMPHASIZED) {
	    Talk_refresh();
	}
	if (cursor_visible == true) {
	    talk_cursor.visible = false;
	    Talk_cursor(cursor_visible);
	}
	break;

    case KeyRelease:
	/*
	 * stop faster cursor movement in talk window
	 */
	if (selectionAndHistory)
	    talk_crs_repeat_count = 0;
	/*
	 * Nothing to do.
	 * We may want to make some kind of key repeat ourselves.
	 * Some day...
	 */
	break;

    case KeyPress:
	/* `strange' keys exist */
	if ((keysym = XLookupKeysym(&event->xkey, 0)) == NoSymbol)
	    break;

	onewidth = XTextWidth(talkFont, talk_str, 1);

	/* `unprintables'? */
	if (XLookupString(&event->xkey, &ch, 1, &keysym, &compose)
		== NoSymbol) {

	    keys_t key;		/* what key is it */
	    char *tmp;		/* for receiving a line from the history */

	    if (!selectionAndHistory)
		break;	/* out of `KeyPress' */

	    /* search the `key' */
	    for (key = Lookup_key(event, keysym, true);
		 key != KEY_DUMMY;
		 key = Lookup_key(event, keysym, false)) {
		switch (key) {
		case KEY_TALK_CURSOR_RIGHT:
		    Talk_cursor(false);
		    /*
		     * faster cursor movement after some time.
		     * `talk_crs_repeat_count' is reset at `KeyRelease'
		     */
		    if (talk_crs_repeat_count > CRS_START_HOPPING) {
			if (talk_cursor.point < strlen(talk_str) - CRS_HOP) {
			    talk_cursor.point += CRS_HOP;
			} else {
			    talk_cursor.point = strlen(talk_str);
			}
		    } else {
			if ( talk_cursor.point < strlen(talk_str) ){
			    talk_cursor.point++;
			    talk_crs_repeat_count++;
			}
		    }
		    Talk_cursor(true);
		    break; /* out of switch(key) */

		case KEY_TALK_CURSOR_LEFT:
                    Talk_cursor(false);
                    /* faster cursor movement after some time */
                    if (talk_crs_repeat_count > CRS_START_HOPPING) {
                        if (talk_cursor.point > CRS_HOP) {
                            talk_cursor.point -= CRS_HOP;
                        } else {
                            talk_cursor.point = 0;
                        }
                    } else {
                        if ( talk_cursor.point > 0 ){
                            talk_cursor.point--;
			    talk_crs_repeat_count++;
                        }
                    }
                    Talk_cursor(true);
		    break;

		case KEY_TALK_CURSOR_UP:
		case KEY_TALK_CURSOR_DOWN:
		    /*
		     * browsing in history.
		     * Get_msg_from_history() will notice if you start
		     * browsing while having an unfinished message:
		     *   you want to store it and will type a new one.
		     *   thus Get_msg_from_history() won't return the next
		     *   message _this time. clear the talk window instead.
		     */
		    tmp = Get_msg_from_history(&history_pos, talk_str,
						    key);
		    if (tmp && strlen(tmp) > 0) {
			/* we got smthng from the history */
			/*strlcpy(talk_str, tmp, MAX_CHARS);
			str_len = strlen(talk_str); */
			Talk_paste(tmp, strlen(tmp), true);
		    } else {
			talk_str[0] = '\0';
			Talk_cursor(false);
			talk_cursor.point = 0;
			Talk_cursor(true);
			XClearWindow(dpy, talk_w);
		    }
		    break;

		default:
		    break;

		} /* switch */
	    } /* for */
	    break; /* out of `KeyPress' */
	} /* XLookupString() == NoSymbol */

	/*
	 * printable or a <ctrl-char>
	 */

	if (selectionAndHistory) {
	    /*
	     * unemphasize?
	     * The key might change the talk string (important when browsing).
	     */
	    switch (ch) {
	    /* special handling for */
	    case CTRL('A'): /* only cursor movement */
	    case CTRL('E'):
	    case CTRL('B'):
	    case CTRL('F'):
	    case CTRL('\r'): /* perhaps nothing was changed */
	    case CTRL('\n'):
	    case '\033':     /* canceled */
		break;
	    default:
		if (isprint(ch)) {
		    /*
		     * the string is modified.
		     * next Get_msg_from_history() will store it in the history
		     */
		    save_talk_str = true;
		    if (selection.talk.state == SEL_EMPHASIZED) {
			/* convenient deleting of text */
			Talk_delete_emphasized_text();
		    }
		}
		break;
	    }
	}

	switch (ch) {
	case '\0':
	    /*
	     * ?  Ignore.
	     */
	    break;

	case '\r':
	case '\n':
	    /*
	     * Return.  Send the talk message to the server if there is text.
	     */
	    if (talk_str[0] != '\0') {
		if (selectionAndHistory && selection.talk.state == SEL_EMPHASIZED) {
		    /*
		     * send a message. it will appear as talk message.
		     * keep a possible emphasizing and set selection.draw.*
		     * to proper values.
		     */
		    selection.talk.state = SEL_SELECTED;
		    selection.keep_emphasizing = true;
		    selection.draw.x1 = selection.talk.x1;
		    selection.draw.x2 = selection.talk.x2;
		}
		/* add to history if the message was not gotten by browsing */
		if (selectionAndHistory && save_talk_str) {
		    Add_msg_to_history(talk_str);
		}
		Net_talk(talk_str);
		talk_cursor.point = 0;
		talk_str[0] = '\0';
	    } else if (selectionAndHistory) {
		/* talk_str is empty */
		save_talk_str = false;
		history_pos = 0;		/* reset history position */
	    }
	    result = false;
	    break;

	case '\033':
	    /*
	     * Escape.  Cancel talking.
	     */
	    talk_str[0] = '\0';
	    talk_cursor.point = 0;
	    if (selectionAndHistory)
		save_talk_str = false;
	    result = false;
	    break;

	case CTRL('A'):
	    /*
	     * Put cursor at start of line.
	     */
	    Talk_cursor(false);
	    talk_cursor.point = 0;
	    Talk_cursor(true);
	    break;

	case CTRL('E'):
	    /*
	     * Put cursor at end of line.
	     */
	    Talk_cursor(false);
	    talk_cursor.point = strlen(talk_str);
	    Talk_cursor(true);
	    break;

	case CTRL('B'):
	    /*
	     * Put cursor one character back.
	     */
	    if (talk_cursor.point > 0) {
		Talk_cursor(false);
		talk_cursor.point--;
		Talk_cursor(true);
	    }
	    break;

	case CTRL('F'):
	    /*
	     * Put cursor one character forward.
	     */
	    if (talk_cursor.point < strlen(talk_str)) {
		Talk_cursor(false);
		talk_cursor.point++;
		Talk_cursor(true);
	    }
	    break;

	case '\b':
	case '\177':
	case CTRL('D'):
	case CTRL('W'):
	case CTRL('U'):
	case CTRL('K'):
	    /*
	     * Erase characters.
	     */
	    Talk_cursor(false);

	    strlcpy(new_str, talk_str, MAX_CHARS);
	    oldlen = strlen(talk_str);
	    newlen = oldlen;

	    /*
	     * Calculate text changes first without drawing.
	     */
	    if (ch == CTRL('W')) {
		/*
		 * Word erase.
		 * Erase whitespace first and then one word.
		 */
		while (newlen > 0 && talk_str[newlen - 1] == ' ') {
		    newlen--;
		}
		while (newlen > 0 && talk_str[newlen - 1] != ' ') {
		    newlen--;
		}
	    }
	    else if (ch == CTRL('U')) {
		/*
		 * Erase everything.
		 */
		newlen = 0;
	    }
	    else if (ch == CTRL('K')) {
		/*
		 * Clear rest of the line.
		 */
		newlen = talk_cursor.point;
	    }
	    else if (oldlen > 0) {
		if (selectionAndHistory && selection.talk.state == SEL_EMPHASIZED) {
		    /*
		     * Erase the emphasized text
		     */
		    strncpy(&new_str[selection.talk.x1], &new_str[selection.talk.x2],
			    oldlen - selection.talk.x2);
		    new_str[selection.talk.x1 + oldlen - selection.talk.x2] = '\0';
		    talk_cursor.point = selection.talk.x1;
		    newlen -= (selection.talk.x2 - selection.talk.x1);
		    selection.talk.state = SEL_NONE;
		} else {
		    if (selectionAndHistory) {
			/*
			 * Erase possibly several characters.
			 */
			if (talk_crs_repeat_count > CRS_START_HOPPING) {
			    if (talk_cursor.point > CRS_HOP) {
				newlen -= CRS_HOP;
				if (ch != CTRL('D')
				    || talk_cursor.point >= newlen) {
				    talk_cursor.point -= CRS_HOP;
				}
				strlcpy(&new_str[talk_cursor.point],
				       &talk_str[talk_cursor.point + CRS_HOP],
				       MAX_CHARS - talk_cursor.point);
			    } else {
				int old_talk_cursor_point = talk_cursor.point;
				newlen -= talk_cursor.point;
				if (ch != CTRL('D')
				    || talk_cursor.point >= newlen) {
				    talk_cursor.point = 0;
				}
				strlcpy(&new_str[0],
				       &talk_str[old_talk_cursor_point],
				       MAX_CHARS);
			    }
			} else {
			    /*
			     * Erase one character.
			     */
			    if ( talk_cursor.point > 0 ){
				newlen--;
				if (ch != CTRL('D')
				    || talk_cursor.point >= newlen) {
				    talk_cursor.point--;
				}
				talk_crs_repeat_count++;
				strlcpy(&new_str[talk_cursor.point],
				       &talk_str[talk_cursor.point + 1],
				       MAX_CHARS - talk_cursor.point);
			    }
			}
		    } else {
			/*
			 * Erase one character.
			 */
			newlen--;
			if (ch != CTRL('D') || talk_cursor.point >= newlen) {
			    if (talk_cursor.point > 0) {
				talk_cursor.point--;
			    }
			}
			strlcpy(&new_str[talk_cursor.point],
			        &talk_str[talk_cursor.point + 1],
			        MAX_CHARS);
		    }
		}
	    }

	    new_str[newlen] = '\0';
	    if (talk_cursor.point > newlen) {
		talk_cursor.point = newlen;
	    }

	    /*
	     * Now reflect the text changes onto the screen.
	     */
	    if (newlen < oldlen) {
		XSetForeground(dpy, talkGC, colors[BLACK].pixel);
		XDrawString(dpy, talk_w, talkGC,
			    talk_cursor.point * onewidth + TALK_INSIDE_BORDER,
			    talkFont->ascent + TALK_INSIDE_BORDER,
			    &talk_str[talk_cursor.point],
			    oldlen - talk_cursor.point);
		XSetForeground(dpy, talkGC, colors[WHITE].pixel);
	    }
	    if (talk_cursor.point < newlen) {
		XDrawString(dpy, talk_w, talkGC,
			    talk_cursor.point * onewidth + TALK_INSIDE_BORDER,
			    talkFont->ascent + TALK_INSIDE_BORDER,
			    &new_str[talk_cursor.point],
			    newlen - talk_cursor.point);
	    }
	    Talk_cursor(cursor_visible);

	    strlcpy(talk_str, new_str, MAX_CHARS);

	    break;

	default:
	    if ((ch & 0x7F) == ch && !isprint(ch)) {
		/*
		 * Unknown special character.
		 */
		break;
	    }

	    oldlen = strlen(talk_str);
	    oldwidth = XTextWidth(talkFont, talk_str, oldlen);
	    if (oldlen >= MAX_CHARS - 2
		|| oldwidth >= TALK_WINDOW_WIDTH - 2*TALK_INSIDE_BORDER - 5) {
		/*
		 * No more space for new text.
		 */
		XBell(dpy, 100);
		break;
	    }

	    /*
	     * Enter new text.
	     */
	    strlcpy(new_str, talk_str, MAX_CHARS);
	    strlcpy(&new_str[talk_cursor.point + 1],
		    &talk_str[talk_cursor.point],
		    MAX_CHARS - (talk_cursor.point + 1));
	    new_str[talk_cursor.point] = ch;
	    newlen = oldlen + 1;

	    /*
	     * Reflect text changes onto screen.
	     */
	    Talk_cursor(false);
	    if (talk_cursor.point < oldlen) {
		/*
		 * Erase old text from cursor to end of line.
		 */
		XSetForeground(dpy, talkGC, colors[BLACK].pixel);
		XDrawString(dpy, talk_w, talkGC,
			    talk_cursor.point * onewidth + TALK_INSIDE_BORDER,
			    talkFont->ascent + TALK_INSIDE_BORDER,
			    &talk_str[talk_cursor.point],
			    oldlen - talk_cursor.point);
		XSetForeground(dpy, talkGC, colors[WHITE].pixel);
	    }
	    XDrawString(dpy, talk_w, talkGC,
			talk_cursor.point * onewidth + TALK_INSIDE_BORDER,
			talkFont->ascent + TALK_INSIDE_BORDER,
			&new_str[talk_cursor.point],
			newlen - talk_cursor.point);
	    talk_cursor.point++;
	    Talk_cursor(cursor_visible);

	    strlcpy(talk_str, new_str, MAX_CHARS);

	    break;
	}
	XFlush(dpy);	/* needed in case we don't get frames to draw soon. */

	/*
	 * End of KeyPress.
	 */
	break;

    default:
	break;
    }

    return result;	/* keep on talking if true, no more talking if false */
}

/*
 * Try to paste `data_len' amount of `data' at the cursor position into
 * the talk window.  Cut off overflow (`accept_len').
 *
 * The global `talk_str' will contain the new content.
 * (safe if *data references *talk_str).
 *
 * if `overwrite' then don't insert/append but substitute the text
 *
 * Return the number of pasted characters.
 */
int Talk_paste(char *data, int data_len, bool overwrite)
{

    int str_len;			/* current length */
    int max_len    = MAX_CHARS - 2;	/* absolute max */
    int new_len;			/* after pasting */
    int char_width = XTextWidth(talkFont, talk_str, 1);
    int max_width  = (TALK_WINDOW_WIDTH - 2*TALK_INSIDE_BORDER - 5);

    int accept_len;			/* for still matching the window */
    char paste_buf[MAX_CHARS -2];	/* gets the XBuffer */
    char tmp_str[MAX_CHARS - 2];
    char talk_backup[MAX_CHARS - 2];	/* no `collision' with data */
    bool cursor_visible = false;
    int i;

	if (char_width == 0)
		char_width = 1;
    if (!selectionAndHistory || !data || data_len == 0 || strlen(data) == 0) {
	return 0;
    }

    if (overwrite) {
	/* implicitly, old text will be deleted now: */
	str_len = 0;
    } else {
	str_len = strlen(talk_str);
	strlcpy(talk_backup, talk_str, sizeof(talk_backup));
    }
    accept_len = (max_width / char_width) - str_len + 1;
    if (accept_len + str_len > max_len)
	accept_len = max_len - str_len;

    if (!accept_len) {
	/* no room to broom */
	XBell(dpy, 100);
	return 0;
    }
    if (data_len > accept_len) {
	/* not all accepted */
	XBell(dpy, 100);
    } else if (data_len < accept_len) {
	/* not the whole string required to paste */
	accept_len = data_len;
    }
    strncpy(paste_buf, data , accept_len);
    paste_buf[accept_len] = '\0';

    /*
     * substitute unprintables according to iso-latin-1.
     *  (according to `data_len' one could ask for all but the
     *   final '\0' to be converted, but we have only text selections anyway)
     */
    /* don't convert a final newline to space */
    if (paste_buf[accept_len-1] == '\n' || paste_buf[accept_len-1] == '\r') {
	paste_buf[accept_len-1] = '\0';
	accept_len--;
    }
    for(i = 0; i<accept_len; i++) {
        if (	  ((unsigned char)paste_buf[i] <   33
		/* && (unsigned char)paste_buf[i] != '\0' */)
	    ||    ((unsigned char)paste_buf[i] >  126
		&& (unsigned char)paste_buf[i] <  161) ) {
            paste_buf[i] = ' ';
        }
    }

    if (overwrite) {
	strncpy(tmp_str, paste_buf, accept_len);
	tmp_str[accept_len] = '\0';
	new_len = accept_len;
    } else {
	/* paste: insert/append */
	strlcpy(tmp_str, talk_backup, sizeof(tmp_str));
	strlcpy(&tmp_str[talk_cursor.point], paste_buf,
		sizeof(tmp_str) - talk_cursor.point);
	strlcpy(&tmp_str[talk_cursor.point + accept_len],
	        &talk_backup[talk_cursor.point],
	        sizeof(tmp_str) - (talk_cursor.point + accept_len));
	new_len = str_len + accept_len;
    }


    /*
     * graphics
     */
    if (overwrite) {
	XClearWindow(dpy, talk_w);
	XSetForeground(dpy, talkGC, colors[WHITE].pixel);
	XDrawString(dpy, talk_w, talkGC,
		    TALK_INSIDE_BORDER,
		    talkFont->ascent + TALK_INSIDE_BORDER,
		    tmp_str, accept_len );
    } else {
	if (selection.talk.state == SEL_EMPHASIZED) {
	    /* redraw unemphasized */
	    selection.talk.state = SEL_SELECTED;
	    Talk_refresh();
	}
	if (talk_cursor.point < str_len) {
	    /*
	     * erase from the point of insertion on
	     */
	    XSetForeground(dpy, talkGC, colors[BLACK].pixel);
	    XDrawString(dpy, talk_w, talkGC,
			talk_cursor.point * char_width + TALK_INSIDE_BORDER,
			talkFont->ascent + TALK_INSIDE_BORDER,
			&talk_backup[talk_cursor.point],
			str_len - talk_cursor.point);
	    XSetForeground(dpy, talkGC, colors[WHITE].pixel);
	}

	/* the new part of the line */
	XDrawString(dpy, talk_w, talkGC,
		    talk_cursor.point * char_width + TALK_INSIDE_BORDER,
		    talkFont->ascent + TALK_INSIDE_BORDER,
		    &tmp_str[talk_cursor.point],
		    new_len - talk_cursor.point);
    }
    strlcpy(talk_str, tmp_str, sizeof(talk_str));

    cursor_visible = talk_cursor.visible;
    Talk_cursor(false);
    if (overwrite) {
	talk_cursor.point = new_len;
    } else {
	talk_cursor.point += accept_len;
    }
    Talk_cursor(cursor_visible);

    return accept_len;
}


void Talk_resize(void)
{
    if (talk_created) {
	XMoveResizeWindow(dpy, talk_w,
			  TALK_WINDOW_X, TALK_WINDOW_Y,
			  TALK_WINDOW_WIDTH, TALK_WINDOW_HEIGHT);
    }
}

/*
 * place the cursor in the talk window with help of the pointer button.
 * return the cursor position as index in talk_str.
 *
 * place cursor conveniently, if `pending' is set and cutting
 * in the talk window finished outside of it (border is also outside).
 */
int Talk_place_cursor(XButtonEvent* xbutton, bool pending)
{
    int		x, y;		/* pixelpositions */
    int		cursor_pos;	/* string index */
    int		Button = xbutton->button;
    int		onewidth = XTextWidth(talkFont, talk_str, 1);

    if (!selectionAndHistory)
	return -1;

    x = xbutton->x;
    y = xbutton->y;

    x -= TALK_INSIDE_BORDER + 1; /* tuned */

    /*
     * some applications behave like this
     *   double frac = (double)x;
     *   cursor_pos = (int)rint(frac / (double)onewidth);
     * (middle of character)
     */
    cursor_pos = x / onewidth;

    /*
     * if it happened outside the window
     */
    if ( cursor_pos < 0 || cursor_pos > TALK_WINDOW_WIDTH/onewidth
	|| y < 0 || y >= TALK_WINDOW_HEIGHT) {
	if (Button == 1 && pending) {
	    /* convenient finish of cutting */
	    if ( cursor_pos < selection.talk.x1) {
		cursor_pos = 0; /* left end */
	    } else {
		cursor_pos = TALK_WINDOW_WIDTH/onewidth; /* right end */
		selection.talk.incl_nl = true;
	    }
	} else {
	    cursor_pos = 0;
	}
    }

    /* no implicit lengthening of talk_str */
    if (cursor_pos > strlen(talk_str)) {
        if (Button == 1) {
            selection.talk.incl_nl = true;
        }
        cursor_pos = strlen(talk_str);
    }

    /* place cursor with pointer */
    Talk_cursor(false);
    talk_cursor.point = cursor_pos;
    Talk_cursor(true);

    return cursor_pos;
}

static void Clear_talk_selection(void)
{
    selection.talk.x1 = selection.talk.x2 = 0;
    selection.talk.state = SEL_NONE;
    selection.talk.incl_nl = false;
}

static void Clear_draw_selection(void)
{
    selection.draw.x1 = selection.draw.x2
	= selection.draw.y1 = selection.draw.y2 = 0;
    selection.draw.state = SEL_NONE;
}

/*
 * If a cut doesn't suceed, leave the selection in appropriate state.
 */
static void Selection_set_state(void)
{
    if (selection.len > 0)
	selection.draw.state = SEL_SELECTED;
    else
	selection.draw.state = SEL_NONE;
}

/*
 * show that someone else owns the selection now
 */
void Clear_selection(void)
{
    if (!selectionAndHistory)
	return;

    if (talk_mapped && selection.talk.state == SEL_EMPHASIZED) {
	/* trick to unemphasize */
	selection.talk.state = SEL_SELECTED;
	Talk_refresh();
    }
    Clear_draw_selection();
    Clear_talk_selection();
    if (selection.txt) {
	free(selection.txt);
    }
    selection.txt = NULL;
    selection.txt_size = 0;
    selection.len = 0;

}

/*
 * cutting from talk window happens here. wanted: selection.txt and the
 * string indices of start and end (selection.draw.x1/2)
 * call Talk_place_cursor() to place the cursor.
 */
void Talk_window_cut(XButtonEvent* xbutton)
{
    int		cursor_pos;	/* where to place the text cursor */
    int		ButtonState = xbutton->type;
    int		Button      = xbutton->button;
    char	tmp[MAX_CHARS];	/* preparing a string for the cut buffer */
    bool	was_pending = false;


    if (!selectionAndHistory)
	return;

    /* convenient cursor placement when finishing a cut */
    if (selection.talk.state == SEL_PENDING
	  && ButtonState == ButtonRelease && Button == Button1) {
	was_pending = true;
    }
    cursor_pos = Talk_place_cursor(xbutton, was_pending);

    if (ButtonState == ButtonPress) {
	/*
	 * a cut began. redraw unemphasized
	 */
	selection.talk.state = SEL_PENDING;
	Talk_refresh();
	selection.talk.x1 = cursor_pos;
	selection.talk.incl_nl = false;

    } else if (ButtonState == ButtonRelease) {
	if (selection.talk.state != SEL_PENDING) {
	    /*
	     * cut didn't start properly
	     */
	    return;
	}
	selection.talk.x2 = cursor_pos;
	if (selection.talk.x1 == selection.talk.x2) {
	    /* no cut */
	    selection.talk.state = SEL_NONE;
	    return;
	}

	/*
	 * A real cut has been made
	 */
	Clear_draw_selection();
	selection.txt_size = MAX_MSGS * MSG_LEN;
	selection.txt = (char *)malloc(selection.txt_size);
	if (selection.txt == NULL) {
	    error("No memory for Selection");
	    return;
        }

	/* swap order, if necessary */
	if (selection.talk.x1 > selection.talk.x2) {
	    int tmp = selection.talk.x2;
	    selection.talk.x2 = selection.talk.x1;
	    selection.talk.x1 = tmp;
	}

	/*
	 * making the cut available; see end of Talk_cut_from_messages()
	 */
	XSetSelectionOwner(dpy, XA_PRIMARY, talk_w, CurrentTime);
	/* `cut buffer' is binary stuff - append '\0'  */
	strncpy(tmp, &talk_str[selection.talk.x1],
		selection.talk.x2 - selection.talk.x1);
	tmp[selection.talk.x2 - selection.talk.x1] = '\0';
	if (selection.talk.incl_nl) {
	    strcat(tmp, "\n");
	    selection.talk.incl_nl = false;
	}
	strlcpy(selection.txt, tmp, selection.txt_size);
	selection.len = strlen(tmp);
	XStoreBytes(dpy, selection.txt, selection.len);

	/*
	 * emphasize the selection
	 */
	selection.talk.state = SEL_EMPHASIZED;
	selection.draw.state = SEL_NONE;
	Talk_refresh();

    } /* ButtonRelease */
}

/*
 * wanted:
 * indices of the characters and the rows of start and end of
 * the selected text.
 *
 * for proper cutting: Notice if cutting heppens left from the most left
 * or right from the most right character (c1/2.x_off).
 *
 * while the cut is pending, the state of Talk+GameMsg[] is `freezed'
 * in Paint_message(). thus call Add_pending_messages() here after this.
 */
void Talk_cut_from_messages(XButtonEvent* xbutton)
{

    const int	BORDER = 10,
		SPACING = messageFont->ascent+messageFont->descent+1;

    int		x, y;	/* of initial ButtonEvent */

    typedef struct {
	int x;		/* pixel positions */
	int y;
	int pixel_len;  /* of whole string */
	int str_index;  /* wanted: index in the message */
	/* at the end of a string? */
	int x_off;	/* -1: left end,  +1: right end */
    } cut_position;

    static cut_position c1 = {0, 0, 0, 0, 0,};

    static int	last_msg_index;	/* index of last message */
    int		i;

    if (!selectionAndHistory)
	return;

    /* quick check if there are messages at all */
    if (TalkMsg[0]->len == 0) {
	if (selection.len > 0)
	    selection.draw.state = SEL_SELECTED;
	else
	    selection.draw.state = SEL_NONE;
	return;
    }

    x = xbutton->x - BORDER;
    y = xbutton->y - BORDER;

    if (y < 0) {
	y = -1;
    } else {
	y /= SPACING;
    }

   /*
    * ..............Button Press...............
    * store the important things
    */
    if (xbutton->type == ButtonPress) {
	/* how many messages? */
	last_msg_index = 0;
	while (last_msg_index < maxMessages
		&& TalkMsg[last_msg_index]->len != 0) {
	    last_msg_index++;
	}
	last_msg_index--; /* make it an index */

	c1.x = x;
	c1.y = y;
	c1.x_off = c1.str_index = c1.pixel_len = 0;

	/*
	 * first adjustments
	 */
	if (c1.y < 0) {
	    /* upper-left end */
	    c1.x	= -1;
	    c1.y	= 0;
	} else if (c1.y > last_msg_index) {
	    /* lower-right end */
	    c1.x_off	= 1;
	    c1.y	= last_msg_index;
	    c1.x	= 0; /* stay safe */
	} else if (c1.x < 0) {
	    /* left end */
	    c1.x	 = -1;
	}
	selection.draw.state = SEL_PENDING;
	return;

   /*
    * ..............Button Release...............
    */
    } else if (xbutton->type == ButtonRelease) {

	/* dummies for `XTextExtents', the faster version of XTextWidth */
	int	font_ascent_return, font_descent_return, direction_return;
	/* wanted: overall_return.width */
	XCharStruct overall_return;
	message_t *ptr;

	cut_position c2 = {0, 0, 0, 0, 0,};

	/*
	 * selection.txt will contain the whole selection
	 */
	char	cut_str[MSG_LEN]; /* for fetching the messages line by line */
	int	cut_str_len;
	int	next = 0;	/* how to walk through the messages */
	int 	current_line;	/* when going through a multi line selection */


	if ( selection.draw.state != SEL_PENDING) {
	    /* no proper start of cut */
	    if (selection.len > 0)
		selection.draw.state = SEL_SELECTED;
	    else
		selection.draw.state = SEL_NONE;
	    return;
	}

	/*
	 * The cut has been made
	 */

	c2.x = x;
	c2.y = y;
	c2.x_off = c2.str_index = c2.pixel_len = 0;

        if (c2.y < 0) {
	    /* upper-left end */
	    c2.x	= -1;
	    c2.y	= 0;
        } else if (c2.y > last_msg_index) {
	    /* lower-right end */
	    c2.x_off	= 1;
	    c2.y	= last_msg_index;
	    c2.x	= 0;
        } else if (c2.x < 0) {
	    /* left end */
	    c2.x	 = -1;
	}

	/* swap order? */
	if (c2.y < c1.y
	    || (c2.y == c1.y
		 && (c2.x_off - c1.x_off < 0
	             || (c2.x < c1.x && (c2.x_off - c1.x_off == 0))))) {
	    cut_position backup = c1;
	    c1 = c2;
	    c2 = backup;
	}

	/*
	 *further decisions, as start and end are in order now
	 */

	/* cut finished at next line - without a character there, though */
	if (c2.x == -1  &&  c1.y < c2.y) {
	    c2.y	-= 1;
	    c2.x_off	= 1 ;
	    c2.x	= 0;
	}
	/* cut started at end of line; jump to next if possible */
	if ((c1.x > TalkMsg[TALK_MSG_SCREENPOS(last_msg_index, c1.y)]->pixelLen
	    || c1.x_off == 1)  &&  c1.y < c2.y) {
	    c1.x	= 0;
	    c1.y	+= 1;
	}
	if (c1.x == -1)
	    c1.x = 0;

	/*
	 * find the indices in the talk string
	 */
	ptr = TalkMsg[TALK_MSG_SCREENPOS(last_msg_index,c1.y)];
	c1.str_index = 0;
	if (c1.x_off == 1) {
	    c1.str_index = ptr->len - 1;
	} else {
	    for (i = 0; i <= ptr->len; i++) {
		XTextExtents(messageFont, ptr->txt, i,
			    &direction_return,
			    &font_ascent_return, &font_descent_return,
			    &overall_return);
		if (overall_return.width >= c1.x) {
		    break;
		}
		c1.str_index = i; /* get maximum implicitly */
	    }
	}

	ptr = TalkMsg[TALK_MSG_SCREENPOS(last_msg_index,c2.y)];
	c2.str_index = 0;
	if (c2.x_off == 1) {
	    c2.str_index = ptr->len - 1;
	} else {
	    for (i = 0; i <= ptr->len; i++) {
		XTextExtents(messageFont, ptr->txt, i,
			    &direction_return,
			    &font_ascent_return, &font_descent_return,
			    &overall_return);
		if (overall_return.width >= c2.x) {
		    break;
		}
		c2.str_index = i; /* get maximum implicitly */
	    }
	}

	/*
	 * `c1' ~ `c2':
	 * the cut doesn't really include a character:
	 * - cutting from the end of a line to the beginning of the next
	 * - or different pixels but the same character
	 */
	if (c1.y == c2.y
	    && (c1.str_index > c2.str_index || (c1.str_index == c2.str_index
						&& c1.x_off == c2.x_off))) {
	    Add_pending_messages();
	    Selection_set_state();
	    return;
	}

	/*
	 * `plug-in':
	 * don't include the last character (if explicitly clicked on)
	 */
	if (c2.str_index == 0) {
	    if (c1.y == c2.y) {
		/* it's possible */
		Add_pending_messages();
		Selection_set_state();
		return;
	    } else {
		/*
		 * the last character pointed at is the first on a line,
		 * don't include
		 */
		c2.y--;
		c2.x_off = 1;
		c2.str_index = TalkMsg[c2.y]->len - 1;
		if (c1.y == c2.y && c1.str_index >= c2.str_index
		    && c1.x_off == 1) {
		    Add_pending_messages();
		    Selection_set_state();
		    return;
		}
	    }
	} else if (c2.str_index == TalkMsg[c2.y]->len) {
		c2.str_index = TalkMsg[c2.y]->len - 1;
		c2.x_off = 0;
	} else if (c2.str_index > 0 && c2.x_off == 0) {
	    /* c2 is not the first on the line and a nl isn't included */
	    c2.str_index--;
	}
	if (c1.str_index == TalkMsg[c1.y]->len)
	    c1.str_index = TalkMsg[c1.y]->len - 1;

	/*
	 * set the globals
	 */
	selection.txt_size = MAX_MSGS * MSG_LEN;
	selection.txt = (char *)malloc(selection.txt_size);
	if (selection.txt == NULL) {
	    error("No memory for Selection");
	    return;
        }
	selection.draw.x1 = c1.str_index;
	selection.draw.x2 = c2.str_index;
	selection.draw.y1 = c1.y;
	selection.draw.y2 = c2.y;

	current_line = TALK_MSG_SCREENPOS(last_msg_index, selection.draw.y1);

	/* how to walk through the messages */
	if (BIT(instruments, SHOW_REVERSE_SCROLL)) {
	    next = -1;
	} else {
	    next = +1;
	}

	/* fetch the first line */
	strlcpy(cut_str, TalkMsg[current_line]->txt, sizeof(cut_str));
	cut_str_len = TalkMsg[current_line]->len;
	current_line += next;

	if (selection.draw.y1 == selection.draw.y2) {
	/* ...it's the only line */
	    strncpy(selection.txt, &cut_str[selection.draw.x1],
		    selection.draw.x2 - selection.draw.x1 + 1);
	    selection.txt[selection.draw.x2 - selection.draw.x1 + 1] = '\0';
	    cut_str[0] = '\0';
	    if (c2.x_off == 1) {
		strlcat(selection.txt, "\n", selection.txt_size);
	    }
	} else {
	    /* ...several lines */
	    strncpy(selection.txt, &cut_str[selection.draw.x1],
		    cut_str_len - selection.draw.x1);
	    selection.txt[cut_str_len - selection.draw.x1] = '\0';
	    strlcat(selection.txt, "\n", selection.txt_size);

	    /* whole lines themselves only if there are >= 3 lines */
	    for (i = selection.draw.y1 + 1; i < selection.draw.y2; i++) {
		strlcpy(cut_str, TalkMsg[current_line]->txt, sizeof(cut_str));
		cut_str_len = TalkMsg[current_line]->len;
		current_line += next;
		strlcat(selection.txt, cut_str, selection.txt_size);
		strlcat(selection.txt, "\n", selection.txt_size);
	    }

	    /* the last line */
	    strlcpy(cut_str, TalkMsg[current_line]->txt, sizeof(cut_str));
	    cut_str_len = TalkMsg[current_line]->len;
	    current_line += next;
	    strncat(selection.txt, cut_str, selection.draw.x2 + 1);
	    if (c2.x_off == 1) {
		strlcat(selection.txt, "\n", selection.txt_size);
	    }
	} /* more than one line */

	selection.len = strlen(selection.txt);

	/*
	 * store in `cut buffer',
	 * usually a selection request is served by the event in xevent.c.
	 * We get that event as we own the `primary' from now on.
	 * draw the selection emphasized from now on
	 */
	XSetSelectionOwner(dpy, XA_PRIMARY, draw, CurrentTime);
	XStoreBytes(dpy, selection.txt, selection.len);
	selection.draw.state = SEL_EMPHASIZED;
	selection.talk.state = SEL_SELECTED;
	Talk_refresh();
	Clear_talk_selection();
	Add_pending_messages();
	return;

    /* ButtonRelease */
    } else {
	return; /* neither ButtonPress nor ButtonRelease ? */
    }
}

void Talk_reverse_cut(void)
{
    /*
     * think twice: it can't work (yet) without hacking all the
     * c&p stuff even more. thus only unemphasize:
     */
    if (selectionAndHistory && selection.draw.state == SEL_EMPHASIZED) {
	selection.draw.state = SEL_SELECTED;
    }
}
