/* $Id: talk.h,v 5.0 2001/04/07 20:00:58 dik Exp $
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

#ifndef TALK_H
#define TALK_H

#define TALK_FAST_NR_OF_MSGS		20               /* talk macros */
#define TALK_FAST_MSG_SIZE		400
#define TALK_FAST_MSG_FNLEN		100
#define TALK_FAST_START_DELIMITER	'['
#define TALK_FAST_END_DELIMITER		']'
#define TALK_FAST_MIDDLE_DELIMITER	'|'
#define TALK_FAST_SPECIAL_TALK_CHAR	'#'

/*
#define TEAM_PLAY               (1<<8)
#define LIMITED_LIVES           (1<<3)
#define MSG_PARSED_FIELD_LEN      20
*/

/* Talk Macros */
int Talk_macro(char *str);

#endif
