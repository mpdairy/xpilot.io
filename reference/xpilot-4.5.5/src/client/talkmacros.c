/* $Id: talkmacros.c,v 5.5 2002/01/29 21:02:10 kimiko Exp $
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


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#ifdef _WINDOWS
# include "NT/winclient.h"
# include "NT/winNet.h"
# include "NT/winAudio.h"
# include "NT/winX.h"
# include "NT/winXThread.h"
# include "NT/winXXPilot.h"
#endif

#include "version.h"
#include "config.h"
#include "const.h"
#include "client.h"
#include "netclient.h"		/* Net_talk(char *str) */
#include "setup.h"
#include "bit.h"
#include "error.h"
#include "protoclient.h"
#include "portability.h"
#include "talk.h"
#include "rules.h"		/* TEAM_PLAY, LIMITED_LIVES */
#include "commonproto.h"


char talkmacros_version[] = VERSION;

/*
 * Abandon your hope, all you enter here
 */

/* The final string, sent to the server */
static char	final_str[MAX_CHARS];

extern int	eyesId;		/* Player we get frame updates for */
extern short	snooping;	/* Are we snooping on someone else? */

/* exported (to xevent.c) */
int		Talk_macro(char *str);

#define MSG_PARSED_FIELD_LEN      20

/*
 * Returns a pointer to the first character after the fields
 */
static char *Talk_macro_fields_info (char *buf, int *n_fields)
{
    int			end_found = 0, level = 0;

    *n_fields = 0;
    while (!end_found)
    {
	switch (*buf)
	{
	case TALK_FAST_START_DELIMITER:
	    if (level++ == 0)
		(*n_fields)++;
	    break;
	case TALK_FAST_MIDDLE_DELIMITER:
	    if (level == 1)
		(*n_fields)++;
	    break;
	case TALK_FAST_END_DELIMITER:
	    level--;
	    if (level == 0)
		end_found = 1;
	    else if (level < 0)
		return NULL;
	    break;
	case '\0':
	    return NULL;
	    break;
	default:
	    break;
	}
	buf++;
    }
    return buf;
}


/* Returns a string pointer to the wanted_field
 * This pointer must be freed after using it
 */
static char *Talk_macro_get_field (char *buf, int wanted_field)
{
    int  finished = 0, level = 0, field = 0, len;
    char *field_ptr, *start_ptr = NULL, *end_ptr = NULL;

    while (!finished)
    {
	switch (*buf)
	{
	case TALK_FAST_START_DELIMITER:
	    if (level == 0)
	    {
		field++;
		if (field == wanted_field)
		    start_ptr = buf + 1;
	    }
	    level++;
	    break;
	case TALK_FAST_MIDDLE_DELIMITER:
	    if (level == 1)
	    {
		field++;
		if (field == wanted_field)
		    start_ptr = buf + 1;
		else if (field == wanted_field + 1)
		{
		    end_ptr = buf;
		    finished = 1;
		}
	    }
	    break;
	case TALK_FAST_END_DELIMITER:
	    level--;
	    if (level == 0)
	    {
		if (field == wanted_field)
		    end_ptr = buf;
		finished = 1;
	    }
	    else if (level < 0)
		return NULL;
	    break;
	case '\0':
	    return NULL;
	    break;
	default:
	    break;
	}
	buf++;

    }
    len = end_ptr - start_ptr;
    if ((field_ptr = (char *) malloc (len + 1)) == NULL) {
	error("Can't allocate memory for talk macro");
	return NULL;
    }
    strncpy (field_ptr, start_ptr, len);
    field_ptr[len] = '\0';
    return field_ptr;
}


static int Talk_macro_parse_mesg(char *outbuf, char *inbuf, long pos, long max)
{
    FILE	*fp;
    char	c;
    long	fsize;
    int		i;
    int		done = 0;
    int		n_fields;
    char	*tmpptr;
    char	*tmpptr1;
    char	*tmpptr2;
    char	*tmpptr3 = 0;
    char	*nextpos;
    char	*filename;
    other_t	*player = NULL;


    while (!done && (c = *inbuf++) != '\0')
    {
	if (pos >= max - 2)
	{
	    if (outbuf == final_str) /* parsing to the talk buffer */
	    {
		outbuf[pos] = '\0';
		if (Net_talk(outbuf) == -1) {
                    return -1;
		}
		pos = 0;
	    }
	    else {
		break;
	    }
	}

	if (player != NULL) {
	    switch (c)
	    {
	    case 'l':
		if (BIT(Setup->mode, LIMITED_LIVES))
		    outbuf[pos++] = player->life + '0';
		break;
	    case 'n':
		tmpptr = player->name;
		for (i = 0; tmpptr[i] != '\0' && pos < max - 2; ++i)
		    outbuf[pos++] = tmpptr[i];
		break;
	    case 's':
		if (pos < max - 1 - 6) /* short - "-16535" max no of chars */
		    pos += sprintf (outbuf+pos, "%.2f", player->score);
		break;
	    case 't':
		if (BIT(Setup->mode, TEAM_PLAY))
		    outbuf[pos++] = player->team + '0';
		break;
	    default:
		break;
	    }
	    player = NULL;
	} else {
	    switch (c) {
	    case TALK_FAST_SPECIAL_TALK_CHAR:
		if ((c = *inbuf++) == '\0') {
		    done = 1;
		    break;
		}
		switch (c) {
		case '=':  /* String comparison */
		    nextpos = Talk_macro_fields_info (inbuf, &n_fields);
		    if (n_fields < 3 || n_fields > 4 || nextpos == NULL)
			break;
		    /* parse field 1 */
		    if ((tmpptr = Talk_macro_get_field (inbuf, 1)) == NULL) {
			error ("Talk_macro_get_field (1) error!");
			break;
		    }
		    if ((tmpptr1 = (char *)malloc (MSG_PARSED_FIELD_LEN))
			    == NULL) {
			error("Can't allocate memory for talk macro.");
			free(tmpptr); /* successful malloc from before */
			break;
		    }
		    Talk_macro_parse_mesg (tmpptr1, tmpptr, 0,
					    MSG_PARSED_FIELD_LEN);
		    free (tmpptr);
		    /* parse field 2 */
		    if ((tmpptr = Talk_macro_get_field (inbuf, 2)) == NULL) {
			error ("Talk_macro_get_field (2) error!");
			break;
		    }
		    if ((tmpptr2 = (char *)malloc (MSG_PARSED_FIELD_LEN))
			    == NULL) {
			error("Can't allocate memory for talk macro.");
			free (tmpptr); /* successful malloc from before */
			break;
		    }
		    Talk_macro_parse_mesg (tmpptr2, tmpptr, 0, MSG_PARSED_FIELD_LEN);
		    free (tmpptr);
		    if (!strcmp(tmpptr1, tmpptr2)) {
			/* True */
                        if ((tmpptr3 = Talk_macro_get_field (inbuf, 3)) == NULL)
			{
			    error ("Talk_macro_get_field (3) error!");
			    free (tmpptr1);
			    free (tmpptr2);
			    break;
			}
			pos = Talk_macro_parse_mesg (outbuf, tmpptr3, pos, max);
		    }
		    else if (n_fields == 4)
		    {
			/* False */
			if ((tmpptr3 = Talk_macro_get_field (inbuf, 4)) == NULL)
			{
			    error ("Talk_macro_get_field (4) error!");
			    free (tmpptr1);
			    free (tmpptr2);
			    break;
			}
			pos = Talk_macro_parse_mesg (outbuf, tmpptr3, pos, max);
		    }
		    inbuf = nextpos;
		    free (tmpptr1);
		    free (tmpptr2);
		    free (tmpptr3);
		    break;
		case 'f':
		    nextpos = Talk_macro_fields_info (inbuf, &n_fields);
		    if (n_fields != 1 || nextpos == NULL) {
			break;
		    }
		    if ((tmpptr = Talk_macro_get_field (inbuf, 1)) == NULL) {
			error ("Talk_macro_get_field error!");
			break;
		    }
		    inbuf = nextpos;
		    if ((filename = (char *)malloc (TALK_FAST_MSG_FNLEN))
			    == NULL) {
			error("Can't allocate memory for talk macro.");
			break;
		    }

		    Talk_macro_parse_mesg (filename, tmpptr, 0,
					    TALK_FAST_MSG_FNLEN);
		    free (tmpptr);
		    if ((fp = fopen (filename, "r")) == NULL) {
			error ("Couldn't open file %s", tmpptr);
			free (filename);
			break;
		    }
		    free (filename);

		    /* Get filesize */
		    fseek (fp, 0L, SEEK_END);
		    fsize = ftell (fp);
		    rewind (fp);

		    if ((tmpptr = (char *)malloc(fsize+1)) == NULL) {
			fclose (fp);
			break;
		    }
		    fread (tmpptr, 1, fsize, fp);
		    tmpptr[fsize] = '\0';
		    fclose (fp);
		    pos = Talk_macro_parse_mesg (outbuf, tmpptr, pos, max);
		    free (tmpptr);
		    break;
		case 'h':
		    tmpptr = getenv ("HOME");
		    if (tmpptr != NULL) {
			while (*tmpptr != '\0' && pos < max - 2) {
			    outbuf[pos++] = *tmpptr++;
			}
		    }
		    break;
		case 'r':
		    nextpos = Talk_macro_fields_info (inbuf, &n_fields);
		    if (n_fields <= 0 || nextpos == NULL)
		      break;
		    if ((tmpptr = Talk_macro_get_field (inbuf,
						    randomMT() % n_fields + 1))
			  == NULL) {
			error ("Talk_macro_get_field error (random)");
			break;
		    }
		    inbuf = nextpos;
		    pos = Talk_macro_parse_mesg (outbuf, tmpptr, pos, max);
		    free (tmpptr);
		    break;
		case 'n':
		    outbuf[pos] = '\0';

		    if ( Net_talk(outbuf) == -1 ) {
			return -1;
		    }
		    pos = 0;
		    break;
		case 'l':
                    if ( !snooping ) {
                        if ((player = Other_by_id(lock_id)) == NULL) {
                          pos = 0;
                          done = 1;
			  break;
                        }
                    } else {
                        if ((player = Other_by_id(eyesId)) == NULL) {
                          pos = 0;
                          done = 1;
			  break;
                        }
		    }
                    break;
		case 's':
		    player = self;
		    break;
		case 't':
		    if (BIT(Setup->mode, TEAM_PLAY)) {
		      outbuf[pos++] = self->team + '0';
		    }
		    break;
		case TALK_FAST_SPECIAL_TALK_CHAR:
		    outbuf[pos++] = c;
		    break;
		default:
		    break;
		} /* case TALK_FAST_SPECIAL_TALK_CHAR: switch (c) */
		break;
	    case '\n':
		break;
	    default:
		outbuf[pos++] = c;
		break;
	    } /* (player != NULL) switch(c) */
	}
    }

    outbuf[pos] = '\0';

    return pos;
}


int Talk_macro(char *str)
{
    /* Comment: sizeof str === MAX_CHARS */
    if (str == NULL) {
	return 1;
    }

    if (Talk_macro_parse_mesg (final_str, str, 0L, MAX_CHARS) > 0) {
	Net_talk(final_str);
    }
    return 0;
}


