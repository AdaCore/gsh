/****************************************************************************
 *                                                                          *
 *                                 G S H                                    *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                      Copyright (C) 2011-2014, AdaCore                    *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, *
 * Boston, MA 02110-1301, USA.                                              *
 *                                                                          *
 ****************************************************************************/

#include "stdlib.h"
#include "stdio.h"
#include "strings.h"

extern char *readline (const char *);
extern void add_history (const char *);

/* Each of the files below is a minimal implementation of the standard
   termcap function with the same name, suitable for use in a Windows
   console window.  */

char* internal_readline (const char *prompt)
{
  char *result = readline(prompt);
  if (strlen(result) > 0)
    add_history(result);
  return result;
}

int
tgetent (char *buffer, char *termtype)
{
  return -1;
}

int
tgetnum (char *name)
{
  return -1;
}

int
tgetflag (char *name)
{
  return -1;
}

char *
tgetstr (char *name, char **area)
{
  return NULL;
}

int
tputs (char *string, int nlines, int (*outfun) ())
{
  while (*string)
    outfun (*string++);

  return 0;
}

char *
tgoto (const char *cap, int col, int row)
{
  return NULL;
}
