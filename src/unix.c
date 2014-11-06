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

#ifndef _WIN32
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdlib.h>

#include <stdio.h>
#include <glob.h>
#include <regex.h>
#include <fnmatch.h>
#include <sys/fcntl.h>

int
gsh_fnmatch(const char *pattern, const char *string)
{
  return fnmatch(pattern, string, 0);
}


int
__gsh_waitpid (void *h)
{
  int status;
  pid_t pid = (pid_t) h;
  pid_t finished;
  finished = waitpid (pid, &status, 0);

  if (finished != pid || WIFEXITED (status) == 0)
    return -1;

  return WEXITSTATUS (status);
}

int
__gsh_set_close_on_exec (int fd,
			 int close_on_exec_p)
{
  int flags = fcntl (fd, F_GETFD, 0);
  if (flags < 0)
    return flags;
  if (close_on_exec_p)
    flags |= FD_CLOEXEC;
  else
    flags &= ~FD_CLOEXEC;
  return fcntl (fd, F_SETFD, flags);

}

void *
__gsh_no_block_spawn (char *args[], char *cwd, char *env[])
{
  pid_t pid;
  char path[4097];
  char **cursor;
  int result;

  char *new_args[4097];
  int index = 0;

  cursor = args;
  while (cursor[0] != NULL)
    {
      new_args[index] = cursor[0];
      cursor++;
      index ++;
    }
  new_args[index] = NULL;
  getcwd(path, 4097);
  chdir(cwd);
  result = posix_spawn(&pid, args[0], NULL, NULL, args, env);
  chdir(path);
  return (void *) pid;
}

unsigned long
__gsh_unlink (char *path)
{
  return (unsigned long) unlink(path);
}

#endif
