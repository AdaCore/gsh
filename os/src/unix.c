/****************************************************************************
 *                                                                          *
 *                                 G S H                                    *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                      Copyright (C) 2011-2019, AdaCore                    *
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
#include <sys/stat.h>

#include <dirent.h>

#include <unistd.h>
#include <stdlib.h>
#include <string.h>

#include <stdio.h>
#include <glob.h>
#include <regex.h>
#include <fnmatch.h>
#include <sys/fcntl.h>
#include <spawn.h>

#if defined(__APPLE__)
#include <copyfile.h>
#define open64 open
#endif

#include "gsh.h"


int
gsh_fnmatch(const char *pattern, const char *string)
{
  return fnmatch(pattern, string, 0);
}


int
__gsh_waitpid (void *h)
{
  int status;
  pid_t pid = (pid_t) (long) h;
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
__gsh_no_block_spawn (char *args[], char *cwd, char *env[],
		      int pstdin, int pstdout, int pstderr,
                      int priority)
{
  pid_t pid;
  char path[4097];
  char **cursor;
  int result;
  int unix_priority;

  char *new_args[4097];
  int index = 0;

  posix_spawn_file_actions_t actions;
  posix_spawn_file_actions_init (&actions);

  if (pstdin != 0)
    {
      posix_spawn_file_actions_adddup2(&actions, pstdin, 0);
      posix_spawn_file_actions_addclose(&actions, pstdin);
    }
  if (pstdout != 1)
    {
      posix_spawn_file_actions_adddup2(&actions, pstdout, 1);
      posix_spawn_file_actions_addclose(&actions, pstdout);
    }
  if (pstderr != 2)
    {
      posix_spawn_file_actions_adddup2(&actions, pstderr, 2);
      posix_spawn_file_actions_addclose(&actions, pstderr);
    }

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
  result = posix_spawn(&pid, args[0], &actions, NULL, args, env);
  if (result == 0) {
    if (priority == P_IDLE) {
      unix_priority = 19;
    } else if (priority == P_BELOW_NORMAL) {
      unix_priority = 10;
    } else if (priority == P_NORMAL) {
      unix_priority = 0;
    } else if (priority == P_ABOVE_NORMAL) {
      unix_priority = -10;
    } else if (priority == P_HIGH) {
      unix_priority = -20;
    } else {
      unix_priority = 20;
    }

    if (unix_priority != 20) {
      setpriority(PRIO_PROCESS, pid, unix_priority);
    }

  }
  chdir(path);
  return (void *) (long) pid;
}

int
__gsh_open(char *path, int kind)
{
  int fd;
  int mode;
  int perm;

  if (kind > READ_MODE)
    {
      mode = O_WRONLY | O_CREAT | O_CLOEXEC;
      if (kind == APPEND_MODE)
	{
	  mode = mode | O_APPEND;
	}
      perm = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
    }
  else
    {
      perm = S_IRUSR | S_IRGRP | S_IROTH;
      mode = O_RDONLY | O_CLOEXEC;
    }

  fd = open64 (path, mode, perm);
  return fd < 0 ? -1 : fd;
}

unsigned long
__gsh_unlink (char *path)
{
  return (unsigned long) unlink(path);
}

struct gsh_file_attributes
__gsh_file_information(char *path)
{
  struct gsh_file_attributes result;
  struct stat mystat;
  int status;

  status = stat (path, &mystat);

  if (status != 0) {

    result.error = 1;
    result.exists = 0;
    result.readable = 0;
    result.writable = 0;
    result.executable = 0;
    result.symbolic_link = 0;
    result.regular = 0;
    result.directory = 0;
    result.stamp = 0;
    result.length = 0;
    return result;
  }

  result.error = 0;
  result.exists = 1;
  result.readable = (mystat.st_mode & S_IRUSR) != 0;
  result.writable = (mystat.st_mode & S_IWUSR) != 0;
  result.executable = (mystat.st_mode & S_IXUSR) != 0;
  result.symbolic_link = S_ISLNK(mystat.st_mode);
  result.regular = S_ISREG(mystat.st_mode);
  result.directory = S_ISDIR(mystat.st_mode);
  result.stamp = mystat.st_mtime;
  result.length = mystat.st_size;

  return result;

}

void *
__gsh_open_directory (char *path)

{
  return opendir (path);
}

void
__gsh_close_directory (void *handle)
{
  closedir (handle);
}

struct gsh_dir_entry
__gsh_next_entry (void *handle)
{
  struct gsh_dir_entry result;
  struct dirent *dirent = (struct dirent *) readdir (handle);

  if (dirent == NULL) {
    result.fi.error = 1;
    result.fi.exists = 0;
    result.fi.readable = 0;
    result.fi.writable = 0;
    result.fi.executable = 0;
    result.fi.symbolic_link = 0;
    result.fi.regular = 0;
    result.fi.directory = 0;
    result.fi.stamp = 0;
    result.fi.length = 0;
    strcpy(result.name, "");
    return result;
  }

  strcpy (result.name, dirent->d_name);
  result.fi.error = 0;
  result.fi.exists = 1;
  result.fi.readable = 0;
  result.fi.writable = 0;
  result.fi.executable = 0;
  result.fi.symbolic_link = dirent->d_type == DT_LNK;
  result.fi.regular = dirent->d_type == DT_REG;
  result.fi.directory = dirent->d_type == DT_DIR;
  result.fi.stamp = 0;
  result.fi.length = 0;
  return result;
}

unsigned long
__gsh_copy_file(char *source,
		            char *target,
		            char fail_if_exists,
		            char preserve_attributes)
{
  int result = 0;

  /* Here we use kernel-space copying for performance reasons. Linux version
   * should be implemented. ??? */

#if defined(__APPLE__)
    copyfile_flags_t flags;
    if (fail_if_exists != 0)
    {
      flags = COPYFILE_EXCL;
    } else {
      flags = COPYFILE_UNLINK;
    }
    if (preserve_attributes != 0)
    {
      flags = flags | COPYFILE_ALL;
    } else {
      flags = flags | COPYFILE_DATA;
    }

    result = copyfile(source, target, NULL, flags);
#endif
  return result;
}

int
__gsh_is_console (int fd)
{
    return isatty (fd);
}

unsigned long
__gsh_mv (char *source,
                          char *target,
                                    char overwrite)
{
        return 1;
}
#endif
