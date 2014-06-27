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
