#include <stdio.h>
#include <glob.h>
#include <regex.h>
#include <fnmatch.h>

int
gsh_fnmatch(const char *pattern, const char *string)
{
  return posix_fnmatch(pattern, string, 0);
}

