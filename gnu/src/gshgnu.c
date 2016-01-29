#include <stdio.h>
#include <glob.h>
#include <regex.h>
#include <fnmatch.h>

int
gsh_fnmatch(const char *pattern, const char *string, int flags)
{
  return posix_fnmatch(pattern, string, flags);
}

int
gsh_regcomp(regex_t *_Restrict_ __preg,
            const char *_Restrict_ __pattern,
            int __cflags)
{
  return rpl_regcomp(__preg, __pattern, __cflags);
}
