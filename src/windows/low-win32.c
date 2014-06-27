#include <_mingw.h>
#include <stdio.h>

#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0501
#endif

/* For C runtime */
#define _UNICODE
/* For Win32 API */
#define UNICODE

#include <tchar.h>
#include <windows.h>
#include <process.h>
#include <signal.h>
#include <io.h>


extern UINT CurrentCodePage;
#define S2WSC(wstr,str,len) \
   MultiByteToWideChar (CurrentCodePage,0,str,-1,wstr,len)

long
__gsh_getpid (void)
{
  return (long) getpid ();
}

int
__gsh_waitpid (HANDLE h)
{
  DWORD exitcode = 1;
  DWORD res;

  if (h != NULL)
    {
      res = WaitForSingleObject (h, INFINITE);
      GetExitCodeProcess (h, &exitcode);
      CloseHandle (h);
    }

  return (int) exitcode;
}

void *ablock (char *args[])
{ char *aresult;
  int asize = 1;
  int k = 0;
  int n = 0;

  while (args[k])
    {
      asize += strlen (args[k]) + 1;
      k++;
    }

  aresult = (char *) malloc (asize);

  k = 0;
  while (args[k])
    {
      strcpy (aresult + n, args[k]);
      n += strlen (args[k]) + 1;
      k++;
    }
  aresult[asize - 2] = 0;
  aresult[asize - 1] = 0;

  return (void *) aresult;
}

static TCHAR *
wjoin (char *args[])
{
  char *aresult;
  int  asize = 1;
  int  k = 0;
  TCHAR *wresult;
  int wsize = 0;

  while (args[k])
    {
      asize += strlen (args[k]) + 1;
      k++;
    }

  aresult = (char *) malloc (asize);
  aresult[0] = 0;
  k = 0;
  while (args[k])
    {
      strcat (aresult, 	args[k]);
      strcat (aresult, " ");
      k++;
    }

  wsize = asize * 2;
  wresult = (TCHAR *) malloc (wsize);

  S2WSC (wresult, aresult, wsize);

  free (aresult);
  return wresult;
}

int
__gsh_set_close_on_exec (int fd,
                          int close_on_exec_p)
{

  HANDLE h = (HANDLE) _get_osfhandle (fd);
  if (h == (HANDLE) -1)
    return -1;
  if (close_on_exec_p)
    return ! SetHandleInformation (h, HANDLE_FLAG_INHERIT, 0);
  return ! SetHandleInformation (h, HANDLE_FLAG_INHERIT,
    HANDLE_FLAG_INHERIT);

}

HANDLE
__gsh_no_block_spawn (char *args[], char *cwd, char *env[])
{
  BOOL result;
  STARTUPINFO SI;
  PROCESS_INFORMATION PI;
  SECURITY_ATTRIBUTES SA;
  TCHAR *wcommand;
  TCHAR *wcwd = NULL;
  void *block_env = NULL;

  /* Startup info. */
  SI.cb          = sizeof (STARTUPINFO);
  SI.lpReserved  = NULL;
  SI.lpReserved2 = NULL;
  SI.lpDesktop   = NULL;
  SI.cbReserved2 = 0;
  SI.lpTitle     = NULL;
  SI.dwFlags     = 0;
  SI.wShowWindow = SW_HIDE;

  /* Security attributes. */
  SA.nLength = sizeof (SECURITY_ATTRIBUTES);
  SA.bInheritHandle = TRUE;
  SA.lpSecurityDescriptor = NULL;

  /* Prepare the command string. */
  wcommand = wjoin (args);

  /* Then the environment block */
  if (env != NULL)
    block_env = ablock (env);

  /* Convert dir to widestrig */
  if (cwd != NULL)
    {
      wcwd = (TCHAR *) malloc (strlen (cwd) * 2 + 2);
      S2WSC (wcwd, cwd, strlen (cwd) * 2 + 2);
    }

  result = CreateProcess
      (NULL, wcommand, &SA, NULL, TRUE,
       GetPriorityClass (GetCurrentProcess()), block_env, wcwd, &SI, &PI);

  free (block_env);
  free (wcommand);
  free (wcwd);

  if (result == TRUE)
    {
      CloseHandle (PI.hThread);
      return PI.hProcess;
    }
  else
    {
      printf ("%d\n", GetLastError ());
      return NULL;

    }
}
