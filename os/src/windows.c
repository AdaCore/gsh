/****************************************************************************
 *                                                                          *
 *                                 G S H                                    *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                      Copyright (C) 2011-2016, AdaCore                    *
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

#ifdef _WIN32

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
#include <subauth.h>
#include <fcntl.h>
#include <sys/stat.h>
#include "gsh.h"

#define S2WSC(wstr,str,len) \
   MultiByteToWideChar (CP_UTF8, 0, str, -1, wstr, len)

static DWORD windows_bid = 0;

static const DWORD WINDOWS_XP_BID = 2600;


long
__gsh_getpid (void)
{
  return (long) getpid ();
}

int
__gsh_open (char *path, int kind)
{
  int fd;
  int mode;
  int perm;
  TCHAR wpath[32768];

  if (kind > READ_MODE)
    {
      mode = O_WRONLY | O_CREAT | O_BINARY | O_NOINHERIT;
      if (kind == APPEND_MODE)
	{
	  mode = mode | O_APPEND;
	}
      perm = S_IWRITE;
    }
  else
    {
      perm = S_IREAD;
      mode = O_RDONLY | O_BINARY | O_NOINHERIT;
    }

  S2WSC (wpath, path, 32768);
  fd = _topen (wpath, mode, perm);

  /* Even if file is opened in append mode, file position should be set to the
     end of file manually. Indeed if the current process does not do any write
     and pass the file descriptor to a child process, then the child process
     will start writing at the beginning of the file.  */
  if (kind == APPEND_MODE)
  {
    _lseek (fd, 0, SEEK_END);
  }
  return fd < 0 ? -1 : fd;
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

int
__gsh_is_console (int fd)
{
  return _isatty (fd);
}

HANDLE
__gsh_no_block_spawn (char *args[], char *cwd, char *env[],
		      int pstdin, int pstdout, int pstderr)
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
  SI.lpDesktop   = NULL;
  SI.lpTitle     = NULL;
  SI.dwFlags     = STARTF_USESTDHANDLES;
  SI.wShowWindow = SW_HIDE;
  SI.cbReserved2 = 0;
  SI.lpReserved2 = NULL;
  SI.hStdInput   = (HANDLE) _get_osfhandle (pstdin);
  SI.hStdOutput  = (HANDLE) _get_osfhandle (pstdout);
  SI.hStdError   = (HANDLE) _get_osfhandle (pstderr);

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

void *
__gsh_open_directory (char *path)

{
  /* Initialize the UNICODE string that contains the file name using native
      representation.  */

  unsigned int len = strlen(path);

  WCHAR wname[len + 1];
  WCHAR wname2[len + 1 + 4];
  UNICODE_STRING name;
  NTSTATUS  result;
  HANDLE handle;

  S2WSC (wname, path, len + 1);
  _tcscpy(wname2, L"\\??\\");
  _tcscat(wname2, wname);

  name.Length = (len + 4) * sizeof (WCHAR);
  name.MaximumLength = name.Length;
  name.Buffer = wname2;

   result = __gsh_u_open_directory (name, &handle);
   if (NT_SUCCESS(result))
      return handle;

   return NULL;
}


struct gsh_file_attributes
__gsh_file_information(char *path)
{
  struct gsh_file_attributes result;
  HANDLE fd = __gsh_open_directory (path);

  BY_HANDLE_FILE_INFORMATION fi;
  BOOL status;

  if (fd == NULL)
    {
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

  status = GetFileInformationByHandle (fd, &fi);
  CloseHandle(fd);

  result.error = 0;
  result.exists = 1;
  result.readable = 1;
  if (~fi.dwFileAttributes & FILE_ATTRIBUTE_READONLY)
    {
      result.writable = 1;
    }
  else
    {
      result.writable = 0;
    }

  result.executable = 1;

  if (fi.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT)
    {
      result.symbolic_link = 1;
    }
  else
    {
      result.symbolic_link = 0;
    }

  if (fi.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY)
    {
      result.directory = 1;
    }
  else
    {
      result.directory = 0;
    }

  if (result.directory == 0 && result.symbolic_link == 0)
    {
      result.regular = 1;
    }
  else
    {
      result.regular = 0;
    }

  result.stamp = ((long long) fi.ftLastWriteTime.dwHighDateTime) << 32;
  result.stamp = result.stamp + (long long) fi.ftLastWriteTime.dwLowDateTime;
  result.length = ((long long) fi.nFileSizeHigh) << 32;
  result.length = result.length + (long long) fi.nFileSizeLow;

  return result;
}


void
__gsh_close_directory (void *handle)
{
  CloseHandle ((HANDLE) handle);
}


typedef struct {
   NTSTATUS last_error_code;
   ULONG    debug;
} UNLINK_RESULT;

extern UNLINK_RESULT
safe_unlink (UNICODE_STRING name);

unsigned long
__gsh_unlink (char *path)
{
  /* Initialize the UNICODE string that contains the file name using native
      representation.  */

  unsigned int len = strlen(path);
  WCHAR wname[len + 1];
  WCHAR wname2[len + 1 + 4];
  UNICODE_STRING name;
  UNLINK_RESULT  result;

  S2WSC (wname, path, len + 1);
  _tcscpy(wname2, L"\\??\\");
  _tcscat(wname2, wname);

  name.Length = (len + 4) * sizeof(WCHAR);
  name.MaximumLength = name.Length;
  name.Buffer = wname2;

   result = safe_unlink(name);
   if (NT_SUCCESS(result.last_error_code))
      return 0;

   return result.last_error_code;
}

unsigned long
__gsh_copy_file (char *source,
		 char *target,
		 char fail_if_exists,
		 char preserve_attributes)
{

  unsigned int slen = strlen(source);
  unsigned int tlen = strlen(target);

  /* Ensure we have the Windows build number */
  if (windows_bid == 0)
    {
      windows_bid = (DWORD) (HIWORD (GetVersion()));
    }

  BOOL result;
  WCHAR wsource[slen + 1];
  WCHAR wsource2[slen + 1 + 4];
  WCHAR wtarget[tlen + 1];
  WCHAR wtarget2[tlen + 1 + 4];
  SECURITY_ATTRIBUTES SA;
  HANDLE h;
  FILETIME ft;
  SYSTEMTIME st;

  /* create source and target filenames in UNICODE format).  */
  S2WSC (wsource, source, slen + 1);
  S2WSC (wtarget, target, tlen + 1);

  /* On Windows XP it seems that CopyFile does not support long paths starting
     with \?\ so use the regular path names which limit the path size to
     262.  */
  if (windows_bid > WINDOWS_XP_BID)
    {
      _tcscpy (wsource2, L"\\??\\");
      _tcscpy (wtarget2, L"\\??\\");
      _tcscat (wsource2, wsource);
      _tcscat (wtarget2, wtarget);
    }
  else
    {
      _tcscpy(wsource2, wsource);
      _tcscpy(wtarget2, wtarget);
    }

  result = CopyFile (wsource2, wtarget2, fail_if_exists != 0);

  if (result == 0)
    {
      return (unsigned long) GetLastError();
    }

  if (preserve_attributes == 0)
    {
      /* By default windows keep attributes. So if preserve_attribute is FALSE
	 then reset the timestamps. */
      SA.nLength = sizeof (SECURITY_ATTRIBUTES);
      SA.bInheritHandle = FALSE;
      GetSystemTime (&st);
      SystemTimeToFileTime (&st, &ft);

      SA.lpSecurityDescriptor = NULL;
      h = CreateFile (wtarget2, GENERIC_WRITE, FILE_SHARE_READ,
		      &SA, OPEN_EXISTING,  FILE_ATTRIBUTE_NORMAL,
		      NULL);
      SetFileTime (h, &ft, &ft, &ft);
      CloseHandle (h);
    }
  return 0;
}

#endif
