/****************************************************************************
 *                                                                          *
 *                                 G S H                                    *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *                      Copyright (C) 2011-2015, AdaCore                    *
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

#include "gsh.h"

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

struct gsh_file_attributes
__gsh_file_information(char *path)
{
  struct gsh_file_attributes result;
  HANDLE fd;
  BY_HANDLE_FILE_INFORMATION fi;
  BOOL status;
  fd = CreateFileA(path, 0,
		  FILE_SHARE_READ | FILE_SHARE_WRITE | FILE_SHARE_DELETE,
		  NULL,
		  OPEN_EXISTING,
		  FILE_ATTRIBUTE_NORMAL,
		  NULL);
  if (fd == INVALID_HANDLE_VALUE)
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
  result.writable = ~fi.dwFileAttributes & FILE_ATTRIBUTE_READONLY;
  result.executable = 1;

  result.symbolic_link = fi.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT;
  result.directory = fi.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY;
  result.regular = ~result.directory & ~result.symbolic_link;

  result.stamp = ((long long) fi.ftLastWriteTime.dwHighDateTime) << 32;
  result.stamp = result.stamp + (long long) fi.ftLastWriteTime.dwLowDateTime;
  result.length = ((long long) fi.nFileSizeHigh) << 32;
  result.length = result.length + (long long) fi.nFileSizeLow;

  return result;
}

void *
__gsh_open_directory (char *path)

{
  HANDLE result;
  char  extended_path[MAX_PATH];
  WIN32_FIND_DATAA data;

  strcpy(extended_path, path);
  strcat(extended_path, "/*.*");

  result = FindFirstFileA (extended_path, &data);
  if (result == INVALID_HANDLE_VALUE)
    {
      return NULL;
    }
}

void
__gsh_close_directory (void *handle)
{
  CloseHandle ((HANDLE) handle);
}

struct gsh_dir_entry
__gsh_next_entry (void *handle)
{
  struct gsh_dir_entry result;
  WIN32_FIND_DATAA data;
  BOOL status;

  status = FindNextFileA( (HANDLE) handle, &data);
  if (status)
    {
      result.fi.error = 0;
      result.fi.exists = 1;
      result.fi.readable = 1;
      result.fi.writable = ~data.dwFileAttributes & FILE_ATTRIBUTE_READONLY;
      result.fi.executable = 1;

      result.fi.symbolic_link = data.dwFileAttributes & FILE_ATTRIBUTE_REPARSE_POINT;
      result.fi.directory = data.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY;
      result.fi.regular = ~result.fi.directory & ~result.fi.symbolic_link;

      result.fi.stamp = ((long long) data.ftLastWriteTime.dwHighDateTime) << 32;
      result.fi.stamp = result.fi.stamp + (long long) data.ftLastWriteTime.dwLowDateTime;
      result.fi.length = ((long long) data.nFileSizeHigh) << 32;
      result.fi.length = result.fi.length + (long long) data.nFileSizeLow;

      strncpy(result.name, data.cFileName, 512);
    }
  else
    {
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
    }

  return result;
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
  _tcscpy(wsource2, L"\\??\\");
  _tcscat(wsource2, wsource);

  S2WSC (wtarget, target, tlen + 1);
  _tcscpy (wtarget2, L"\\??\\");
  _tcscat (wtarget2, wtarget);

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
