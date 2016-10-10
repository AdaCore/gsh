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

#include<windows.h>
#include<subauth.h>

NTSTATUS
__gsh_kernel_open_directory (UNICODE_STRING name, HANDLE *handle);

NTSTATUS
__gsh_kernel_move (UNICODE_STRING u_source,
		   UNICODE_STRING u_target,
		   char overwrite);

struct gsh_dir_entry
__gsh_kernel_next_entry (void *handle);

typedef struct {
   NTSTATUS last_error_code;
   ULONG    debug;
} UNLINK_RESULT;

UNLINK_RESULT
__gsh_kernel_safe_unlink (UNICODE_STRING name);
