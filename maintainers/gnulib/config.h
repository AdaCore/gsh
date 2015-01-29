/* config.h.  Generated from config.h.in by configure.  */
/* config.h.in.  Generated from configure.ac by autoheader.  */

/* Define to the number of bits in type 'ptrdiff_t'. */
/* #undef BITSIZEOF_PTRDIFF_T */

/* Define to the number of bits in type 'sig_atomic_t'. */
/* #undef BITSIZEOF_SIG_ATOMIC_T */

/* Define to the number of bits in type 'size_t'. */
/* #undef BITSIZEOF_SIZE_T */

/* Define to the number of bits in type 'wchar_t'. */
/* #undef BITSIZEOF_WCHAR_T */

/* Define to the number of bits in type 'wint_t'. */
/* #undef BITSIZEOF_WINT_T */

/* Define to one of `_getb67', `GETB67', `getb67' for Cray-2 and Cray-YMP
   systems. This function is required for `alloca.c' support on those systems.
   */
/* #undef CRAY_STACKSEG_END */

/* Define to 1 if using `alloca.c'. */
/* #undef C_ALLOCA */

/* the name of the file descriptor member of DIR */
/* #undef DIR_FD_MEMBER_NAME */

#ifdef DIR_FD_MEMBER_NAME
# define DIR_TO_FD(Dir_p) ((Dir_p)->DIR_FD_MEMBER_NAME)
#else
# define DIR_TO_FD(Dir_p) -1
#endif


/* Define to 1 if nl_langinfo (YESEXPR) returns a non-empty string. */
/* #undef FUNC_NL_LANGINFO_YESEXPR_WORKS */

/* Define to a C preprocessor expression that evaluates to 1 or 0, depending
   whether the gnulib module malloc-gnu shall be considered present. */
#define GNULIB_MALLOC_GNU 1

/* Define to 1 when the gnulib module btowc should be tested. */
#define GNULIB_TEST_BTOWC 1

/* Define to 1 when the gnulib module closedir should be tested. */
#define GNULIB_TEST_CLOSEDIR 1

/* Define to 1 when the gnulib module dirfd should be tested. */
#define GNULIB_TEST_DIRFD 1

/* Define to 1 when the gnulib module getlogin_r should be tested. */
#define GNULIB_TEST_GETLOGIN_R 1

/* Define to 1 when the gnulib module malloc-posix should be tested. */
#define GNULIB_TEST_MALLOC_POSIX 1

/* Define to 1 when the gnulib module mbrtowc should be tested. */
#define GNULIB_TEST_MBRTOWC 1

/* Define to 1 when the gnulib module mbsinit should be tested. */
#define GNULIB_TEST_MBSINIT 1

/* Define to 1 when the gnulib module mbsrtowcs should be tested. */
#define GNULIB_TEST_MBSRTOWCS 1

/* Define to 1 when the gnulib module mbtowc should be tested. */
#define GNULIB_TEST_MBTOWC 1

/* Define to 1 when the gnulib module memchr should be tested. */
#define GNULIB_TEST_MEMCHR 1

/* Define to 1 when the gnulib module mempcpy should be tested. */
#define GNULIB_TEST_MEMPCPY 1

/* Define to 1 when the gnulib module nl_langinfo should be tested. */
#define GNULIB_TEST_NL_LANGINFO 1

/* Define to 1 when the gnulib module opendir should be tested. */
#define GNULIB_TEST_OPENDIR 1

/* Define to 1 when the gnulib module readdir should be tested. */
#define GNULIB_TEST_READDIR 1

/* Define to 1 when the gnulib module wcrtomb should be tested. */
#define GNULIB_TEST_WCRTOMB 1

/* Define to 1 if you have 'alloca' after including <alloca.h>, a header that
   may be supplied by this distribution. */
#define HAVE_ALLOCA 1

/* Define to 1 if you have <alloca.h> and it should be used (not on Ultrix).
   */
/* #undef HAVE_ALLOCA_H */

/* Define to 1 if you have the <bp-sym.h> header file. */
/* #undef HAVE_BP_SYM_H */

/* Define to 1 if you have the `btowc' function. */
#define HAVE_BTOWC 1

/* Define to 1 if you have the `closedir' function. */
#define HAVE_CLOSEDIR 1

/* Define to 1 if you have the declaration of `dirfd', and to 0 if you don't.
   */
#define HAVE_DECL_DIRFD 0

/* Define to 1 if you have the declaration of `getc_unlocked', and to 0 if you
   don't. */
#define HAVE_DECL_GETC_UNLOCKED 0

/* Define to 1 if you have the declaration of `getlogin', and to 0 if you
   don't. */
#define HAVE_DECL_GETLOGIN 0

/* Define to 1 if you have the declaration of `getlogin_r', and to 0 if you
   don't. */
#define HAVE_DECL_GETLOGIN_R 0

/* Define to 1 if you have the declaration of `isblank', and to 0 if you
   don't. */
#define HAVE_DECL_ISBLANK 1

/* Define to 1 if you have the declaration of `mbrtowc', and to 0 if you
   don't. */
/* #undef HAVE_DECL_MBRTOWC */

/* Define to 1 if you have the declaration of `mbsinit', and to 0 if you
   don't. */
/* #undef HAVE_DECL_MBSINIT */

/* Define to 1 if you have the declaration of `mbsrtowcs', and to 0 if you
   don't. */
/* #undef HAVE_DECL_MBSRTOWCS */

/* Define to 1 if you have the declaration of `strncasecmp', and to 0 if you
   don't. */
#define HAVE_DECL_STRNCASECMP 1

/* Define to 1 if you have the declaration of `towlower', and to 0 if you
   don't. */
/* #undef HAVE_DECL_TOWLOWER */

/* Define to 1 if you have the declaration of `wcrtomb', and to 0 if you
   don't. */
/* #undef HAVE_DECL_WCRTOMB */

/* Define to 1 if you have the <dirent.h> header file. */
#define HAVE_DIRENT_H 1

/* Define to 1 if you have the `dirfd' function. */
/* #undef HAVE_DIRFD */

/* Define to 1 if you have the <features.h> header file. */
/* #undef HAVE_FEATURES_H */

/* Define to 1 if you have the `fstatat' function. */
/* #undef HAVE_FSTATAT */

/* Define to 1 if you have the `getlogin_r' function. */
/* #undef HAVE_GETLOGIN_R */

/* Define to 1 if you have the `getpwnam_r' function. */
/* #undef HAVE_GETPWNAM_R */

/* Define to 1 if you have the <glob.h> header file. */
/* #undef HAVE_GLOB_H */

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the `isblank' function. */
#define HAVE_ISBLANK 1

/* Define to 1 if you have the `iswcntrl' function. */
#define HAVE_ISWCNTRL 1

/* Define to 1 if you have the `iswctype' function. */
#define HAVE_ISWCTYPE 1

/* Define if you have <langinfo.h> and nl_langinfo(CODESET). */
/* #undef HAVE_LANGINFO_CODESET */

/* Define to 1 if you have the <langinfo.h> header file. */
/* #undef HAVE_LANGINFO_H */

/* Define to 1 if you have the <libintl.h> header file. */
/* #undef HAVE_LIBINTL_H */

/* Define to 1 if the system has the type `long long int'. */
#define HAVE_LONG_LONG_INT 1

/* Define to 1 if your system has a GNU libc compatible 'malloc' function, and
   to 0 otherwise. */
#define HAVE_MALLOC_GNU 1

/* Define if the 'malloc' function is POSIX compliant. */
/* #undef HAVE_MALLOC_POSIX */

/* Define to 1 if mmap()'s MAP_ANONYMOUS flag is available after including
   config.h and <sys/mman.h>. */
/* #undef HAVE_MAP_ANONYMOUS */

/* Define to 1 if you have the `mbrtowc' function. */
#define HAVE_MBRTOWC 1

/* Define to 1 if you have the `mbsinit' function. */
#define HAVE_MBSINIT 1

/* Define to 1 if you have the `mbsrtowcs' function. */
#define HAVE_MBSRTOWCS 1

/* Define to 1 if <wchar.h> declares mbstate_t. */
#define HAVE_MBSTATE_T 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `mempcpy' function. */
/* #undef HAVE_MEMPCPY */

/* Define to 1 if you have the `mprotect' function. */
#define HAVE_MPROTECT 1

/* Define to 1 if you have the `nl_langinfo' function. */
/* #undef HAVE_NL_LANGINFO */

/* Define to 1 if you have the `opendir' function. */
#define HAVE_OPENDIR 1

/* Define to 1 if alphasort is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_ALPHASORT */

/* Define to 1 if atoll is declared even after undefining macros. */
#define HAVE_RAW_DECL_ATOLL 1

/* Define to 1 if btowc is declared even after undefining macros. */
#define HAVE_RAW_DECL_BTOWC 1

/* Define to 1 if canonicalize_file_name is declared even after undefining
   macros. */
/* #undef HAVE_RAW_DECL_CANONICALIZE_FILE_NAME */

/* Define to 1 if chdir is declared even after undefining macros. */
#define HAVE_RAW_DECL_CHDIR 1

/* Define to 1 if chown is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_CHOWN */

/* Define to 1 if closedir is declared even after undefining macros. */
#define HAVE_RAW_DECL_CLOSEDIR 1

/* Define to 1 if dirfd is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_DIRFD */

/* Define to 1 if dup is declared even after undefining macros. */
#define HAVE_RAW_DECL_DUP 1

/* Define to 1 if dup2 is declared even after undefining macros. */
#define HAVE_RAW_DECL_DUP2 1

/* Define to 1 if dup3 is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_DUP3 */

/* Define to 1 if endusershell is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_ENDUSERSHELL */

/* Define to 1 if environ is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_ENVIRON */

/* Define to 1 if euidaccess is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_EUIDACCESS */

/* Define to 1 if faccessat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FACCESSAT */

/* Define to 1 if fchdir is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FCHDIR */

/* Define to 1 if fchmodat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FCHMODAT */

/* Define to 1 if fchownat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FCHOWNAT */

/* Define to 1 if fdatasync is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FDATASYNC */

/* Define to 1 if fdopendir is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FDOPENDIR */

/* Define to 1 if ffs is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FFS */

/* Define to 1 if ffsl is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FFSL */

/* Define to 1 if ffsll is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FFSLL */

/* Define to 1 if fstat is declared even after undefining macros. */
#define HAVE_RAW_DECL_FSTAT 1

/* Define to 1 if fstatat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FSTATAT */

/* Define to 1 if fsync is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FSYNC */

/* Define to 1 if ftruncate is declared even after undefining macros. */
#define HAVE_RAW_DECL_FTRUNCATE 1

/* Define to 1 if futimens is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_FUTIMENS */

/* Define to 1 if getcwd is declared even after undefining macros. */
#define HAVE_RAW_DECL_GETCWD 1

/* Define to 1 if getdomainname is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETDOMAINNAME */

/* Define to 1 if getdtablesize is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETDTABLESIZE */

/* Define to 1 if getgroups is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETGROUPS */

/* Define to 1 if gethostname is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETHOSTNAME */

/* Define to 1 if getloadavg is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETLOADAVG */

/* Define to 1 if getlogin is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETLOGIN */

/* Define to 1 if getlogin_r is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETLOGIN_R */

/* Define to 1 if getpagesize is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETPAGESIZE */

/* Define to 1 if getsubopt is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETSUBOPT */

/* Define to 1 if getusershell is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GETUSERSHELL */

/* Define to 1 if grantpt is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GRANTPT */

/* Define to 1 if group_member is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_GROUP_MEMBER */

/* Define to 1 if initstate_r is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_INITSTATE_R */

/* Define to 1 if iswctype is declared even after undefining macros. */
#define HAVE_RAW_DECL_ISWCTYPE 1

/* Define to 1 if lchmod is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_LCHMOD */

/* Define to 1 if lchown is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_LCHOWN */

/* Define to 1 if link is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_LINK */

/* Define to 1 if linkat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_LINKAT */

/* Define to 1 if lseek is declared even after undefining macros. */
#define HAVE_RAW_DECL_LSEEK 1

/* Define to 1 if lstat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_LSTAT */

/* Define to 1 if mbrlen is declared even after undefining macros. */
#define HAVE_RAW_DECL_MBRLEN 1

/* Define to 1 if mbrtowc is declared even after undefining macros. */
#define HAVE_RAW_DECL_MBRTOWC 1

/* Define to 1 if mbsinit is declared even after undefining macros. */
#define HAVE_RAW_DECL_MBSINIT 1

/* Define to 1 if mbsnrtowcs is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MBSNRTOWCS */

/* Define to 1 if mbsrtowcs is declared even after undefining macros. */
#define HAVE_RAW_DECL_MBSRTOWCS 1

/* Define to 1 if memmem is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MEMMEM */

/* Define to 1 if mempcpy is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MEMPCPY */

/* Define to 1 if memrchr is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MEMRCHR */

/* Define to 1 if mkdirat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKDIRAT */

/* Define to 1 if mkdtemp is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKDTEMP */

/* Define to 1 if mkfifo is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKFIFO */

/* Define to 1 if mkfifoat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKFIFOAT */

/* Define to 1 if mknod is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKNOD */

/* Define to 1 if mknodat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKNODAT */

/* Define to 1 if mkostemp is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKOSTEMP */

/* Define to 1 if mkostemps is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKOSTEMPS */

/* Define to 1 if mkstemp is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKSTEMP */

/* Define to 1 if mkstemps is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_MKSTEMPS */

/* Define to 1 if nl_langinfo is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_NL_LANGINFO */

/* Define to 1 if opendir is declared even after undefining macros. */
#define HAVE_RAW_DECL_OPENDIR 1

/* Define to 1 if pipe is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_PIPE */

/* Define to 1 if pipe2 is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_PIPE2 */

/* Define to 1 if posix_openpt is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_POSIX_OPENPT */

/* Define to 1 if pread is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_PREAD */

/* Define to 1 if ptsname is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_PTSNAME */

/* Define to 1 if ptsname_r is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_PTSNAME_R */

/* Define to 1 if pwrite is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_PWRITE */

/* Define to 1 if random_r is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_RANDOM_R */

/* Define to 1 if rawmemchr is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_RAWMEMCHR */

/* Define to 1 if readdir is declared even after undefining macros. */
#define HAVE_RAW_DECL_READDIR 1

/* Define to 1 if readlink is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_READLINK */

/* Define to 1 if readlinkat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_READLINKAT */

/* Define to 1 if realpath is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_REALPATH */

/* Define to 1 if rewinddir is declared even after undefining macros. */
#define HAVE_RAW_DECL_REWINDDIR 1

/* Define to 1 if rmdir is declared even after undefining macros. */
#define HAVE_RAW_DECL_RMDIR 1

/* Define to 1 if rpmatch is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_RPMATCH */

/* Define to 1 if scandir is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_SCANDIR */

/* Define to 1 if setenv is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_SETENV */

/* Define to 1 if setstate_r is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_SETSTATE_R */

/* Define to 1 if setusershell is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_SETUSERSHELL */

/* Define to 1 if sleep is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_SLEEP */

/* Define to 1 if srandom_r is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_SRANDOM_R */

/* Define to 1 if stat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STAT */

/* Define to 1 if stpcpy is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STPCPY */

/* Define to 1 if stpncpy is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STPNCPY */

/* Define to 1 if strcasecmp is declared even after undefining macros. */
#define HAVE_RAW_DECL_STRCASECMP 1

/* Define to 1 if strcasestr is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRCASESTR */

/* Define to 1 if strchrnul is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRCHRNUL */

/* Define to 1 if strdup is declared even after undefining macros. */
#define HAVE_RAW_DECL_STRDUP 1

/* Define to 1 if strerror_r is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRERROR_R */

/* Define to 1 if strncasecmp is declared even after undefining macros. */
#define HAVE_RAW_DECL_STRNCASECMP 1

/* Define to 1 if strncat is declared even after undefining macros. */
#define HAVE_RAW_DECL_STRNCAT 1

/* Define to 1 if strndup is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRNDUP */

/* Define to 1 if strnlen is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRNLEN */

/* Define to 1 if strpbrk is declared even after undefining macros. */
#define HAVE_RAW_DECL_STRPBRK 1

/* Define to 1 if strsep is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRSEP */

/* Define to 1 if strsignal is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRSIGNAL */

/* Define to 1 if strtod is declared even after undefining macros. */
#define HAVE_RAW_DECL_STRTOD 1

/* Define to 1 if strtok_r is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRTOK_R */

/* Define to 1 if strtoll is declared even after undefining macros. */
#define HAVE_RAW_DECL_STRTOLL 1

/* Define to 1 if strtoull is declared even after undefining macros. */
#define HAVE_RAW_DECL_STRTOULL 1

/* Define to 1 if strverscmp is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_STRVERSCMP */

/* Define to 1 if symlink is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_SYMLINK */

/* Define to 1 if symlinkat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_SYMLINKAT */

/* Define to 1 if towctrans is declared even after undefining macros. */
#define HAVE_RAW_DECL_TOWCTRANS 1

/* Define to 1 if ttyname_r is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_TTYNAME_R */

/* Define to 1 if unlink is declared even after undefining macros. */
#define HAVE_RAW_DECL_UNLINK 1

/* Define to 1 if unlinkat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_UNLINKAT */

/* Define to 1 if unlockpt is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_UNLOCKPT */

/* Define to 1 if unsetenv is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_UNSETENV */

/* Define to 1 if usleep is declared even after undefining macros. */
#define HAVE_RAW_DECL_USLEEP 1

/* Define to 1 if utimensat is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_UTIMENSAT */

/* Define to 1 if wcpcpy is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_WCPCPY */

/* Define to 1 if wcpncpy is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_WCPNCPY */

/* Define to 1 if wcrtomb is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCRTOMB 1

/* Define to 1 if wcscasecmp is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_WCSCASECMP */

/* Define to 1 if wcscat is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSCAT 1

/* Define to 1 if wcschr is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSCHR 1

/* Define to 1 if wcscmp is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSCMP 1

/* Define to 1 if wcscoll is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSCOLL 1

/* Define to 1 if wcscpy is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSCPY 1

/* Define to 1 if wcscspn is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSCSPN 1

/* Define to 1 if wcsdup is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSDUP 1

/* Define to 1 if wcslen is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSLEN 1

/* Define to 1 if wcsncasecmp is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_WCSNCASECMP */

/* Define to 1 if wcsncat is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSNCAT 1

/* Define to 1 if wcsncmp is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSNCMP 1

/* Define to 1 if wcsncpy is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSNCPY 1

/* Define to 1 if wcsnlen is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_WCSNLEN */

/* Define to 1 if wcsnrtombs is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_WCSNRTOMBS */

/* Define to 1 if wcspbrk is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSPBRK 1

/* Define to 1 if wcsrchr is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSRCHR 1

/* Define to 1 if wcsrtombs is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSRTOMBS 1

/* Define to 1 if wcsspn is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSSPN 1

/* Define to 1 if wcsstr is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSSTR 1

/* Define to 1 if wcstok is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSTOK 1

/* Define to 1 if wcswidth is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_WCSWIDTH */

/* Define to 1 if wcsxfrm is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCSXFRM 1

/* Define to 1 if wctob is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCTOB 1

/* Define to 1 if wctrans is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCTRANS 1

/* Define to 1 if wctype is declared even after undefining macros. */
#define HAVE_RAW_DECL_WCTYPE 1

/* Define to 1 if wcwidth is declared even after undefining macros. */
/* #undef HAVE_RAW_DECL_WCWIDTH */

/* Define to 1 if wmemchr is declared even after undefining macros. */
#define HAVE_RAW_DECL_WMEMCHR 1

/* Define to 1 if wmemcmp is declared even after undefining macros. */
#define HAVE_RAW_DECL_WMEMCMP 1

/* Define to 1 if wmemcpy is declared even after undefining macros. */
#define HAVE_RAW_DECL_WMEMCPY 1

/* Define to 1 if wmemmove is declared even after undefining macros. */
#define HAVE_RAW_DECL_WMEMMOVE 1

/* Define to 1 if wmemset is declared even after undefining macros. */
#define HAVE_RAW_DECL_WMEMSET 1

/* Define to 1 if _Exit is declared even after undefining macros. */
#define HAVE_RAW_DECL__EXIT 1

/* Define to 1 if you have the `readdir' function. */
#define HAVE_READDIR 1

/* Define to 1 if 'sig_atomic_t' is a signed integer type. */
/* #undef HAVE_SIGNED_SIG_ATOMIC_T */

/* Define to 1 if 'wchar_t' is a signed integer type. */
/* #undef HAVE_SIGNED_WCHAR_T */

/* Define to 1 if 'wint_t' is a signed integer type. */
/* #undef HAVE_SIGNED_WINT_T */

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strcasecmp' function. */
#define HAVE_STRCASECMP 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strncasecmp' function. */
#define HAVE_STRNCASECMP 1

/* Define if there is a member named d_type in the struct describing directory
   headers. */
/* #undef HAVE_STRUCT_DIRENT_D_TYPE */

/* Define to 1 if you have the `symlink' function. */
/* #undef HAVE_SYMLINK */

/* Define to 1 if you have the <sys/bitypes.h> header file. */
/* #undef HAVE_SYS_BITYPES_H */

/* Define to 1 if you have the <sys/cdefs.h> header file. */
/* #undef HAVE_SYS_CDEFS_H */

/* Define to 1 if you have the <sys/inttypes.h> header file. */
/* #undef HAVE_SYS_INTTYPES_H */

/* Define to 1 if you have the <sys/mman.h> header file. */
/* #undef HAVE_SYS_MMAN_H */

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the `towlower' function. */
#define HAVE_TOWLOWER 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if the system has the type `unsigned long long int'. */
#define HAVE_UNSIGNED_LONG_LONG_INT 1

/* Define to 1 if you have the <wchar.h> header file. */
#define HAVE_WCHAR_H 1

/* Define if you have the 'wchar_t' type. */
#define HAVE_WCHAR_T 1

/* Define to 1 if you have the `wcrtomb' function. */
#define HAVE_WCRTOMB 1

/* Define to 1 if you have the `wcscoll' function. */
#define HAVE_WCSCOLL 1

/* Define to 1 if you have the <wctype.h> header file. */
#define HAVE_WCTYPE_H 1

/* Define if you have the 'wint_t' type. */
#define HAVE_WINT_T 1

/* Define to 1 if you have the `wmemchr' function. */
#define HAVE_WMEMCHR 1

/* Define to 1 if you have the `wmemcpy' function. */
#define HAVE_WMEMCPY 1

/* Define to 1 if you have the `wmempcpy' function. */
/* #undef HAVE_WMEMPCPY */

/* Define to 1 if O_NOATIME works. */
#define HAVE_WORKING_O_NOATIME 0

/* Define to 1 if O_NOFOLLOW works. */
#define HAVE_WORKING_O_NOFOLLOW 0

/* Define to 1 if the system has the type `_Bool'. */
#define HAVE__BOOL 1

/* Define to a substitute value for mmap()'s MAP_ANONYMOUS flag. */
/* #undef MAP_ANONYMOUS */

/* Define if the mbrtowc function has the NULL pwc argument bug. */
/* #undef MBRTOWC_NULL_ARG1_BUG */

/* Define if the mbrtowc function has the NULL string argument bug. */
/* #undef MBRTOWC_NULL_ARG2_BUG */

/* Define if the mbrtowc function does not return 0 for a NUL character. */
/* #undef MBRTOWC_NUL_RETVAL_BUG */

/* Define if the mbrtowc function returns a wrong return value. */
#define MBRTOWC_RETVAL_BUG 1

/* Name of package */
#define PACKAGE "pippo"

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "paperino@staff.pippo.org"

/* Define to the full name of this package. */
#define PACKAGE_NAME "pippo"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "pippo 2.6"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "pippo"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "2.6"

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'ptrdiff_t'. */
/* #undef PTRDIFF_T_SUFFIX */

/* Define if nl_langinfo exists but is overridden by gnulib. */
/* #undef REPLACE_NL_LANGINFO */

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'sig_atomic_t'. */
/* #undef SIG_ATOMIC_T_SUFFIX */

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'size_t'. */
/* #undef SIZE_T_SUFFIX */

/* If using the C implementation of alloca, define if you know the
   direction of stack growth for your system; otherwise it will be
   automatically deduced at runtime.
        STACK_DIRECTION > 0 => grows toward higher addresses
        STACK_DIRECTION < 0 => grows toward lower addresses
        STACK_DIRECTION = 0 => direction of growth unknown */
/* #undef STACK_DIRECTION */

/* Define to 1 if the `S_IS*' macros in <sys/stat.h> do not work properly. */
/* #undef STAT_MACROS_BROKEN */

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* Version number of package */
#define VERSION "2.6"

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'wchar_t'. */
/* #undef WCHAR_T_SUFFIX */

/* Define to l, ll, u, ul, ull, etc., as suitable for constants of type
   'wint_t'. */
/* #undef WINT_T_SUFFIX */

/* Enable large inode numbers on Mac OS X.  */
#ifndef _DARWIN_USE_64_BIT_INODE
# define _DARWIN_USE_64_BIT_INODE 1
#endif

/* Number of bits in a file offset, on hosts where this is settable. */
/* #undef _FILE_OFFSET_BITS */

/* Define for large files, on AIX-style hosts. */
/* #undef _LARGE_FILES */

/* Define to 1 if on MINIX. */
/* #undef _MINIX */

/* The _Noreturn keyword of draft C1X.  */
#ifndef _Noreturn
# if (3 <= __GNUC__ || (__GNUC__ == 2 && 8 <= __GNUC_MINOR__) \
      || 0x5110 <= __SUNPRO_C)
#  define _Noreturn __attribute__ ((__noreturn__))
# elif defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn
# endif
#endif


/* Define to 2 if the system does not provide POSIX.1 features except with
   this defined. */
/* #undef _POSIX_1_SOURCE */

/* Define to 1 if you need to in order for `stat' and other things to work. */
/* #undef _POSIX_SOURCE */

/* Define if you want regoff_t to be at least as wide POSIX requires. */
#define _REGEX_LARGE_OFFSETS 1

/* Define to 500 only on HP-UX. */
/* #undef _XOPEN_SOURCE */

/* Enable extensions on AIX 3, Interix.  */
#ifndef _ALL_SOURCE
# define _ALL_SOURCE 1
#endif
/* Enable general extensions on MacOS X.  */
#ifndef _DARWIN_C_SOURCE
# define _DARWIN_C_SOURCE 1
#endif
/* Enable GNU extensions on systems that have them.  */
#ifndef _GNU_SOURCE
# define _GNU_SOURCE 1
#endif
/* Enable threading extensions on Solaris.  */
#ifndef _POSIX_PTHREAD_SEMANTICS
# define _POSIX_PTHREAD_SEMANTICS 1
#endif
/* Enable extensions on HP NonStop.  */
#ifndef _TANDEM_SOURCE
# define _TANDEM_SOURCE 1
#endif
/* Enable general extensions on Solaris.  */
#ifndef __EXTENSIONS__
# define __EXTENSIONS__ 1
#endif


/* Define to a replacement function name for fnmatch(). */
#define fnmatch posix_fnmatch

/* Define to `__inline__' or `__inline' if that's what the C compiler
   calls it, or to nothing if 'inline' is not supported under any name.  */
#ifndef __cplusplus
/* #undef inline */
#endif

/* Work around a bug in Apple GCC 4.0.1 build 5465: In C99 mode, it supports
   the ISO C 99 semantics of 'extern inline' (unlike the GNU C semantics of
   earlier versions), but does not display it by setting __GNUC_STDC_INLINE__.
   __APPLE__ && __MACH__ test for MacOS X.
   __APPLE_CC__ tests for the Apple compiler and its version.
   __STDC_VERSION__ tests for the C99 mode.  */
#if defined __APPLE__ && defined __MACH__ && __APPLE_CC__ >= 5465 && !defined __cplusplus && __STDC_VERSION__ >= 199901L && !defined __GNUC_STDC_INLINE__
# define __GNUC_STDC_INLINE__ 1
#endif

/* Define to a type if <wchar.h> does not define. */
/* #undef mbstate_t */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef mode_t */

/* Define to the type of st_nlink in struct stat, or a supertype. */
#define nlink_t int

/* Define to `int' if <sys/types.h> does not define. */
/* #undef pid_t */

/* Define to rpl_re_comp if the replacement should be used. */
#define re_comp rpl_re_comp

/* Define to rpl_re_compile_fastmap if the replacement should be used. */
#define re_compile_fastmap rpl_re_compile_fastmap

/* Define to rpl_re_compile_pattern if the replacement should be used. */
#define re_compile_pattern rpl_re_compile_pattern

/* Define to rpl_re_exec if the replacement should be used. */
#define re_exec rpl_re_exec

/* Define to rpl_re_match if the replacement should be used. */
#define re_match rpl_re_match

/* Define to rpl_re_match_2 if the replacement should be used. */
#define re_match_2 rpl_re_match_2

/* Define to rpl_re_search if the replacement should be used. */
#define re_search rpl_re_search

/* Define to rpl_re_search_2 if the replacement should be used. */
#define re_search_2 rpl_re_search_2

/* Define to rpl_re_set_registers if the replacement should be used. */
#define re_set_registers rpl_re_set_registers

/* Define to rpl_re_set_syntax if the replacement should be used. */
#define re_set_syntax rpl_re_set_syntax

/* Define to rpl_re_syntax_options if the replacement should be used. */
#define re_syntax_options rpl_re_syntax_options

/* Define to rpl_regcomp if the replacement should be used. */
#define regcomp rpl_regcomp

/* Define to rpl_regerror if the replacement should be used. */
#define regerror rpl_regerror

/* Define to rpl_regexec if the replacement should be used. */
#define regexec rpl_regexec

/* Define to rpl_regfree if the replacement should be used. */
#define regfree rpl_regfree

/* Define to the equivalent of the C99 'restrict' keyword, or to
   nothing if this is not supported.  Do not define if restrict is
   supported directly.  */
#define restrict __restrict
/* Work around a bug in Sun C++: it does not support _Restrict or
   __restrict__, even though the corresponding Sun C compiler ends up with
   "#define restrict _Restrict" or "#define restrict __restrict__" in the
   previous line.  Perhaps some future version of Sun C++ will work with
   restrict; if so, hopefully it defines __RESTRICT like Sun C does.  */
#if defined __SUNPRO_CC && !defined __RESTRICT
# define _Restrict
# define __restrict__
#endif

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */

/* Define as a signed type of the same size as size_t. */
/* #undef ssize_t */

/* Define as a marker that can be attached to declarations that might not
    be used.  This helps to reduce warnings, such as from
    GCC -Wunused-parameter.  */
#if __GNUC__ >= 3 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 7)
# define _GL_UNUSED __attribute__ ((__unused__))
#else
# define _GL_UNUSED
#endif
/* The name _UNUSED_PARAMETER_ is an earlier spelling, although the name
   is a misnomer outside of parameter lists.  */
#define _UNUSED_PARAMETER_ _GL_UNUSED

/* The __pure__ attribute was added in gcc 2.96.  */
#if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 96)
# define _GL_ATTRIBUTE_PURE __attribute__ ((__pure__))
#else
# define _GL_ATTRIBUTE_PURE /* empty */
#endif

/* The __const__ attribute was added in gcc 2.95.  */
#if __GNUC__ > 2 || (__GNUC__ == 2 && __GNUC_MINOR__ >= 95)
# define _GL_ATTRIBUTE_CONST __attribute__ ((__const__))
#else
# define _GL_ATTRIBUTE_CONST /* empty */
#endif

