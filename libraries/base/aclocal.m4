dnl @synopsis FP_READDIR_EOF_ERRNO
dnl
dnl Check what readdir() sets 'errno' to upon reaching 
dnl end of directory; not setting it is the correct thing to do,
dnl but mingw based versions have set it to ENOENT until recently
dnl (summer 2004).
dnl
dnl
AC_DEFUN(FP_READDIR_EOF_ERRNO,
[AC_CACHE_CHECK([what readdir sets errno to upon EOF], fptools_cv_readdir_eof_errno,
[AC_TRY_RUN([#include <dirent.h>
#include <stdio.h>
#include <errno.h>
int
main(argc, argv)
int argc;
char **argv;
{
  FILE *f=fopen("conftestval", "w");
#if defined(__MINGW32__)
  int fd = mkdir("testdir");
#else
  int fd = mkdir("testdir", 0666);
#endif
  DIR* dp;
  struct dirent* de;
  int err = 0;

  if (!f) return 1;
  if (fd == -1) { 
     fprintf(stderr,"unable to create directory; quitting.\n");
     return 1;
  }
  close(fd);
  dp = opendir("testdir");
  if (!dp) { 
     fprintf(stderr,"unable to browse directory; quitting.\n");
     rmdir("testdir");
     return 1;
  }

  /* the assumption here is that readdir() will only return NULL
   * due to reaching the end of the directory.
   */
  while (de = readdir(dp)) {
  	;
  }
  err = errno;
  fprintf(f,"%d", err);
  fclose(f);
  closedir(dp);
  rmdir("testdir");
  return 0;
}],fptools_cv_readdir_eof_errno=`cat conftestval`, fptools_cv_readdir_eof_errno=bogus, fptools_cv_readdir_eof_errno=0)])
dnl the cross value is somewhat bogus.
AC_DEFINE_UNQUOTED([READDIR_ERRNO_EOF], [$fptools_cv_readdir_eof_errno], [readdir() sets errno to this upon EOF])
])

dnl @synopsis FP_DIRENT_FLAT_LAYOUT
dnl
dnl Check whether 'struct dirent' (in dirent.h) has d_name defined
dnl as being the final field in a struct, or a pointer to somewhere
dnl else. The former is the standardly thing to do, but mingw defns
dnl have for the longest time gone for the latter. They no longer do,
dnl hence the need to configure test for this.
dnl
dnl
AC_DEFUN(FP_DIRENT_FLAT_LAYOUT,
[AC_CACHE_CHECK([if struct dirent layout is flat], fptools_cv_dirent_flat_layout,
[AC_TRY_RUN([#include <dirent.h>
#include <stdio.h>
#include <string.h>
int
main(argc, argv)
int argc;
char **argv;
{
  struct dirent de;
  /*
   * Check whether d_name is defined as
   *    struct dirent { .... ; char d_name[..]; } 
   * or
   *    struct dirent { .... ; char* d_name; } 
   * 
   * Returns 0 if the former.
   */
  memset(&de,0,sizeof(struct dirent));
  return ((int)de.d_name == 0);
}],fptools_cv_dirent_flat_layout=yes, fptools_cv_dirent_flat_layout=no, fptools_cv_dirent_flat_layout=yes)])
dnl the cross value is somewhat bogus.
if test "$fptools_cv_dirent_flat_layout" = yes; then
AC_DEFINE([STRUCT_DIRENT_FLAT_LAYOUT], [1], [Define to 1 if struct dirent is a flat structure])
fi
])
