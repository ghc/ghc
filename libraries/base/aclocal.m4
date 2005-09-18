# FP_COMPUTE_INT(EXPRESSION, VARIABLE, INCLUDES, IF-FAILS)
# --------------------------------------------------------
# Assign VARIABLE the value of the compile-time EXPRESSION using INCLUDES for
# compilation. Execute IF-FAILS when unable to determine the value. Works for
# cross-compilation, too.
#
# Implementation note: We are lazy and use an internal autoconf macro, but it
# is supported in autoconf versions 2.50 up to the actual 2.57, so there is
# little risk.
AC_DEFUN([FP_COMPUTE_INT],
[_AC_COMPUTE_INT([$1], [$2], [$3], [$4])[]dnl
])# FP_COMPUTE_INT


# FP_CHECK_CONST(EXPRESSION, [INCLUDES = DEFAULT-INCLUDES], [VALUE-IF-FAIL = -1])
# -------------------------------------------------------------------------------
# Defines CONST_EXPRESSION to the value of the compile-time EXPRESSION, using
# INCLUDES. If the value cannot be determined, use VALUE-IF-FAIL.
AC_DEFUN([FP_CHECK_CONST],
[AS_VAR_PUSHDEF([fp_Cache], [fp_cv_const_$1])[]dnl
AC_CACHE_CHECK([value of $1], fp_Cache,
[FP_COMPUTE_INT([$1], fp_check_const_result, [AC_INCLUDES_DEFAULT([$2])],
                [fp_check_const_result=m4_default([$3], ['-1'])])
AS_VAR_SET(fp_Cache, [$fp_check_const_result])])[]dnl
AC_DEFINE_UNQUOTED(AS_TR_CPP([CONST_$1]), AS_VAR_GET(fp_Cache), [The value of $1.])[]dnl
AS_VAR_POPDEF([fp_Cache])[]dnl
])# FP_CHECK_CONST


# FP_CHECK_CONSTS_TEMPLATE(EXPRESSION...)
# ---------------------------------------
# autoheader helper for FP_CHECK_CONSTS
m4_define([FP_CHECK_CONSTS_TEMPLATE],
[AC_FOREACH([fp_Const], [$1],
  [AH_TEMPLATE(AS_TR_CPP(CONST_[]fp_Const),
               [The value of ]fp_Const[.])])[]dnl
])# FP_CHECK_CONSTS_TEMPLATE


# FP_CHECK_CONSTS(EXPRESSION..., [INCLUDES = DEFAULT-INCLUDES], [VALUE-IF-FAIL = -1])
# -----------------------------------------------------------------------------------
# List version of FP_CHECK_CONST
AC_DEFUN([FP_CHECK_CONSTS],
[FP_CHECK_CONSTS_TEMPLATE([$1])dnl
for fp_const_name in $1
do
FP_CHECK_CONST([$fp_const_name], [$2], [$3])
done
])# FP_CHECK_CONSTS


dnl ** Map an arithmetic C type to a Haskell type.
dnl    Based on autconf's AC_CHECK_SIZEOF.

dnl FPTOOLS_CHECK_HTYPE(TYPE [, DEFAULT_VALUE, [, VALUE-FOR-CROSS-COMPILATION])
AC_DEFUN([FPTOOLS_CHECK_HTYPE],
[changequote(<<, >>)dnl
dnl The name to #define.
define(<<AC_TYPE_NAME>>, translit(htype_$1, [a-z *], [A-Z_P]))dnl
dnl The cache variable name.
define(<<AC_CV_NAME>>, translit(fptools_cv_htype_$1, [ *], [_p]))dnl
define(<<AC_CV_NAME_supported>>, translit(fptools_cv_htype_sup_$1, [ *], [_p]))dnl
changequote([, ])dnl
AC_MSG_CHECKING(Haskell type for $1)
AC_CACHE_VAL(AC_CV_NAME,
[AC_CV_NAME_supported=yes
fp_check_htype_save_cppflags="$CPPFLAGS"
CPPFLAGS="$CPPFLAGS $X_CFLAGS"
AC_RUN_IFELSE([AC_LANG_SOURCE([[#include <stdio.h>
#include <stddef.h>

#if HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif

#if HAVE_UNISTD_H
# include <unistd.h>
#endif

#if HAVE_SYS_STAT_H
# include <sys/stat.h>
#endif

#if HAVE_FCNTL_H
# include <fcntl.h>
#endif

#if HAVE_SIGNAL_H
# include <signal.h>
#endif

#if HAVE_TIME_H
# include <time.h>
#endif

#if HAVE_TERMIOS_H
# include <termios.h>
#endif

#if HAVE_STRING_H
# include <string.h>
#endif

#if HAVE_CTYPE_H
# include <ctype.h>
#endif

#if defined(HAVE_GL_GL_H)
# include <GL/gl.h>
#elif defined(HAVE_OPENGL_GL_H)
# include <OpenGL/gl.h>
#endif

#if defined(HAVE_AL_ALC_H)
# include <AL/alc.h>
#elif defined(HAVE_OPENAL_ALC_H)
# include <OpenAL/alc.h>
#endif

#if HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif

typedef $1 testing;

main() {
  FILE *f=fopen("conftestval", "w");
  if (!f) exit(1);
  if (((testing)((int)((testing)1.4))) == ((testing)1.4)) {
    fprintf(f, "%s%d\n",
           ((testing)(-1) < (testing)0) ? "Int" : "Word",
           sizeof(testing)*8);
  } else {
    fprintf(f,"%s\n",
           (sizeof(testing) >  sizeof(double)) ? "LDouble" :
           (sizeof(testing) == sizeof(double)) ? "Double"  : "Float");
  }
  fclose(f);
  exit(0);
}]])],[AC_CV_NAME=`cat conftestval`],
[ifelse([$2], , [AC_CV_NAME=NotReallyAType; AC_CV_NAME_supported=no], [AC_CV_NAME=$2])],
[ifelse([$3], , [AC_CV_NAME=NotReallyATypeCross; AC_CV_NAME_supported=no], [AC_CV_NAME=$3])])]) dnl
CPPFLAGS="$fp_check_htype_save_cppflags"
if test "$AC_CV_NAME_supported" = yes; then
  AC_MSG_RESULT($AC_CV_NAME)
  AC_DEFINE_UNQUOTED(AC_TYPE_NAME, $AC_CV_NAME, [Define to Haskell type for $1])
else
  AC_MSG_RESULT([not supported])
fi
undefine([AC_TYPE_NAME])dnl
undefine([AC_CV_NAME])dnl
undefine([AC_CV_NAME_supported])dnl
])


# FP_READDIR_EOF_ERRNO
# --------------------
# Defines READDIR_ERRNO_EOF to what readdir() sets 'errno' to upon reaching end
# of directory (not set => 0); not setting it is the correct thing to do, but
# MinGW based versions have set it to ENOENT until recently (summer 2004).
AC_DEFUN([FP_READDIR_EOF_ERRNO],
[AC_CACHE_CHECK([what readdir sets errno to upon EOF], [fptools_cv_readdir_eof_errno],
[AC_RUN_IFELSE([AC_LANG_SOURCE([[#include <dirent.h>
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
}]])],
[fptools_cv_readdir_eof_errno=`cat conftestval`],
[AC_MSG_WARN([failed to determine the errno value])
 fptools_cv_readdir_eof_errno=0],
[fptools_cv_readdir_eof_errno=0])])
AC_DEFINE_UNQUOTED([READDIR_ERRNO_EOF], [$fptools_cv_readdir_eof_errno], [readdir() sets errno to this upon EOF])
])# FP_READDIR_EOF_ERRNO
