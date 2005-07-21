# Extra autoconf macros for the Glasgow fptools
#
# To be a good autoconf citizen, names of local macros have prefixed with FP_ to
# ensure we don't clash with any pre-supplied autoconf ones.


# FP_EVAL_STDERR(COMMAND)
# -----------------------
# Eval COMMAND, save its stderr (without lines resulting from shell tracing)
# into the file conftest.err and the exit status in the variable fp_status.
AC_DEFUN([FP_EVAL_STDERR],
[{ (eval $1) 2>conftest.er1
  fp_status=$?
  grep -v '^ *+' conftest.er1 >conftest.err
  rm -f conftest.er1
  (exit $fp_status); }[]dnl
])# FP_EVAL_STDERR


# FP_CHECK_FLAG(FLAG, [ACTION-IF-SUPPORTED], [ACTION-IF-NOT-SUPPORTED])
# ---------------------------------------------------------------------
# Check to see whether the compiler for the current language supports a
# particular option.
#
# Implementation note: When given an unkown option, GCC issues an warning on
# stderr only, but returns an exit value of 0 nevertheless. Consequently we have
# to check stderr *and* the exit value.
#
# Used by ghc.
AC_DEFUN(FP_CHECK_FLAG,
[AC_LANG_COMPILER_REQUIRE()dnl
AC_LANG_CASE([C],          [fp_compiler="$CC"  m4_pushdef([fp_Flags], [CFLAGS])],
             [C++],        [fp_compiler="$CXX" m4_pushdef([fp_Flags], [CXXFLAGS])],
             [Fortran 77], [fp_compiler="$F77" m4_pushdef([fp_Flags], [FFLAGS])])
m4_pushdef([fp_Cache], [fp_cv_[]fp_Flags[]AS_TR_SH([$1])])[]dnl
AC_CACHE_CHECK([whether $fp_compiler accepts $1], [fp_Cache],
[AC_LANG_CONFTEST([AC_LANG_PROGRAM()])
fp_save_flags="$fp_Flags"
fp_Flags="$fp_Flags $1"
fp_Cache=no
if FP_EVAL_STDERR([$ac_compile conftest.$ac_ext]) >/dev/null; then
  test -s conftest.err || fp_Cache=yes
fi
fp_Flags="$fp_save_flags"
rm -f conftest.err conftest.$ac_ext])
AS_IF([test $fp_Cache = yes], [$2], [$3])[]dnl
m4_popdef([fp_Cache])[]dnl
m4_popdef([fp_Flags])[]dnl
])# FP_CHECK_FLAG


# FP_PROG_CONTEXT_DIFF
# --------------------
# Figure out how to do context diffs. Sets the output variable ContextDiffCmd.
#
# Note: NeXTStep thinks diff'ing a file against itself is "trouble".
#
# Used by ghc, glafp-utils/ltx, and glafp-utils/runstdtest.
AC_DEFUN([FP_PROG_CONTEXT_DIFF],
[AC_CACHE_CHECK([for a working context diff], [fp_cv_context_diff],
[echo foo > conftest1
echo foo > conftest2
fp_cv_context_diff=no
for fp_var in '-C 1' '-c1'
do
  if diff $fp_var conftest1 conftest2 > /dev/null 2>&1; then
    fp_cv_context_diff="diff $fp_var"
    break
  fi
done])
if test x"$fp_cv_context_diff" = xno; then
   AC_MSG_ERROR([cannot figure out how to do context diffs])
fi
AC_SUBST(ContextDiffCmd, [$fp_cv_context_diff])
])# FP_PROG_CONTEXT_DIFF


# FP_DECL_ALTZONE
# ---------------
# Defines HAVE_DECL_ALTZONE to 1 if declared, 0 otherwise.
#
# Used by base package.
AC_DEFUN([FP_DECL_ALTZONE],
[AC_REQUIRE([AC_HEADER_TIME])dnl
AC_CHECK_HEADERS([sys/time.h])
AC_CHECK_DECLS([altzone], [], [],[#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif])
])# FP_DECL_ALTZONE


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


# FP_CHECK_ALIGNMENT(TYPE, [IGNORED], [INCLUDES = DEFAULT-INCLUDES])
# ------------------------------------------------------------------
# A variation of AC_CHECK_SIZEOF for computing the alignment restrictions of a
# given type. Defines ALIGNMENT_TYPE.
AC_DEFUN([FP_CHECK_ALIGNMENT],
[AS_LITERAL_IF([$1], [],
               [AC_FATAL([$0: requires literal arguments])])[]dnl
AC_CHECK_TYPE([$1], [], [], [$3])[]dnl
m4_pushdef([fp_Cache], [AS_TR_SH([fp_cv_alignment_$1])])[]dnl
AC_CACHE_CHECK([alignment of $1], [fp_Cache],
[if test "$AS_TR_SH([ac_cv_type_$1])" = yes; then
  FP_COMPUTE_INT([(long) (&((struct { char c; $1 ty; } *)0)->ty)],
                 [fp_Cache],
                 [AC_INCLUDES_DEFAULT([$3])],
                 [AC_MSG_ERROR([cannot compute alignment ($1)
See `config.log' for more details.], [77])])
else
  fp_Cache=0
fi])[]dnl
AC_DEFINE_UNQUOTED(AS_TR_CPP(alignment_$1), $fp_Cache, [The alignment of a `$1'.])[]dnl
m4_popdef([fp_Cache])[]dnl
])# FP_CHECK_ALIGNMENT


# FP_LEADING_UNDERSCORE
# ---------------------
# Test for determining whether symbol names have a leading underscore. We assume
# that they _haven't_ if anything goes wrong. Sets the output variable
# LeadingUnderscore to YES or NO and defines LEADING_UNDERSCORE correspondingly.
#
# Some nlist implementations seem to try to be compatible by ignoring a leading
# underscore sometimes (eg. FreeBSD). We therefore have to work around this by
# checking for *no* leading underscore first. Sigh.  --SDM
#
# Similarly on OpenBSD, but this test doesn't help. -- dons
AC_DEFUN([FP_LEADING_UNDERSCORE],
[AC_CHECK_LIB([elf], [nlist], [LIBS="-lelf $LIBS"])
AC_CACHE_CHECK([leading underscore in symbol names], [fptools_cv_leading_underscore], [
# Hack!: nlist() under Digital UNIX insist on there being an _,
# but symbol table listings shows none. What is going on here?!?
#
# Another hack: cygwin doesn't come with nlist.h , so we hardwire
# the underscoredness of that "platform"
case $HostPlatform in
*openbsd*) # x86 openbsd is ELF from 3.4 >, meaning no leading uscore
  case $build in
    i386-*2\.@<:@0-9@:>@ | i386-*3\.@<:@0-3@:>@ ) fptools_cv_leading_underscore=yes ;;
    *) fptools_cv_leading_underscore=no ;;
  esac ;;
alpha-dec-osf*) fptools_cv_leading_underscore=no;;
*cygwin32) fptools_cv_leading_underscore=yes;;
*mingw32) fptools_cv_leading_underscore=yes;;
*) AC_RUN_IFELSE([AC_LANG_SOURCE([[#ifdef HAVE_NLIST_H
#include <nlist.h>
struct nlist xYzzY1[] = {{"xYzzY1", 0},{0}};
struct nlist xYzzY2[] = {{"_xYzzY2", 0},{0}};
#endif

int main(argc, argv)
int argc;
char **argv;
{
#ifdef HAVE_NLIST_H
    if(nlist(argv[0], xYzzY1) == 0 && xYzzY1[0].n_value != 0)
        exit(1);
    if(nlist(argv[0], xYzzY2) == 0 && xYzzY2[0].n_value != 0)
        exit(0);
#endif
    exit(1);
}]])],[fptools_cv_leading_underscore=yes],[fptools_cv_leading_underscore=no],[fptools_cv_leading_underscore=no])
;;
esac]);
AC_SUBST([LeadingUnderscore], [`echo $fptools_cv_leading_underscore | sed 'y/yesno/YESNO/'`])
if test x"$fptools_cv_leading_underscore" = xyes; then
   AC_DEFINE([LEADING_UNDERSCORE], [1], [Define to 1 if C symbols have a leading underscore added by the compiler.])
fi])# FP_LEADING_UNDERSCORE


# FP_COMPARE_VERSIONS(VERSION1, TEST, VERSION2, [ACTION-IF-TRUE], [ACTION-IF-FALSE])
# ----------------------------------------------------------------------------------
# Compare dotted version numbers VERSION1 and VERSION2 lexicographically according
# to TEST (one of -eq, -ne, -lt, -le, -gt, or -ge).
AC_DEFUN([FP_COMPARE_VERSIONS],
[fp_version1=$1; fp_version2=$3
fp_save_IFS=$IFS; IFS='.'
while test x"$fp_version1" != x || test x"$fp_version2" != x
do

  set dummy $fp_version1; shift
  fp_num1=""
  test $[@%:@] = 0 || { fp_num1="[$]1"; shift; }
  test x"$fp_num1" = x && fp_num1="0"
  fp_version1="[$]*"

  set dummy $fp_version2; shift
  fp_num2=""
  test $[@%:@] = 0 || { fp_num2="[$]1"; shift; }
  test x"$fp_num2" = x && fp_num2="0"
  fp_version2="[$]*"

  test "$fp_num1" = "$fp_num2" || break;
done
IFS=$fp_save_IFS
AS_IF([test "$fp_num1" $2 "$fp_num2"], [$4], [$5])[]dnl
])# FP_COMPARE_VERSIONS


dnl
dnl Check for GreenCard and version.
dnl
AC_DEFUN(FPTOOLS_GREENCARD,
[
AC_PATH_PROG(GreenCardCmd,greencard)
AC_CACHE_CHECK([for version of greencard], fptools_cv_greencard_version,
changequote(, )dnl
[if test x"$GreenCardCmd" != x; then
   fptools_cv_greencard_version="`$GreenCardCmd --version |
			  grep 'version' | sed -e 's/greencard. version \([^ ]*\).*/\1/g'`"
else
   fptools_cv_greencard_version=""
fi
changequote([, ])dnl
])
FP_COMPARE_VERSIONS([$fptools_cv_greencard_version],[-lt],[$1],
  [AC_MSG_ERROR([greencard version $1 or later is required (found '$fptools_cv_greencard_version')])])[]dnl
GreenCardVersion=$fptools_cv_greencard_version
AC_SUBST(GreenCardVersion)
])

dnl
dnl Check for Happy and version.  If we're building GHC, then we need
dnl at least Happy version 1.14.  If there's no installed Happy, we look
dnl for a happy source tree and point the build system at that instead.
dnl
AC_DEFUN(FPTOOLS_HAPPY,
[
if test -d $srcdir/happy; then
   SrcTreeHappyCmd=$hardtop/happy/src/happy-inplace
else
   SrcTreeHappyCmd=""
fi
if test x"$UseSrcTreeHappy" = xYES; then
  if test x"$SrcTreeHappyCmd" != x; then
     HappyCmd=$SrcTreeHappyCmd
  else
     AC_MSG_ERROR([--enable-src-tree-happy given, but happy not found in source tree])
  fi
else
  AC_PATH_PROG(HappyCmd,happy,$SrcTreeHappyCmd)
fi
AC_CACHE_CHECK([for version of happy], fptools_cv_happy_version,
changequote(, )dnl
[if test x"$HappyCmd" = x"$SrcTreeHappyCmd" -a -e $srcdir/happy/mk/version.mk; then
   fptools_cv_happy_version=`grep '^ProjectVersion[ 	]*=' $srcdir/happy/mk/version.mk | sed 's/[^0-9]*\([0-9][0-9]*\.[0-9][0-9]*\).*/\1/g'`;
elif test x"$HappyCmd" != x; then
   fptools_cv_happy_version="`$HappyCmd -v |
			  grep 'Happy Version' | sed -e 's/Happy Version \([^ ]*\).*/\1/g'`" ;
else
   fptools_cv_happy_version="";
fi;
changequote([, ])dnl
])
if test -d $srcdir/ghc -a ! -f $srcdir/ghc/compiler/parser/Parser.hs; then
  FP_COMPARE_VERSIONS([$fptools_cv_happy_version],[-lt],[1.15],
  [AC_MSG_ERROR([Happy version 1.15 or later is required to compile GHC.])])[]dnl
fi
HappyVersion=$fptools_cv_happy_version;
AC_SUBST(HappyVersion)
])

dnl
dnl Check for Haddock and version.  If there's no installed Haddock, we look
dnl for a haddock source tree and point the build system at that instead.
dnl
AC_DEFUN(FPTOOLS_HADDOCK,
[
if test -d $srcdir/haddock; then
   SrcTreeHaddockCmd=$hardtop/haddock/src/haddock-inplace
else
   SrcTreeHaddockCmd=""
fi
if test x"$UseSrcTreeHaddock" = xYES; then
  if test x"$SrcTreeHaddockCmd" != x; then
     HaddockCmd=$SrcTreeHaddockCmd
  else
     AC_MSG_ERROR([--enable-src-tree-haddock given, but haddock not found in source tree])
  fi
else
  AC_PATH_PROG(HaddockCmd,haddock,$SrcTreeHaddockCmd)
fi
dnl Darn, I forgot to make Haddock print out its version number when
dnl invoked with -v.  We could try generating some HTML and grepping
dnl through that to find the version number, but I think we'll make
dnl do without it for now.
])

dnl
dnl Check for Alex and version.  If we're building GHC, then we need
dnl at least Alex version 2.0.  If there's no installed Alex, we look
dnl for a alex source tree and point the build system at that instead.
dnl
AC_DEFUN(FPTOOLS_ALEX,
[
if test -d $srcdir/alex; then
   SrcTreeAlexCmd=$hardtop/alex/src/alex-inplace
else
   SrcTreeAlexCmd=""
fi
if test x"$UseSrcTreeAlex" = xYES; then
  if test x"$SrcTreeAlexCmd" != x; then
     AlexCmd=$SrcTreeAlexCmd
  else
     AC_MSG_ERROR([--enable-src-tree-alex given, but alex not found in source tree])
  fi
else
  AC_PATH_PROG(AlexCmd,alex,$SrcTreeAlexCmd)
fi
AC_CACHE_CHECK([for version of alex], fptools_cv_alex_version,
changequote(, )dnl
[if test x"$AlexCmd" = x"$SrcTreeAlexCmd"; then
   fptools_cv_alex_version=`grep '^ProjectVersion[ 	]*=' $srcdir/alex/mk/version.mk | sed 's/[^0-9]*\([0-9][0-9]*\.[0-9][0-9]*\).*/\1/g'`;
elif test x"$AlexCmd" != x; then
   fptools_cv_alex_version="`$AlexCmd -v |
			  grep 'Alex [Vv]ersion' | sed -e 's/Alex [Vv]ersion \([0-9\.]*\).*/\1/g'`" ;
else
   fptools_cv_alex_version="";
fi;
changequote([, ])dnl
])
if test -d $srcdir/ghc -a ! -f $srcdir/ghc/compiler/parser/Lexer.hs; then
  FP_COMPARE_VERSIONS([$fptools_cv_alex_version],[-lt],[2.0],
  [AC_MSG_ERROR([Alex version 2.0 or later is required to compile GHC.])])[]dnl
fi
AlexVersion=$fptools_cv_alex_version;
AC_SUBST(AlexVersion)
])


# FP_PROG_LD
# ----------
# Sets the output variable LdCmd to the (non-Cygwin version of the) full path
# of ld. Exits if no ld can be found
AC_DEFUN([FP_PROG_LD],
[AC_PATH_PROG([fp_prog_ld_raw], [ld])
if test -z "$fp_prog_ld_raw"; then
  AC_MSG_ERROR([cannot find ld in your PATH, no idea how to link])
fi
LdCmd=$fp_prog_ld_raw
case $HostPlatform in
  *mingw32) if test x${OSTYPE} != xmsys; then
 	      LdCmd="`cygpath -w ${fp_prog_ld_raw} | sed -e 's@\\\\@/@g'`"
              AC_MSG_NOTICE([normalized ld command to $LdCmd])
            fi
            ;;
esac
AC_SUBST([LdCmd])
])# FP_PROG_LD


# FP_PROG_LD_X
# ------------
# Sets the output variable LdXFlag to -x if ld supports this flag, otherwise the
# variable's value is empty.
AC_DEFUN([FP_PROG_LD_X],
[AC_REQUIRE([FP_PROG_LD])
AC_CACHE_CHECK([whether ld understands -x], [fp_cv_ld_x],
[echo 'foo() {}' > conftest.c
${CC-cc} -c conftest.c
if ${LdCmd} -r -x -o conftest2.o conftest.o > /dev/null 2>&1; then
   fp_cv_ld_x=yes
else
   fp_cv_ld_x=no
fi
rm -rf conftest*])
if test "$fp_cv_ld_x" = yes; then
  LdXFlag=-x
else
  LdXFlag=
fi
AC_SUBST([LdXFlag])
])# FP_PROG_LD_X


# FP_PROG_LD_IS_GNU
# -----------------
# Sets the output variable LdIsGNULd to YES or NO, depending on whether it is
# GNU ld or not.
AC_DEFUN([FP_PROG_LD_IS_GNU],
[AC_REQUIRE([FP_PROG_LD])
AC_CACHE_CHECK([whether ld is GNU ld], [fp_cv_gnu_ld],
[if ${LdCmd} --version 2> /dev/null | grep "GNU" > /dev/null 2>&1; then
  fp_cv_gnu_ld=yes
else
  fp_cv_gnu_ld=no
fi])
AC_SUBST([LdIsGNULd], [`echo $fp_cv_gnu_ld | sed 'y/yesno/YESNO/'`])
])# FP_PROG_LD_IS_GNU


# FP_PROG_AR
# ----------
# Sets fp_prog_ar_raw to the full path of ar and fp_prog_ar to a non-Cygwin
# version of it. Exits if no ar can be found
AC_DEFUN([FP_PROG_AR],
[AC_PATH_PROG([fp_prog_ar_raw], [ar])
if test -z "$fp_prog_ar_raw"; then
  AC_MSG_ERROR([cannot find ar in your PATH, no idea how to make a library])
fi
fp_prog_ar=$fp_prog_ar_raw
case $HostPlatform in
  *mingw32) if test x${OSTYPE} != xmsys; then
 	      fp_prog_ar="`cygpath -w ${fp_prog_ar_raw} | sed -e 's@\\\\@/@g'`"
              AC_MSG_NOTICE([normalized ar command to $fp_prog_ar])
            fi
            ;;
esac
])# FP_PROG_AR


# FP_PROG_AR_IS_GNU
# -----------------
# Sets fp_prog_ar_is_gnu to yes or no, depending on whether it is GNU ar or not.
AC_DEFUN([FP_PROG_AR_IS_GNU],
[AC_REQUIRE([FP_PROG_AR])
AC_CACHE_CHECK([whether $fp_prog_ar_raw is GNU ar], [fp_cv_prog_ar_is_gnu],
[if $fp_prog_ar_raw --version 2> /dev/null | grep "GNU" > /dev/null 2>&1; then
  fp_cv_prog_ar_is_gnu=yes
else
  fp_cv_prog_ar_is_gnu=no
fi])
fp_prog_ar_is_gnu=$fp_cv_prog_ar_is_gnu
])# FP_PROG_AR_IS_GNU


# FP_PROG_AR_ARGS
# ---------------
# Sets fp_prog_ar_args to the arguments for ar and the output variable ArCmd
# to a non-Cygwin invocation of ar including these arguments.
AC_DEFUN([FP_PROG_AR_ARGS],
[AC_REQUIRE([FP_PROG_AR_IS_GNU])
AC_CACHE_CHECK([for ar arguments], [fp_cv_prog_ar_args],
[
# GNU ar needs special treatment: it appears to have problems with
# object files with the same name if you use the 's' modifier, but
# simple 'ar q' works fine, and doesn't need a separate ranlib.
if test $fp_prog_ar_is_gnu = yes; then
  fp_cv_prog_ar_args="q"
else
  touch conftest.dummy
  for fp_var in clqsZ clqs cqs clq cq ; do
     rm -f conftest.a
     if $fp_prog_ar_raw $fp_var conftest.a conftest.dummy > /dev/null 2> /dev/null; then
        fp_cv_prog_ar_args=$fp_var
        break
     fi
  done
  rm -f conftest*
  if test -z "$fp_cv_prog_ar_args"; then
    AC_MSG_ERROR([cannot figure out how to use your $fp_prog_ar_raw])
  fi
fi])
fp_prog_ar_args=$fp_cv_prog_ar_args
AC_SUBST([ArCmd], ["$fp_prog_ar $fp_prog_ar_args"])

])# FP_PROG_AR_ARGS


# FP_PROG_AR_NEEDS_RANLIB
# -----------------------
# Sets the output variable RANLIB to "ranlib" if it is needed and found,
# to ":" otherwise.
AC_DEFUN([FP_PROG_AR_NEEDS_RANLIB],
[AC_REQUIRE([FP_PROG_AR_IS_GNU])
AC_REQUIRE([FP_PROG_AR_ARGS])
AC_REQUIRE([AC_PROG_CC])
AC_CACHE_CHECK([whether ranlib is needed], [fp_cv_prog_ar_needs_ranlib],
[if test $fp_prog_ar_is_gnu = yes; then
  fp_cv_prog_ar_needs_ranlib=no
elif test $TargetPlatform = powerpc-apple-darwin; then
  # It's quite tedious to check for Apple's crazy timestamps in .a files,
  # so we hardcode it.
  fp_cv_prog_ar_needs_ranlib=yes
elif echo $fp_prog_ar_args | grep "s" > /dev/null 2> /dev/null; then
  fp_cv_prog_ar_needs_ranlib=no
else
  fp_cv_prog_ar_needs_ranlib=yes
fi])
if test $fp_cv_prog_ar_needs_ranlib = yes; then
   AC_PROG_RANLIB
else
  RANLIB=":"
  AC_SUBST([RANLIB])
fi
])# FP_PROG_AR_NEEDS_RANLIB


# FP_PROG_AR_SUPPORTS_INPUT
# -------------------------
# Sets the output variable ArSupportsInput to "-input" or "", depending on
# whether ar supports -input flag is supported or not.
AC_DEFUN([FP_PROG_AR_SUPPORTS_INPUT],
[AC_REQUIRE([FP_PROG_AR_IS_GNU])
AC_REQUIRE([FP_PROG_AR_ARGS])
AC_CACHE_CHECK([whether $fp_prog_ar_raw supports -input], [fp_cv_prog_ar_supports_input],
[fp_cv_prog_ar_supports_input=no
if test $fp_prog_ar_is_gnu = no; then
  rm -f conftest*
  touch conftest.lst
  if FP_EVAL_STDERR([$fp_prog_ar_raw $fp_prog_ar_args conftest.a -input conftest.lst]) >/dev/null; then
    test -s conftest.err || fp_cv_prog_ar_supports_input=yes
  fi
  rm -f conftest*
fi])
if test $fp_cv_prog_ar_supports_input = yes; then
    ArSupportsInput="-input"
else
    ArSupportsInput=""
fi
AC_SUBST([ArSupportsInput])
])# FP_PROG_AR_SUPPORTS_INPUT


dnl
dnl AC_SHEBANG_PERL - can we she-bang perl?
dnl
AC_DEFUN(FPTOOLS_SHEBANG_PERL,
[AC_CACHE_CHECK([if your perl works in shell scripts], fptools_cv_shebang_perl,
[echo "#!$PerlCmd"'
exit $1;
' > conftest
chmod u+x conftest
(SHELL=/bin/sh; export SHELL; ./conftest 69 > /dev/null)
if test $? -ne 69; then
   fptools_cv_shebang_perl=yes
else
   fptools_cv_shebang_perl=no
fi
rm -f conftest
])])


# FP_HAVE_GCC
# -----------
# Extra testing of the result AC_PROG_CC, testing the gcc version no. Sets the
# output variables HaveGcc and GccVersion.
AC_DEFUN([FP_HAVE_GCC],
[AC_REQUIRE([AC_PROG_CC])
if test -z "$GCC"; then
   fp_have_gcc=NO
else
   fp_have_gcc=YES
fi
if test "$fp_have_gcc" = "NO" -a -d $srcdir/ghc; then
  AC_MSG_ERROR([gcc is required])
fi
AC_CACHE_CHECK([version of gcc], [fp_gcc_version],
[if test "$fp_have_gcc" = "YES"; then
   fp_gcc_version="`$CC -v 2>&1 | grep 'version ' | sed -e 's/.*version [[^0-9]]*\([[0-9.]]*\).*/\1/g'`"
   FP_COMPARE_VERSIONS([$fp_gcc_version], [-lt], [2.0],
     [AC_MSG_ERROR([Need at least gcc version 2.0 (3.4+ recommended)])])
 else
   fp_gcc_version="not-installed"
 fi
])
AC_SUBST([HaveGcc], [$fp_have_gcc])
AC_SUBST([GccVersion], [$fp_gcc_version])
])# FP_HAVE_GCC

AC_DEFUN([FP_MINGW_GCC],
[
  case $HostOS_CPP in
    mingw*)  AC_MSG_CHECKING([whether $CC is a mingw gcc])
	     if $CC -v 2>&1 | grep mingw >/dev/null; then
		AC_MSG_RESULT([yes])
	     else
		AC_MSG_RESULT([no])
		AC_MSG_ERROR([Please use --with-gcc to specify a mingw gcc])
	     fi;;
    *) ;;	
  esac
])

dnl Small feature test for perl version. Assumes PerlCmd
dnl contains path to perl binary
dnl
AC_DEFUN(FPTOOLS_CHECK_PERL_VERSION,
[$PerlCmd -v >conftest.out 2>&1
   if grep "v5.6" conftest.out >/dev/null 2>&1; then
      :
   else
      if grep "v5.8" conftest.out >/dev/null 2>&1; then
         :
      else
         if grep "version 6" conftest.out >/dev/null 2>&1; then
            :
         else
            AC_MSG_ERROR([your version of perl probably won't work, try upgrading it.])
         fi
      fi
   fi
rm -fr conftest*
])


# FP_CHECK_PROG(VARIABLE, PROG-TO-CHECK-FOR,
#               [VALUE-IF-NOT-FOUND], [PATH], [REJECT])
# -----------------------------------------------------
# HACK: A small wrapper around AC_CHECK_PROG, setting VARIABLE to the full path
# of PROG-TO-CHECK-FOR when found.
AC_DEFUN([FP_CHECK_PROG],
[AC_CHECK_PROG([$1], [$2], [$as_dir/$ac_word$ac_exec_ext], [$3], [$4], [$5])][]dnl
)# FP_CHECK_PROC


# FP_PROG_FIND
# ------------
# Find a non-WinDoze version of the "find" utility.
AC_DEFUN([FP_PROG_FIND],
[AC_PATH_PROG([fp_prog_find], [find])
echo foo > conftest.txt
$fp_prog_find conftest.txt -print > conftest.out 2>&1
if grep '^conftest.txt$' conftest.out > /dev/null 2>&1 ; then
  # OK, looks like a real "find".
  FindCmd="$fp_prog_find"
else
  # Found a poor WinDoze version of "find", ignore it.
  AC_MSG_WARN([$fp_prog_find looks like a non-*nix find, ignoring it])
  FP_CHECK_PROG([FindCmd], [find], [], [], [$fp_prog_find])
fi
rm -f conftest.txt conftest.out
AC_SUBST([FindCmd])[]dnl
])# FP_PROG_FIND


# FP_PROG_SORT
# ------------
# Find a Unix-like sort
AC_DEFUN([FP_PROG_SORT],
[AC_PATH_PROG([fp_prog_sort], [sort])
echo conwip > conftest.txt
$fp_prog_sort -f conftest.txt > conftest.out 2>&1
if grep 'conwip' conftest.out > /dev/null 2>&1 ; then
  # The goods
  SortCmd="$fp_prog_sort"
else
  # Summink else..pick next one.
  AC_MSG_WARN([$fp_prog_sort looks like a non-*nix sort, ignoring it])
  FP_CHECK_PROG([SortCmd], [sort], [], [], [$fp_prog_sort])
fi
rm -f conftest.txt conftest.out
AC_SUBST([SortCmd])[]dnl
])# FP_PROG_SORT

dnl
dnl FPTOOLS_NOCACHE_CHECK prints a message, then sets the
dnl values of the second argument to the result of running
dnl the commands given by the third. It does not cache its
dnl result, so it is suitable for checks which should be
dnl run every time.
dnl
AC_DEFUN(FPTOOLS_NOCACHE_CHECK,
[AC_MSG_CHECKING([$1])
 $3
 AC_MSG_RESULT([$][$2])
])

dnl
dnl FPTOOLS_GHC_VERSION(version)
dnl FPTOOLS_GHC_VERSION(major, minor [, patchlevel])
dnl FPTOOLS_GHC_VERSION(version, major, minor, patchlevel)
dnl
dnl Test for version of installed ghc.  Uses $GHC.
dnl [original version pinched from c2hs]
dnl
AC_DEFUN(FPTOOLS_GHC_VERSION,
[FPTOOLS_NOCACHE_CHECK([version of ghc], [fptools_version_of_ghc],
["${WithGhc-ghc}" --version > conftestghc 2>&1
  cat conftestghc >&AS_MESSAGE_LOG_FD
#Useless Use Of cat award...
  fptools_version_of_ghc=`cat conftestghc | sed -n -e 's/, patchlevel *\([[0-9]]\)/.\1/;s/.* version \([[0-9]][[0-9.]]*\).*/\1/p'`
  rm -fr conftest*
  if test "[$]fptools_version_of_ghc" = ""
  then
    fptools_version_of_ghc='unknown'
  fi
fptools_version_of_ghc[_major]=`echo [$]fptools_version_of_ghc | sed -e 's/^\([[0-9]]\).*/\1/'`
fptools_version_of_ghc[_minor]=`echo [$]fptools_version_of_ghc | sed -e 's/^[[0-9]]\.\([[0-9]]*\).*/\1/'`
fptools_version_of_ghc[_pl]=`echo [$]fptools_version_of_ghc | sed -n -e 's/^[[0-9]]\.[[0-9]]*\.\([[0-9]]*\)/\1/p'`
#
if test "[$]fptools_version_of_ghc[_pl]" = ""
then
  fptools_version_of_ghc[_all]="[$]fptools_version_of_ghc[_major].[$]fptools_version_of_ghc[_minor]"
  fptools_version_of_ghc[_pl]="0"
else
  fptools_version_of_ghc[_all]="[$]fptools_version_of_ghc[_major].[$]fptools_version_of_ghc[_minor].[$]fptools_version_of_ghc[_pl]"
fi
#
ifelse($#, [1], [dnl
[$1]="[$]fptools_version_of_ghc[_all]"
], $#, [2], [dnl
[$1]="[$]fptools_version_of_ghc[_major]"
[$2]="[$]fptools_version_of_ghc[_minor]"
], $#, [3], [dnl
[$1]="[$]fptools_version_of_ghc[_major]"
[$2]="[$]fptools_version_of_ghc[_minor]"
[$3]="[$]fptools_version_of_ghc[_pl]"
], $#, [4], [dnl
[$1]="[$]fptools_version_of_ghc[_all]"
[$2]="[$]fptools_version_of_ghc[_major]"
[$3]="[$]fptools_version_of_ghc[_minor]"
[$4]="[$]fptools_version_of_ghc[_pl]"
])
])
])dnl


dnl ** Map an arithmetic C type to a Haskell type.
dnl    Based on autconf's AC_CHECK_SIZEOF.

dnl FPTOOLS_CHECK_HTYPE(TYPE [, DEFAULT_VALUE, [, VALUE-FOR-CROSS-COMPILATION])
AC_DEFUN(FPTOOLS_CHECK_HTYPE,
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


# FP_CHECK_FUNC(FUNCTION, PROLOGUE, BODY, [ACTION-IF-FOUND], [ACTION-IF-NOT-FOUND])
# ---------------------------------------------------------------------------------
# A variant of AC_CHECK_FUNCS, limited to a single FUNCTION, but with the
# additional flexibility of specifying the PROLOGUE and BODY.
AC_DEFUN([FP_CHECK_FUNC],
[AS_VAR_PUSHDEF([fp_func], [fp_cv_func_$1])dnl
AC_CACHE_CHECK([for $1], fp_func,
[AC_LINK_IFELSE([AC_LANG_PROGRAM([$2], [$3])],
                [AS_VAR_SET(fp_func, yes)],
                [AS_VAR_SET(fp_func, no)])])
AS_IF([test AS_VAR_GET(fp_func) = yes],
      [AC_DEFINE(AS_TR_CPP(HAVE_$1), [1],
                [Define to 1 if you have the `]$1[' function.]) $4],
      [$5])dnl
AS_VAR_POPDEF([fp_func])dnl
])# FP_CHECK_FUNC


# FP_GEN_DOCBOOK_XML
# ------------------
# Generates a DocBook XML V4.2 document in conftest.xml.
AC_DEFUN([FP_GEN_DOCBOOK_XML],
[rm -f conftest.xml
cat > conftest.xml << EOF
<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE book PUBLIC "-//OASIS//DTD DocBook XML V4.2//EN"
   "http://www.oasis-open.org/docbook/xml/4.2/docbookx.dtd">
<book id="test">
  <title>A DocBook Test Document</title>
  <chapter id="id-one">
    <title>A Chapter Title</title>
    <para>This is a paragraph, referencing <xref linkend="id-two"/>.</para>
  </chapter>
  <chapter id="id-two">
    <title>Another Chapter Title</title>
    <para>This is another paragraph, referencing <xref linkend="id-one"/>.</para>
  </chapter>
</book>
EOF
]) # FP_GEN_DOCBOOK_XML


# FP_PROG_XSLTPROC
# ----------------
# Sets the output variable XsltprocCmd to the full path of the XSLT processor
# xsltproc. XsltprocCmd is empty if xsltproc could not be found.
AC_DEFUN([FP_PROG_XSLTPROC],
[AC_PATH_PROG([XsltprocCmd], [xsltproc])
if test -z "$XsltprocCmd"; then
  AC_MSG_WARN([cannot find xsltproc in your PATH, you will not be able to build the documentation])
fi
])# FP_PROG_XSLTPROC


# FP_DIR_DOCBOOK_XSL(XSL-DIRS)
# ----------------------------
# Check which of the directories XSL-DIRS contains DocBook XSL stylesheets. The
# output variable DIR_DOCBOOK_XSL will contain the first usable directory or
# will be empty if none could be found.
AC_DEFUN([FP_DIR_DOCBOOK_XSL],
[AC_REQUIRE([FP_PROG_XSLTPROC])dnl
if test -n "$XsltprocCmd"; then
  AC_CACHE_CHECK([for DocBook XSL stylesheet directory], fp_cv_dir_docbook_xsl,
  [FP_GEN_DOCBOOK_XML
  fp_cv_dir_docbook_xsl=no
  for fp_var in $1; do
     if $XsltprocCmd ${fp_var}/html/docbook.xsl conftest.xml > /dev/null 2>&1; then
        fp_cv_dir_docbook_xsl=$fp_var
        break
     fi
  done
  rm -rf conftest*])
fi
if test x"$fp_cv_dir_docbook_xsl" = xno; then
  AC_MSG_WARN([cannot find DocBook XSL stylesheets, you will not be able to build the documentation])
  DIR_DOCBOOK_XSL=
else
  DIR_DOCBOOK_XSL=$fp_cv_dir_docbook_xsl
fi
AC_SUBST([DIR_DOCBOOK_XSL])
])# FP_DIR_DOCBOOK_XSL


# FP_PROG_XMLLINT
# ----------------
# Sets the output variable XmllintCmd to the full path of the XSLT processor
# xmllint. XmllintCmd is empty if xmllint could not be found.
AC_DEFUN([FP_PROG_XMLLINT],
[AC_PATH_PROG([XmllintCmd], [xmllint])
if test -z "$XmllintCmd"; then
  AC_MSG_WARN([cannot find xmllint in your PATH, you will not be able to validate your documentation])
fi
])# FP_PROG_XMLLINT


# FP_CHECK_DOCBOOK_DTD
# --------------------
AC_DEFUN([FP_CHECK_DOCBOOK_DTD],
[AC_REQUIRE([FP_PROG_XMLLINT])dnl
if test -n "$XmllintCmd"; then
  AC_MSG_CHECKING([for DocBook DTD])
  FP_GEN_DOCBOOK_XML
  if $XmllintCmd --valid --noout conftest.xml > /dev/null 2>&1; then
    AC_MSG_RESULT([ok])
  else
    AC_MSG_RESULT([failed])
    AC_MSG_WARN([cannot find a DTD for DocBook XML V4.2, you will not be able to validate your documentation])
    AC_MSG_WARN([check your XML_CATALOG_FILES environment variable and/or /etc/xml/catalog])
  fi
  rm -rf conftest*
fi
])# FP_CHECK_DOCBOOK_DTD


# FP_GEN_FO
# ------------------
# Generates a formatting objects document in conftest.fo.
AC_DEFUN([FP_GEN_FO],
[rm -f conftest.fo
cat > conftest.fo << EOF
<?xml version="1.0"?>
<fo:root xmlns:fo="http://www.w3.org/1999/XSL/Format">
  <fo:layout-master-set>
    <fo:simple-page-master master-name="blank">
      <fo:region-body/>
    </fo:simple-page-master>
  </fo:layout-master-set>
  <fo:page-sequence master-reference="blank">
    <fo:flow flow-name="xsl-region-body">
      <fo:block>
        Test!
      </fo:block>
    </fo:flow>
  </fo:page-sequence>
</fo:root>
EOF
]) # FP_GEN_FO


# FP_PROG_FOP
# -----------
# Set the output variable 'FopCmd' to the first working 'fop' in the current
# 'PATH'. Note that /usr/bin/fop is broken in SuSE 9.1 (unpatched), so try
# /usr/share/fop/fop.sh in that case (or no 'fop'), too.
AC_DEFUN([FP_PROG_FOP],
[AC_PATH_PROGS([FopCmd1], [fop])
if test -n "$FopCmd1"; then
  AC_CACHE_CHECK([for $FopCmd1 usability], [fp_cv_fop_usability],
    [FP_GEN_FO
    if "$FopCmd1" -fo conftest.fo -ps conftest.ps > /dev/null 2>&1; then
      fp_cv_fop_usability=yes
    else
      fp_cv_fop_usability=no
    fi
    rm -rf conftest*])
  if test x"$fp_cv_fop_usability" = xyes; then
     FopCmd=$FopCmd1
  fi
fi
if test -z "$FopCmd"; then
  AC_PATH_PROGS([FopCmd2], [fop.sh], , [/usr/share/fop])
  FopCmd=$FopCmd2
fi
AC_SUBST([FopCmd])
])# FP_PROG_FOP


# FP_PROG_FO_PROCESSOR
# --------------------
# Try to find an FO processor. PassiveTeX output is sometimes a bit strange, so
# try FOP first. Sets the output variables FopCmd, XmltexCmd, DvipsCmd, and
# PdfxmltexCmd.
AC_DEFUN([FP_PROG_FO_PROCESSOR],
[AC_REQUIRE([FP_PROG_FOP])
AC_PATH_PROG([XmltexCmd], [xmltex])
AC_PATH_PROG([DvipsCmd], [dvips])
if test -z "$FopCmd"; then
  if test -z "$XmltexCmd"; then
    AC_MSG_WARN([cannot find an FO => DVI converter, you will not be able to build DVI or PostScript documentation])
  else
    if test -z "$DvipsCmd"; then
      AC_MSG_WARN([cannot find a DVI  => PS converter, you will not be able to build PostScript documentation])
    fi
  fi
  AC_PATH_PROG([PdfxmltexCmd], [pdfxmltex])
  if test -z "$PdfxmltexCmd"; then
    AC_MSG_WARN([cannot find an FO => PDF converter, you will not be able to build PDF documentation])
  fi
elif test -z "$XmltexCmd"; then
  AC_MSG_WARN([cannot find an FO => DVI converter, you will not be able to build DVI documentation])
fi
])# FP_PROG_FO_PROCESSOR


# FP_PROG_GHC_PKG
# ----------------
# Try to find a ghc-pkg matching the ghc mentioned in the environment variable
# WithGhc. If the latter is unset or no matching ghc-pkg can be found, try to
# find a plain ghc-pkg. Sets the output variable GhcPkgCmd.
AC_DEFUN([FP_PROG_GHC_PKG],
[AC_CACHE_CHECK([for ghc-pkg matching $WithGhc], fp_cv_matching_ghc_pkg,
[fp_ghc_pkg_guess=`echo $WithGhc | sed 's,ghc\(@<:@^/\\@:>@*\)$,ghc-pkg\1,'`
if "$fp_ghc_pkg_guess" -l > /dev/null 2>&1; then
  fp_cv_matching_ghc_pkg=$fp_ghc_pkg_guess
else
  fp_cv_matching_ghc_pkg=no
fi])
if test x"$fp_cv_matching_ghc_pkg" = xno; then
  AC_PATH_PROG([GhcPkgCmd], [ghc-pkg])
else
  GhcPkgCmd=$fp_cv_matching_ghc_pkg
fi])# FP_PROG_GHC_PKG

# LocalWords:  fi
