# Extra autoconf macros for the Glasgow fptools
#
# To be a good autoconf citizen, names of local macros have prefixed with FP_ to
# ensure we don't clash with any pre-supplied autoconf ones.


# FP_EVAL_STDERR(COMMAND)
# ------------------------
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
# -----------------------------------------------------------------------
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
# -------------------
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
# ---------------------------------------------------------
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


# FP_CHECK_CONST(EXPRESSION, [INCLUDES = DEFAULT-INCLUDES], [VALUE-IF-FAIL = -1])
# ---------------------------------------------------------------------------------
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
# ----------------------------------
# autoheader helper for FP_CHECK_CONSTS
m4_define([FP_CHECK_CONSTS_TEMPLATE],
[AC_FOREACH([fp_Const], [$1],
  [AH_TEMPLATE(AS_TR_CPP(CONST_[]fp_Const),
               [The value of ]fp_Const[.])])[]dnl
])# FP_CHECK_CONSTS_TEMPLATE


# FP_CHECK_CONSTS(EXPRESSION..., [INCLUDES = DEFAULT-INCLUDES], [VALUE-IF-FAIL = -1])
# -------------------------------------------------------------------------------------
# List version of FP_CHECK_CONST
AC_DEFUN(FP_CHECK_CONSTS,
[FP_CHECK_CONSTS_TEMPLATE([$1])dnl
for fp_const_name in $1
do
FP_CHECK_CONST([$fp_const_name], [$2], [$3])
done
])# FP_CHECK_CONSTS


dnl ** check for leading underscores in symbol names
dnl 
dnl Test for determining whether symbol names have a leading
dnl underscore.
dnl 
dnl We assume that they _haven't_ if anything goes wrong.
dnl
dnl Some nlist implementations seem to try to be compatible by ignoring
dnl a leading underscore sometimes (eg. FreeBSD).  We therefore have
dnl to work around this by checking for *no* leading underscore first.
dnl Sigh.  --SDM
dnl
dnl Similarly on OpenBSD, but this test doesn't help. -- dons
dnl
AC_DEFUN(FPTOOLS_UNDERSCORE,
[AC_CHECK_LIB(elf, nlist, LIBS="-lelf $LIBS")dnl
AC_CACHE_CHECK([leading underscore in symbol names], fptools_cv_lead_uscore,

dnl
dnl Hack!: nlist() under Digital UNIX insist on there being an _,
dnl but symbol table listings shows none. What is going on here?!?
dnl
dnl Another hack: cygwin doesn't come with nlist.h , so we hardwire
dnl the underscoredness of that "platform"
changequote(<<, >>)dnl
<<
case $HostPlatform in
*openbsd*) # x86 openbsd is ELF from 3.4 >, meaning no leading uscore
    case $build in
        i386-*2\.[[0-9]] | i386-*3\.[[0-3]] ) fptools_cv_lead_uscore='yes' ;;
        *)      fptools_cv_lead_uscore='no' ;;
    esac ;;
alpha-dec-osf*) fptools_cv_lead_uscore='no';;
*cygwin32) fptools_cv_lead_uscore='yes';;
*mingw32) fptools_cv_lead_uscore='yes';;
*) >>
changequote([, ])dnl
AC_TRY_RUN([#ifdef HAVE_NLIST_H
#include <nlist.h>
changequote(<<, >>)dnl
<<
struct nlist xYzzY1[] = {{"xYzzY1", 0},{0}};
struct nlist xYzzY2[] = {{"_xYzzY2", 0},{0}};
#endif

main(argc, argv)
int argc;
char **argv;
{
#ifdef HAVE_NLIST_H
    if(nlist(argv[0], xYzzY1) == 0 && xYzzY1[0].n_value != 0)
        exit(1);
    if(nlist(argv[0], xYzzY2) == 0 && xYzzY2[0].n_value != 0)
        exit(0);>>
changequote([, ])dnl
#endif
    exit(1);
}], fptools_cv_lead_uscore=yes, fptools_cv_lead_uscore=no, fptools_cv_lead_uscore=NO)
;;
esac);
LeadingUnderscore=`echo $fptools_cv_lead_uscore | sed 'y/yesno/YESNO/'`
AC_SUBST(LeadingUnderscore)
case $LeadingUnderscore in
YES) AC_DEFINE([LEADING_UNDERSCORE], [1], [Define to 1 if C symbols have a leading underscore added by the compiler.]);;
esac
])


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
dnl at least Happy version 1.13.  If there's no installed Happy, we look
dnl for a happy source tree and point the build system at that instead.
dnl
AC_DEFUN(FPTOOLS_HAPPY,
[
if test -d $srcdir/happy; then
   SrcTreeHappyCmd=$hardtop/happy/src/happy-inplace
fi
if test x"$UseSrcTreeHappy" = xYES; then
  HappyCmd=$SrcTreeHappyCmd
else
  AC_PATH_PROG(HappyCmd,happy,$SrcTreeHappyCmd)
fi
AC_CACHE_CHECK([for version of happy], fptools_cv_happy_version,
changequote(, )dnl
[if test x"$HappyCmd" = x"$SrcTreeHappyCmd"; then
   fptools_cv_happy_version=`grep '^ProjectVersion[ 	]*=' $srcdir/happy/mk/version.mk | sed 's/.*\([0-9][0-9]*\.[0-9][0-9]*\).*/\1/g'`;
elif test x"$HappyCmd" != x; then
   fptools_cv_happy_version="`$HappyCmd -v |
			  grep 'Happy Version' | sed -e 's/Happy Version \([^ ]*\).*/\1/g'`" ;
else
   fptools_cv_happy_version="";
fi;
changequote([, ])dnl
])
if test -d $srcdir/ghc -a ! -f $srcdir/ghc/compiler/parser/Parser.hs; then
  FP_COMPARE_VERSIONS([$fptools_cv_happy_version],[-lt],[1.13],
  [AC_MSG_ERROR([Happy version 1.13 or later is required to compile GHC.])])[]dnl
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
fi
if test x"$UseSrcTreeHaddock" = xYES; then
  HaddockCmd=$SrcTreeHaddockCmd
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
fi
if test x"$UseSrcTreeAlex" = xYES; then
  AlexCmd=$SrcTreeAlexCmd
else
  AC_PATH_PROG(AlexCmd,alex,$SrcTreeAlexCmd)
fi
AC_CACHE_CHECK([for version of alex], fptools_cv_alex_version,
changequote(, )dnl
[if test x"$AlexCmd" = x"$SrcTreeAlexCmd"; then
   fptools_cv_alex_version=`grep '^ProjectVersion[ 	]*=' $srcdir/alex/mk/version.mk | sed 's/.*\([0-9][0-9]*\.[0-9][0-9]*\).*/\1/g'`;
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


dnl
dnl Check whether ld supports -x
dnl
AC_DEFUN(FPTOOLS_LD_X,
[AC_CACHE_CHECK([whether ld understands -x], fptools_cv_ld_x,
[
echo 'foo() {}' > conftest.c
${CC-cc} -c conftest.c
if ${LdCmd} -r -x -o foo.o conftest.o; then
   fptools_cv_ld_x=yes
else
   fptools_cv_ld_x=no
fi
rm -rf conftest.c conftest.o foo.o
])
if test "$fptools_cv_ld_x" = yes; then
	LdXFlag=-x
else
	LdXFlag=
fi
AC_SUBST(LdXFlag)
])


dnl *** Checking for ar and its arguments + whether we need ranlib.
dnl
dnl ArCmd, ArSupportsInput and RANLIB are AC_SUBST'ed
dnl On Digital UNIX, we test for the -Z (compress) and
dnl -input (take list of files from external file) flags.
dnl 
AC_DEFUN(FPTOOLS_PROG_AR_AND_RANLIB,
[AC_PATH_PROG(ArCmdRaw,ar)
if test -z "$ArCmdRaw"; then
    echo "You don't seem to have ar in your PATH...I have no idea how to make a library"
    exit 1;
fi
dnl GNU ar needs special treatment: it appears to have problems with
dnl object files with the same name if you use the 's' modifier, but
dnl simple 'ar q' works fine, and doesn't need a separate ranlib.
if $ArCmdRaw --version | grep 'GNU' >/dev/null 2>/dev/null; then
    ArCmdArgs='q'
    NeedRanLib=''
elif $ArCmdRaw clqsZ conftest.a >/dev/null 2>/dev/null; then
    ArCmdArgs="clqsZ"
    NeedRanLib=''
elif $ArCmdRaw clqs conftest.a >/dev/null 2>/dev/null; then
    ArCmdArgs="clqs"
    NeedRanLib=''
elif $ArCmdRaw cqs conftest.a >/dev/null 2>/dev/null; then
    ArCmdArgs="cqs"
    NeedRanLib=''
elif $ArCmdRaw clq conftest.a >/dev/null 2>/dev/null; then
    ArCmdArgs="clq"
    NeedRanLib='YES'
elif $ArCmdRaw cq conftest.a >/dev/null 2>/dev/null; then
    ArCmdArgs="cq"
    NeedRanLib='YES'
elif $ArCmdRaw cq conftest.a 2>&1 | grep 'no archive members specified' >/dev/null 2>/dev/null; then
    ArCmdArgs="cq"
    NeedRanLib='YES'
else
    echo "I can't figure out how to use your $ArCmd"
    exit 1
fi
rm -rf conftest*
case $HostPlatform in
 *mingw32) 
 	  ArCmd="`cygpath -w ${ArCmdRaw} | sed -e 's@\\\\@/@g' ` ${ArCmdArgs}"
 	  ;;
 *) ArCmd="${ArCmdRaw} ${ArCmdArgs}"
    ;;
esac
test -n "$ArCmd" && test -n "$verbose" && echo "        setting ArCmd to $ArCmd"
AC_SUBST(ArCmd)
if $ArCmd conftest.a -input /dev/null >/dev/null 2>/dev/null; then
    ArSupportsInput='-input'
else
    ArSupportsInput=''
fi
rm -rf conftest*
test -n "$ArSupportsInput" && test -n "$verbose" && echo "        setting ArSupportsInput to $ArSupportsInput"
AC_SUBST(ArSupportsInput)
if test -z "$NeedRanLib"; then
    RANLIB=':'
    test -n "$verbose" && echo "        setting RANLIB to $RANLIB"
    AC_SUBST(RANLIB)
else
    AC_PROG_RANLIB
fi
])

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

dnl
dnl Extra testing of the result AC_PROG_CC, testing the gcc version no.
dnl *Must* be called after AC_PROG_CC
dnl
AC_DEFUN(FPTOOLS_HAVE_GCC,
[AC_CACHE_CHECK([whether you have an ok gcc], fptools_cv_have_gcc,
[if test -z "$GCC"; then
    echo ''
    echo "You would be better off with gcc"
    echo "Perhaps it is already installed, but not in your PATH?"
    fptools_cv_have_gcc='no'
else
changequote(, )dnl
    gcc_version_str="`$CC -v 2>&1 | grep 'version ' | sed -e 's/.*version [^0-9]*\([0-9][0-9]*\)\.\([0-9][0-9]*\).*/\1\.\2/g' `"
changequote([, ])dnl
    fptools_cv_have_gcc='yes'
    FP_COMPARE_VERSIONS([$gcc_version_str], [-lt], [2.0],
        [fptools_cv_have_gcc='no'
        echo ""
	echo "your gcc version appears to be ..."
        $CC --version
        echo "gcc prior to 2.0 and have never worked with ghc."
        echo "we recommend 2.95.3, although versions back to 2.7.2 should be ok."
        AC_MSG_ERROR([gcc 1.X has never been supported])])
fi
])
HaveGcc=`echo $fptools_cv_have_gcc | sed 'y/yesno/YESNO/'`
AC_SUBST(HaveGcc)
])

dnl
dnl Some OSs (Mandrake Linux, in particular) configure GCC with
dnl -momit-leaf-frame-pointer on by default.  If this is the case, we
dnl need to turn it off for mangling to work.  The test is currently a bit
dnl crude, using only the version number of gcc.
dnl
AC_DEFUN([FPTOOLS_GCC_NEEDS_NO_OMIT_LFPTR],
[AC_CACHE_CHECK([whether gcc needs -mno-omit-leaf-frame-pointer], [fptools_cv_gcc_needs_no_omit_lfptr],
[FP_COMPARE_VERSIONS([$gcc_version_str], [-ge], [3.2],
  [fptools_cv_gcc_needs_no_omit_lfptr=yes],
  [fptools_cv_gcc_needs_no_omit_lfptr=no])])
if test "$fptools_cv_gcc_needs_no_omit_lfptr" = "yes"; then
   AC_DEFINE([HAVE_GCC_MNO_OMIT_LFPTR], [1], [Define to 1 if gcc supports -mno-omit-leaf-frame-pointer.])
fi])# FPTOOLS_GCC_NEEDS_NO_OMIT_LFPTR


dnl Small feature test for perl version. Assumes PerlCmd
dnl contains path to perl binary
dnl
AC_DEFUN(FPTOOLS_CHECK_PERL_VERSION,
[$PerlCmd -v >conftest.out 2>&1
if grep "version 5" conftest.out >/dev/null 2>&1; then
   :
else
   if grep "v5.6" conftest.out >/dev/null 2>&1; then
      :
   else
      if grep "v5.8" conftest.out >/dev/null 2>&1; then
         :
      else
         if grep "version 6" conftest.out >/dev/null 2>&1; then
            :
         else
            echo "Your version of perl probably won't work."
         fi  
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
  cat conftestghc >&AC_FD_CC
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
AC_TRY_RUN([#include <stdio.h>
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
}],AC_CV_NAME=`cat conftestval`,
ifelse([$2], , [AC_CV_NAME=NotReallyAType; AC_CV_NAME_supported=no], AC_CV_NAME=$2),
ifelse([$3], , [AC_CV_NAME=NotReallyATypeCross; AC_CV_NAME_supported=no], AC_CV_NAME=$3))]) dnl
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

dnl Based on AC_TRY_LINK - run iftrue if links cleanly with no warning

dnl FPTOOLS_TRY_LINK_NOWARN(flags,main?,iftrue,iffalse)

AC_DEFUN(FPTOOLS_TRY_LINK_NOWARN,
[
ac_save_LIBS="$LIBS"
LIBS=[$1];
cat > conftest.$ac_ext <<EOF
dnl This sometimes fails to find confdefs.h, for some reason.
dnl [#]line __oline__ "[$]0"
[#]line __oline__ "configure"
#include "confdefs.h"
[$2]
int t() { return 0; }
EOF
if AC_TRY_EVAL(ac_link); then
  ifelse([$3], , :, [
    LIBS="$ac_save_LIBS"
    rm -rf conftest*
    $3])
  ifelse([$4], , , [else
    LIBS="$ac_save_LIBS"
    rm -rf conftest*
    $4
])dnl
fi
rm -f conftest*
]
)

dnl Loosely based on AC_CHECK_LIB in acgeneral.m4 in autoconf distribution

dnl FPTOOLS_CHECK_FLAG_NOWARN(NAME, FLAG, CODE, iftrue, iffalse)

AC_DEFUN(FPTOOLS_CHECK_FLAG_NOWARN,
[AC_MSG_CHECKING([for $1])
 AC_CACHE_VAL(ac_cv_flag_$1,
   [FPTOOLS_TRY_LINK_NOWARN("$2", [main() { $3; exit(0); } ],
     eval "ac_cv_flag_$1=yes",
     eval "ac_cv_flag_$1=no"
   )]
 )
if eval "test \"`echo '$ac_cv_flag_'$1`\" = yes"; then
  AC_MSG_RESULT(yes)
  LIBS="$2 $LIBS"
  $4
else
  AC_MSG_RESULT(no)
  $5
fi
])

dnl FPTOOLS_CHECK_LIB_NOWARN(LIBRARY, FUNCTION)

AC_DEFUN(FPTOOLS_CHECK_LIB_NOWARN,
[FPTOOLS_CHECK_FLAG_NOWARN([function_$2],[],[extern char $2(); $2();],
[changequote(, )dnl
  ac_tr_lib=HAVE_LIB`echo $1 | tr 'abcdefghijklmnopqrstuvwxyz' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'`
 changequote([, ])dnl
 AC_DEFINE_UNQUOTED($ac_tr_lib)
],
[FPTOOLS_CHECK_FLAG_NOWARN([library_$1],[-l$1],[extern char $2(); $2();],
[changequote(, )dnl
  ac_tr_lib=HAVE_LIB`echo $1 | tr 'abcdefghijklmnopqrstuvwxyz' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'`
 changequote([, ])dnl
 AC_DEFINE_UNQUOTED($ac_tr_lib)
],
[]
)])]
)


dnl ** Check which CATALOG file we have to use with DocBook SGML.
dnl
dnl FPTOOLS_DOCBOOK_CATALOG(VARIABLE, JADE, STYLESHEET, CATALOGS-TO-CHECK-FOR)
dnl
dnl If any of the catalogs given in CATALOGS-TO-CHECK-FOR works on this
dnl platform, let VARIABLE refer to this catalog; otherwise, VARIABLE
dnl is set to "no".  JADE is the jade executable and STYLESHEET
dnl a DocBook style sheet.
dnl
AC_DEFUN(FPTOOLS_DOCBOOK_CATALOG,
[AC_CACHE_CHECK([for DocBook CATALOG], fptools_cv_sgml_catalog,
[
cat > conftest.sgml << EOF
<!DOCTYPE Article PUBLIC "-//OASIS//DTD DocBook V3.1//EN">
<Article>
<ArtHeader>
<Title>Test</Title>
<Author><OtherName>Test</OtherName></Author>
<Address>Test</Address>
<PubDate>Test</PubDate>
</ArtHeader>
<Sect1><Title>Test</Title>
<Para>
Test.
</Para>
</Sect1>
</Article>
EOF
fptools_cv_sgml_catalog=no
if test -z "$SGML_CATALOG_FILES" ; then
 for fptools_catalog in $4; do
   ac_try="$2 -t rtf -d $3#print -c $fptools_catalog conftest.sgml"
   if AC_TRY_EVAL(ac_try); then
     fptools_cv_sgml_catalog=[$]fptools_catalog
     break
   fi
 done
else
# If the env var SGML_CATALOG_FILES is defined, assume things are cool.
  fptools_cv_sgml_catalog="yes"
fi
])
rm -rf conftest*
if test $fptools_cv_sgml_catalog != "no"; then
  $1=$fptools_cv_sgml_catalog
fi
])

dnl ######################################################################
dnl FPTOOLS_SEARCH_LIBS(INCLUDES, FUNCTION, SEARCH-LIBS [, ACTION-IF-FOUND
dnl                     [, ACTION-IF-NOT-FOUND [, OTHER-LIBRARIES]]])
dnl Search for a library defining FUNC, if it's not already available.
dnl This is almost the same as AC_SEARCH_LIBS, but the INCLUDES can be
dnl specified.
dnl ######################################################################

AC_DEFUN(FPTOOLS_SEARCH_LIBS,
[AC_PREREQ([2.13])
AC_CACHE_CHECK([for library containing $2], [ac_cv_search_$2],
[ac_func_search_save_LIBS="$LIBS"
ac_cv_search_$2="no"
AC_TRY_LINK([$1], [$2()], [ac_cv_search_$2="none required"])
test "$ac_cv_search_$2" = "no" && for i in $3; do
LIBS="-l$i $6 $ac_func_search_save_LIBS"
AC_TRY_LINK([$1], [$2()],
[ac_cv_search_$2="-l$i"
break])
done
LIBS="$ac_func_search_save_LIBS"])
if test "$ac_cv_search_$2" != "no"; then
  test "$ac_cv_search_$2" = "none required" || LIBS="$ac_cv_search_$2 $LIBS"
  $4
else :
  $5
fi])

dnl ####################### -*- Mode: M4 -*- ###########################
dnl Copyright (C) 98, 1999 Matthew D. Langston <langston@SLAC.Stanford.EDU>
dnl
dnl This file is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2 of the License, or
dnl (at your option) any later version.
dnl
dnl This file is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this file; if not, write to:
dnl
dnl   Free Software Foundation, Inc.
dnl   Suite 330
dnl   59 Temple Place
dnl   Boston, MA 02111-1307, USA.
dnl ####################################################################


dnl @synopsis FPTOOLS_CHECK_LIBM
dnl 
dnl Search for math library (typically -lm).
dnl
dnl The variable LIBM (which is not an output variable by default) is
dnl set to a value which is suitable for use in a Makefile (for example,
dnl in make's LOADLIBES macro) provided you AC_SUBST it first.
dnl
dnl @author Matthew D. Langston <langston@SLAC.Stanford.EDU>

# FPTOOLS_CHECK_LIBM - check for math library
AC_DEFUN(FPTOOLS_CHECK_LIBM,
[AC_REQUIRE([AC_CANONICAL_HOST])dnl
LIBM=
case "$host" in
*-*-beos*)
  # These system don't have libm
  ;;
*-ncr-sysv4.3*)
  AC_CHECK_LIB(mw, _mwvalidcheckl, LIBM="-lmw")
  AC_CHECK_LIB(m, main, LIBM="$LIBM -lm")
  ;;
*)
  AC_CHECK_LIB(m, main, LIBM="-lm")
  ;;
esac
])

dnl ######################################################################
dnl Note: Caching has been completely rewritten, but is still no perfect yet.
dnl ######################################################################

dnl ########################### -*- Mode: M4 -*- #######################
dnl Copyright (C) 98, 1999 Matthew D. Langston <langston@SLAC.Stanford.EDU>
dnl
dnl This file is free software; you can redistribute it and/or modify it
dnl under the terms of the GNU General Public License as published by
dnl the Free Software Foundation; either version 2 of the License, or
dnl (at your option) any later version.
dnl
dnl This file is distributed in the hope that it will be useful, but
dnl WITHOUT ANY WARRANTY; without even the implied warranty of
dnl MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
dnl General Public License for more details.
dnl
dnl You should have received a copy of the GNU General Public License
dnl along with this file; if not, write to:
dnl
dnl   Free Software Foundation, Inc.
dnl   Suite 330
dnl   59 Temple Place
dnl   Boston, MA 02111-1307, USA.
dnl ####################################################################

dnl @synopsis FPTOOLS_HAVE_OPENGL
dnl 
dnl Search for OpenGL.  We search first for Mesa (a GPL'ed version of
dnl OpenGL) before a vendor's version of OpenGL if we were specifically
dnl asked to with `--with-Mesa=yes' or `--with-Mesa'.
dnl
dnl The four "standard" OpenGL libraries are searched for: "-lGL",
dnl "-lGLU", "-lGLX" (or "-lMesaGL", "-lMesaGLU" as the case may be) and
dnl "-lglut".
dnl
dnl All of the libraries that are found (since "-lglut" or "-lGLX" might
dnl be missing) are added to the shell output variable "GL_LIBS", along
dnl with any other libraries that are necessary to successfully link an
dnl OpenGL application (e.g. the X11 libraries).  Care has been taken to
dnl make sure that all of the libraries in "GL_LIBS" are listed in the
dnl proper order.
dnl
dnl Additionally, the shell output variable "GL_CFLAGS" is set to any
dnl flags (e.g. "-I" flags) that are necessary to successfully compile
dnl an OpenGL application.
dnl
dnl The following shell variable (which are not output variables) are
dnl also set to either "yes" or "no" (depending on which libraries were
dnl found) to help you determine exactly what was found.
dnl
dnl   have_GL
dnl   have_GLU
dnl   have_GLX
dnl   have_glut
dnl
dnl A complete little toy "Automake `make distcheck'" package of how to
dnl use this macro is available at:
dnl
dnl   ftp://ftp.slac.stanford.edu/users/langston/autoconf/ac_opengl-0.01.tar.gz
dnl
dnl Please note that as the ac_opengl macro and the toy example evolves,
dnl the version number increases, so you may have to adjust the above
dnl URL accordingly.
dnl
dnl @author Matthew D. Langston <langston@SLAC.Stanford.EDU>

AC_DEFUN(FPTOOLS_HAVE_OPENGL,
[
  AC_REQUIRE([AC_PROG_CC])
  AC_REQUIRE([AC_PATH_X])
  AC_REQUIRE([AC_PATH_XTRA])
  AC_REQUIRE([FPTOOLS_CHECK_LIBM])

dnl Check for Mesa first if we were asked to.
  AC_ARG_ENABLE(Mesa,
[  --enable-mesa
	Prefer Mesa over a vendor's native OpenGL library (default=no)
],
                use_Mesa=$enableval,
                use_Mesa=no)

  if test x"$use_Mesa" = xyes; then
     GL_search_list="MesaGL  GL  opengl32"
    GLU_search_list="MesaGLU GLU glu32"
    GLX_search_list="MesaGLX GLX"
  else
     GL_search_list="GL  opengl32 MesaGL"
    GLU_search_list="GLU glu32    MesaGLU"
    GLX_search_list="GLX          MesaGLX"
  fi      

  AC_LANG_SAVE
  AC_LANG_C

dnl If we are running under X11 then add in the appropriate libraries.
  if test x"$no_x" != xyes; then
dnl Add everything we need to compile and link X programs to GL_CFLAGS
dnl and GL_X_LIBS/GLUT_X_LIBS.
    GL_CFLAGS="$CPPFLAGS $X_CFLAGS"
    GL_X_LIBS="$X_LIBS $X_PRE_LIBS -lXext -lX11 $X_EXTRA_LIBS $LIBM"
    GLUT_X_LIBS="$X_LIBS $X_PRE_LIBS -lXmu -lXt -lXi -lXext -lX11 $X_EXTRA_LIBS $LIBM"
  fi
  GL_save_CPPFLAGS="$CPPFLAGS"
  CPPFLAGS="$GL_CFLAGS"

  GL_save_LIBS="$LIBS"
  LIBS="$GL_X_LIBS"

  FPTOOLS_SEARCH_LIBS([#include <GL/gl.h>],   glEnd,         $GL_search_list,  have_GL=yes,   have_GL=no)
  FPTOOLS_SEARCH_LIBS([#include <GL/glu.h>],  gluNewQuadric, $GLU_search_list, have_GLU=yes,  have_GLU=no)
  FPTOOLS_SEARCH_LIBS([#include <GL/glx.h>],  glXWaitX,      $GLX_search_list, have_GLX=yes,  have_GLX=no)

  if test -n "$LIBS"; then
    GL_LIBS="$LDFLAGS $LIBS"
  else
    GL_LIBS="$LDFLAGS"
    GL_CFLAGS=
  fi

  dnl Keep the GL/GLU/GLX libs, but expand the rest to what GLUT needs.
  dnl (Some systems, like OpenBSD, need the GL/GLU libs.)
  LIBS=`echo "$LIBS" | sed "s@$GL_X_LIBS@$GLUT_X_LIBS@"`

  FPTOOLS_SEARCH_LIBS([#include <GL/glut.h>], glutMainLoop,  glut32 glut,      have_glut=yes, have_glut=no)

  if test -n "$LIBS"; then
    GLUT_LIBS="$LDFLAGS $LIBS"
  fi

  AC_CACHE_CHECK([OpenGL flags], mdl_cv_gl_cflags, [mdl_cv_gl_cflags="$GL_CFLAGS"])
  GL_CFLAGS="$mdl_cv_gl_cflags"
  AC_SUBST(GL_CFLAGS)
  AC_CACHE_CHECK([OpenGL libs],  mdl_cv_gl_libs,   [mdl_cv_gl_libs="$GL_LIBS"])
  GL_LIBS="$mdl_cv_gl_libs"
  AC_SUBST(GL_LIBS)
  AC_CACHE_CHECK([GLUT libs],  mdl_cv_glut_libs,   [mdl_cv_glut_libs="$GLUT_LIBS"])
  GLUT_LIBS="$mdl_cv_glut_libs"
  AC_SUBST(GLUT_LIBS)

dnl Reset GL_X_LIBS/GLUT_X_LIBS regardless, since they were just temporary variables
dnl and we don't want to be global namespace polluters.
  GL_X_LIBS=
  GLUT_X_LIBS=

  LIBS="$GL_save_LIBS"
  CPPFLAGS="$GL_save_CPPFLAGS"

  AC_LANG_RESTORE
])

# LocalWords:  fi
