dnl $Id: aclocal.m4,v 1.31 1998/11/17 01:24:58 reid Exp $
dnl 
dnl Extra autoconf macros for the Glasgow fptools
dnl
dnl To be a good autoconf citizen, names of local macros have
dnl prefixed with FPTOOLS_ to ensure we don't clash
dnl with any pre-supplied autoconf ones.

dnl
dnl Has timezone the type time_t or long (HP-UX 10.20 apparently
dnl has `long'..)
dnl 
AC_DEFUN(FPTOOLS_TYPE_TIMEZONE,
[AC_CACHE_CHECK([type of timezone], fptools_cv_type_timezone,
[AC_TRY_COMPILE([#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif

extern time_t timezone;
],
[int i;], fptools_cv_type_timezone=time_t, fptools_cv_type_timezone=long)])
AC_DEFINE_UNQUOTED(TYPE_TIMEZONE, $fptools_cv_type_timezone)
])

dnl *** Is altzone available? ***
dnl 
AC_DEFUN(FPTOOLS_ALTZONE,
[AC_CACHE_CHECK([altzone], fptools_cv_altzone,
[AC_TRY_LINK([#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
], [return altzone;], 
fptools_cv_altzone=yes, fptools_cv_altzone=no)])
if test "$fptools_cv_altzone" = yes; then
  AC_DEFINE(HAVE_ALTZONE)
fi
])

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
AC_DEFUN(FPTOOLS_UNDERSCORE,
[AC_CHECK_LIB(elf, nlist, LIBS="-lelf $LIBS")dnl
AC_CACHE_CHECK([leading underscore in symbol names], fptools_cv_lead_uscore,

dnl
dnl Hack!: nlist() under Digital UNIX insist on there being an _,
dnl but symbol table listings shows none. What is going on here?!?
dnl
changequote(<<, >>)dnl
<<
case $HostPlatform in
alpha-dec-osf*) fptools_cv_lead_uscore='no';;
*cygwin32)      fptools_cv_lead_uscore='yes';;
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
YES) AC_DEFINE(LEADING_UNDERSCORE);;
esac
])

dnl
dnl Check for Happy and version.
dnl
AC_DEFUN(FPTOOLS_HAPPY,
[AC_PATH_PROG(HappyCmd,happy)
AC_CACHE_CHECK([for version of happy], fptools_cv_happy_version,
[if test x"$HappyCmd" != x; then
   fptools_cv_happy_version="`$HappyCmd -v |
changequote(, )dnl
			  grep 'Happy Version' | sed -e 's/Happy Version \([^ ]*\).*/\1/g'`" ;
changequote([, ])dnl
else
   fptools_cv_happy_version="";
fi;
if expr "$fptools_cv_happy_version" "<" 1.4 > /dev/null 2>&1; then
   echo
   echo "Happy version 1.4 or later is required to compile GHC."
   exit 1;
fi;
])
HappyVersion=$ac_cv_happy_version;
AC_SUBST(HappyVersion)
])

dnl
dnl What's the best way of doing context diffs?
dnl
dnl (NB: NeXTStep thinks diff'ing a file against itself is "trouble")
dnl
AC_DEFUN(FPTOOLS_PROG_DIFF,
[AC_CACHE_CHECK([for ok way to do context diffs], fptools_cv_context_diffs,
[echo foo > conftest1
echo foo > conftest2
if diff -C 1 conftest1 conftest2 > /dev/null 2>&1 ; then
    fptools_cv_context_diffs='diff -C 1'
else
    if diff -c1 conftest1 conftest2 > /dev/null 2>&1 ; then
        fptools_cv_context_diffs='diff -c1'
    else
        echo "Can't figure out how to do context diffs."
        echo "Neither \`diff -C 1' nor \`diff -c1' works."
        exit 1
    fi
fi
rm -f conftest1 conftest2
])
ContextDiffCmd=$fptools_cv_context_diffs
AC_SUBST(ContextDiffCmd)
])

dnl
dnl Finding the Right Yacc
dnl
AC_DEFUN(FPTOOLS_PROG_YACCY,
[AC_PROG_YACC
if test "$YACC" = "yacc"; then
   AC_CACHE_CHECK([if it is an OK yacc], ac_cv_prog_yacc,
   [AC_CHECK_PROG(WhatCmd, what, what, :)
    $WhatCmd $YACC > conftest.out
    if egrep 'y1\.c 1\..*SMI' conftest.out >/dev/null 2>&1; then
        echo "I don't trust your $YaccCmd; it looks like an old Sun yacc"
        if test -f /usr/lang/yacc; then
           echo "I'm going to use /usr/lang/yacc instead"
           ac_cv_prog_yacc=/usr/lang/yacc
        else
           echo "I'm assuming the worst...no parser generator at all"
           ac_cv_prog_yacc=:
        fi
    elif egrep 'y1\.c.*Revision: 4\.2\.6\.3.*DEC' conftest.out >/dev/null 2>&1; then
        echo "I don't trust your $YaccCmd; it looks like a lame DEC yacc"
        echo "I'm assuming the worst...no parser generator at all"
        ac_cv_prog_yacc=:
    else
	ac_cv_prog_yacc=$YACC
    fi
    rm -fr conftest*
])
else
    ac_cv_prog_yacc=$YACC
fi
YaccCmd=$ac_cv_prog_yacc
AC_SUBST(YaccCmd)
])

dnl *** Checking for ar and its arguments + whether we need ranlib.
dnl
dnl ArCmd and RANLIB are AC_SUBST'ed
dnl 
AC_DEFUN(FPTOOLS_PROG_AR_AND_RANLIB,
[AC_PATH_PROG(ArCmd,ar)
if test -z "$ArCmd"; then
    echo "You don't seem to have ar in your PATH...I have no idea how to make a library"
    exit 1;
fi
if $ArCmd clqs conftest.a >/dev/null 2>/dev/null; then
    ArCmd="$ArCmd clqs"
    NeedRanLib=''
elif $ArCmd cqs conftest.a >/dev/null 2>/dev/null; then
    ArCmd="$ArCmd cqs"
    NeedRanLib=''
elif $ArCmd clq conftest.a >/dev/null 2>/dev/null; then
    ArCmd="$ArCmd clq"
    NeedRanLib='YES'
elif $ArCmd cq conftest.a >/dev/null 2>/dev/null; then
    ArCmd="$ArCmd cq"
    NeedRanLib='YES'
elif $ArCmd cq conftest.a 2>&1 | grep 'no archive members specified' >/dev/null 2>/dev/null; then
    ArCmd="$ArCmd cq"
    NeedRanLib='YES'
else
    echo "I can't figure out how to use your $ArCmd"
    exit 1
fi
rm -rf conftest*
test -n "$ArCmd" && test -n "$verbose" && echo "        setting ArCmd to $ArCmd"
AC_SUBST(ArCmd)
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
    cmd_string="`$CC -v 2>&1 | grep 'version ' | sed -e 's/.*version [^0-9]*\([0-9][0-9]*\)\.\([0-9][0-9]*\).*/expr 20 \\\< \1 \\\* 10 + \2/g' `"
changequote([, ])dnl
    if test `eval $cmd_string 2>/dev/null` != "1"; then
	echo ''
        echo "I'm not sure if your version of gcc will work,"
        echo "but it's worth a shot, eh?"
    fi
    fptools_cv_have_gcc='yes'
fi
])
HaveGcc=`echo $fptools_cv_have_gcc | sed 'y/yesno/YESNO/'`
AC_SUBST(HaveGcc)
])

dnl
dnl FPTOOLS_PROG_GNUCPP gathers the path to the cpp that the
dnl gcc driver calls upon.
dnl
dnl Substitutes: GNUCPP and RAWCPP (latter is 'GNUCPP -traditional')
dnl
AC_DEFUN(FPTOOLS_PROG_GNUCPP,
[AC_CACHE_CHECK([how to invoke GNU cpp directly], fptools_cv_gnu_cpp,
[if test "$HaveGcc" = "YES"; then
	echo > conftest.c
	gcc -v -E conftest.c >/dev/null 2>conftest.out
	# \x5c = backslash
	echo 'tr/\x5c/\//; /(\S+\/)cpp/ && print "[$]{1}cpp -iprefix [$]1";' > conftest.pl
	fptools_cv_gnu_cpp="`eval $PerlCmd -n conftest.pl conftest.out`"
	rm -fr conftest*
 else
	# We need to be able to invoke CPP directly, preferably
	# with input from stdin (mkdependHS and hscpp depend on
	# this at the moment).
	# Take a guess at what to use, this probably won't work.
	echo Warning: GNU cpp not found, using $CPP
	fptools_cv_gnu_cpp = $CPP
 fi
])
GNUCPP=$fptools_cv_gnu_cpp
RAWCPP="$GNUCPP -traditional"
AC_SUBST(GNUCPP)
AC_SUBST(RAWCPP)
])

dnl Small feature test for perl version. Assumes PerlCmd
dnl contains path to perl binary
dnl
AC_DEFUN(FPTOOLS_CHECK_PERL_VERSION,
[$PerlCmd -v >conftest.out 2>&1
if grep "version 4" conftest.out >/dev/null 2>&1; then
   if grep "Patch level: 35" conftest.out >/dev/null 2>&1; then
      echo "
************************************************************************
Uh-oh...looks like you have Perl 4.035.

Perl version 4.035 has a bug to do with recursion that will bite if
you run the lit2texi script, when making Info files from
literate files of various sorts.  Either use perl5, the last version of perl4 
(4.036), or an older version (e.g., perl 4.019). Failing that, don't create
any Info files :-)
************************************************************************
"
   fi
else
   if grep "version 5" conftest.out >/dev/null 2>&1; then
      :
   else
     echo "I'm not sure if your version of perl will work,"
     echo "but it's worth a shot, eh?"
   fi
fi
rm -fr conftest*
])

dnl ** figure out the alignment restriction of a type
dnl    (required SIZEOF test but AC_CHECK_SIZEOF doesn't call PROVIDE
dnl     so we can't call REQUIRE)

dnl FPTOOLS_CHECK_ALIGNMENT(TYPE)
AC_DEFUN(FPTOOLS_CHECK_ALIGNMENT,
[changequote(<<, >>)dnl
dnl The name to #define.
define(<<AC_TYPE_NAME>>, translit(alignment_$1, [a-z *], [A-Z_P]))dnl
dnl The cache variable name.
define(<<AC_CV_NAME>>, translit(ac_cv_alignment_$1, [ *], [_p]))dnl
dnl The name of the corresponding size.
define(<<AC_CV_SIZEOF_NAME>>, translit(ac_cv_sizeof_$1, [ *], [_p]))dnl
changequote([, ])dnl
AC_MSG_CHECKING(alignment of $1)
AC_CACHE_VAL(AC_CV_NAME,
[AC_TRY_RUN([
#include <stdio.h>
#if HAVE_STDDEF_H
#include <stddef.h>
#endif
#ifndef offsetof
#define offsetof(ty,field) ((size_t)((char *)&((ty *)0)->field - (char *)(ty *)0))
#endif
int
main()
{
  FILE *f=fopen("conftestval", "w");
  if (!f) exit(1);
  fprintf(f, "%d\n", offsetof(struct { char c; $1 ty;},ty));
  exit(0);
}],
AC_CV_NAME=`cat conftestval`,
AC_CV_NAME=$AC_CV_SIZEOF_NAME,
AC_CV_NAME=$AC_CV_SIZEOF_NAME)])
AC_MSG_RESULT($AC_CV_NAME)
AC_DEFINE_UNQUOTED(AC_TYPE_NAME, $AC_CV_NAME)
AC_PROVIDE($AC_TYPE_NAME)
undefine([AC_TYPE_NAME])dnl
undefine([AC_CV_NAME])dnl
undefine([AC_CV_SIZEOF_NAME])dnl
])

dnl ** figure out whether C compiler supports 'long long's
dnl    (Closely based on Andreas Zeller's macro for testing
dnl     for this under C++)
dnl
dnl    If the C compiler supports `long long' types,
dnl    define `HAVE_LONG_LONG'.
dnl
AC_DEFUN(FPTOOLS_C_LONG_LONG,
[
AC_REQUIRE([AC_PROG_CC])
AC_MSG_CHECKING(whether ${CC} supports long long types)
AC_CACHE_VAL(fptools_cv_have_long_long,
[
AC_LANG_SAVE
AC_LANG_C
AC_TRY_COMPILE(,[long long a;],
fptools_cv_have_long_long=yes,
fptools_cv_have_long_long=no)
AC_LANG_RESTORE
])
AC_MSG_RESULT($fptools_cv_have_long_long)
if test "$fptools_cv_have_long_long" = yes; then
AC_DEFINE(HAVE_LONG_LONG)
fi
])

dnl *** Can we open files in binary mode? ***
dnl 
AC_DEFUN(FPTOOLS_O_BINARY,
[
AC_REQUIRE([AC_PROG_CC])
AC_MSG_CHECKING(whether we can open files in binary mode)
AC_CACHE_VAL(fptools_cv_have_o_binary,
[
AC_LANG_SAVE
AC_LANG_C
AC_TRY_COMPILE(,
[#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
int x = O_BINARY;],
fptools_cv_have_o_binary=yes,
fptools_cv_have_o_binary=no)
AC_LANG_RESTORE
])
AC_MSG_RESULT($fptools_cv_have_o_binary)
if test "$fptools_cv_have_o_binary" = yes; then
AC_DEFINE(HAVE_O_BINARY)
fi
])

dnl *** Which one comes first, .text or .data? ***
dnl 
AC_DEFUN(FPTOOLS_CODE_BEFORE_DATA,
[AC_CACHE_CHECK([if code section appears before data], fptools_cv_code_bef_data,
[AC_TRY_RUN([
int f() { return 1; }
int i;
int main() { return ((char*)&f > (char*)&i); }

],
fptools_cv_code_bef_data=yes, fptools_cv_code_bef_data=no)])
if test "$fptools_cv_code_bef_data" = yes; then
  AC_DEFINE(CODE_BEFORE_DATA)
fi
])

dnl *** Helper function **
dnl 
AC_DEFUN(FPTOOLS_IN_SCOPE,AC_TRY_LINK([extern char* $1;],[return (int)&$2], $3=yes, $3=no))

dnl *** What's the end-of-text-section marker called? ***
dnl
AC_DEFUN(FPTOOLS_END_TEXT_SECTION,
[AC_CACHE_CHECK([for end of text section marker], fptools_cv_end_of_text,
[
not_done=1
for i in etext _etext __etext; do
  FPTOOLS_IN_SCOPE($i,$i,fptools_cv_end_of_text)
  if test "$fptools_cv_end_of_text" = yes; then
   AC_DEFINE_UNQUOTED(TEXT_SECTION_END_MARKER, $i)
   not_done=0
   break
  fi
done
if test "$not_done" = 1; then
FPTOOLS_IN_SCOPE(etext asm("etext"),etext,fptools_cv_end_of_text);
if test "$fptools_cv_end_of_text" = yes; then
  AC_DEFINE(TEXT_SECTION_END_MARKER, etext asm("etext"))
else
  AC_DEFINE(TEXT_SECTION_END_MARKER, dunno_what_it_is)
fi
fi
])])

dnl *** What's the end-of-data-section marker called? ***
dnl
AC_DEFUN(FPTOOLS_END_DATA_SECTION,
[AC_CACHE_CHECK([for end of data section marker], fptools_cv_end_of_data,
[
not_done=1
for i in end _end __end; do
  FPTOOLS_IN_SCOPE($i,$i,fptools_cv_end_of_data)
  if test "$fptools_cv_end_of_data" = yes; then
   AC_DEFINE_UNQUOTED(DATA_SECTION_END_MARKER, $i)
   not_done=0
   break
  fi
done
if test "$not_done" = 1; then
FPTOOLS_IN_SCOPE(end asm("end"),end,fptools_cv_end_of_data);
if test "$fptools_cv_end_of_data" = yes; then
  AC_DEFINE(DATA_SECTION_END_MARKER, end asm("end"))
else
  AC_DEFINE(DATA_SECTION_END_MARKER, dunno_what_it_is)
fi
fi
])])



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

dnl check for prototypes
dnl
AC_DEFUN(AC_C_PROTOTYPES,
[AC_CACHE_CHECK([prototypes], ac_cv_prototypes,
[AC_TRY_COMPILE([
void foo(int);
void foo(i)
int i; { 
return;
}
],
[int i;], 
ac_cv_prototypes=yes,
ac_cv_prototypes=no)])
if test "$ac_cv_prototypes" = yes; then
AC_DEFINE(HAVE_PROTOTYPES)
fi
])


# LocalWords:  fi
