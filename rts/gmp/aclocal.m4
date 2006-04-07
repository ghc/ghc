dnl aclocal.m4 generated automatically by aclocal 1.4a

dnl Copyright (C) 1994, 1995-8, 1999 Free Software Foundation, Inc.
dnl This file is free software; the Free Software Foundation
dnl gives unlimited permission to copy and/or distribute it,
dnl with or without modifications, as long as this notice is preserved.

dnl This program is distributed in the hope that it will be useful,
dnl but WITHOUT ANY WARRANTY, to the extent permitted by law; without
dnl even the implied warranty of MERCHANTABILITY or FITNESS FOR A
dnl PARTICULAR PURPOSE.

dnl  GMP specific autoconf macros


dnl  Copyright (C) 2000 Free Software Foundation, Inc.
dnl
dnl  This file is part of the GNU MP Library.
dnl
dnl  The GNU MP Library is free software; you can redistribute it and/or modify
dnl  it under the terms of the GNU Lesser General Public License as published
dnl  by the Free Software Foundation; either version 2.1 of the License, or (at
dnl  your option) any later version.
dnl
dnl  The GNU MP Library is distributed in the hope that it will be useful, but
dnl  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
dnl  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
dnl  License for more details.
dnl
dnl  You should have received a copy of the GNU Lesser General Public License
dnl  along with the GNU MP Library; see the file COPYING.LIB.  If not, write to
dnl  the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
dnl  MA 02111-1307, USA.


dnl  GMP_HEADER_GETVAL(NAME,FILE)
dnl  ----------------------------
dnl  Expand to the value of a "#define NAME" from the given FILE.
dnl  The regexps here aren't very rugged, but are enough for gmp.
dnl  /dev/null as a parameter prevents a hang if $2 is accidentally omitted.

define(GMP_HEADER_GETVAL,
[patsubst(patsubst(
esyscmd([grep "^#define $1 " $2 /dev/null 2>/dev/null]),
[^.*$1[ 	]+],[]),
[[
 	]*$],[])])


dnl  GMP_VERSION
dnl  -----------
dnl  The gmp version number, extracted from the #defines in gmp.h.
dnl  Two digits like 3.0 if patchlevel <= 0, or three digits like 3.0.1 if
dnl  patchlevel > 0.

define(GMP_VERSION,
[GMP_HEADER_GETVAL(__GNU_MP_VERSION,gmp.h)[]dnl
.GMP_HEADER_GETVAL(__GNU_MP_VERSION_MINOR,gmp.h)[]dnl
ifelse(m4_eval(GMP_HEADER_GETVAL(__GNU_MP_VERSION_PATCHLEVEL,gmp.h) > 0),1,
[.GMP_HEADER_GETVAL(__GNU_MP_VERSION_PATCHLEVEL,gmp.h)])])


dnl  GMP_PROG_M4()
dnl  -------------
dnl
dnl  Find a working m4, either in $PATH or likely locations, and setup $M4
dnl  and an AC_SUBST accordingly.  If $M4 is already set then it's a user
dnl  choice and is accepted with no checks.  GMP_PROG_M4 is like
dnl  AC_PATH_PROG or AC_CHECK_PROG, but it tests each m4 found to see if
dnl  it's good enough.
dnl 
dnl  See mpn/asm-defs.m4 for details on the known bad m4s.

AC_DEFUN(GMP_PROG_M4,
[AC_CACHE_CHECK([for suitable m4],
                gmp_cv_prog_m4,
[if test -n "$M4"; then
  gmp_cv_prog_m4="$M4"
else
  cat >conftest.m4 <<\EOF
dnl  must protect this against being expanded during autoconf m4!
[define(dollarhash,``$][#'')dnl
ifelse(dollarhash(x),1,`define(t1,Y)',
``bad: $][# not supported (SunOS /usr/bin/m4)
'')dnl
ifelse(eval(89),89,`define(t2,Y)',
`bad: eval() doesnt support 8 or 9 in a constant (OpenBSD 2.6 m4)
')dnl
ifelse(t1`'t2,YY,`good
')dnl]
EOF
  echo "trying m4" 1>&AC_FD_CC
  gmp_tmp_val="`(m4 conftest.m4) 2>&AC_FD_CC`"
  echo "$gmp_tmp_val" 1>&AC_FD_CC
  if test "$gmp_tmp_val" = good; then
    gmp_cv_prog_m4="m4"
  else
    IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS=":"
dnl $ac_dummy forces splitting on constant user-supplied paths.
dnl POSIX.2 word splitting is done only on the output of word expansions,
dnl not every word.  This closes a longstanding sh security hole.
    ac_dummy="$PATH:/usr/5bin"
    for ac_dir in $ac_dummy; do
      test -z "$ac_dir" && ac_dir=.
      echo "trying $ac_dir/m4" 1>&AC_FD_CC
      gmp_tmp_val="`($ac_dir/m4 conftest.m4) 2>&AC_FD_CC`"
      echo "$gmp_tmp_val" 1>&AC_FD_CC
      if test "$gmp_tmp_val" = good; then
        gmp_cv_prog_m4="$ac_dir/m4"
        break
      fi
    done
    IFS="$ac_save_ifs"
    if test -z "$gmp_cv_prog_m4"; then
      AC_MSG_ERROR([No usable m4 in \$PATH or /usr/5bin (see config.log for reasons).])
    fi
  fi
  rm -f conftest.m4
fi])
M4="$gmp_cv_prog_m4"
AC_SUBST(M4)
])


dnl  GMP_PROG_CC_FIND([CC_LIST], [REQ_64BIT_CC])
dnl  Find first working compiler in CC_LIST.
dnl  If REQ_64BIT_CC is "yes", the compiler is required to be able to 
dnl  produce 64-bit code.
dnl  NOTE: If a compiler needs any special flags for producing 64-bit code,
dnl  these have to be found in shell variable `gmp_cflags64_{cc}', where `{cc}'
dnl  is the name of the compiler.
dnl  Set CC to the name of the first working compiler.
dnl  If a 64-bit compiler is found, set CC64 to the name of the compiler and
dnl  CFLAGS64 to flags to use.
dnl  This macro does not test if any of the compilers found is a GNU compiler.
dnl  To do this, when you have finally made up your mind on which one to use, 
dnl  and set CC accordingly, invoke [GMP_PROG_CC_SELECT].  That macro will 
dnl  also make sure that your selection of CFLAGS is valid.
dnl
AC_DEFUN(GMP_PROG_CC_FIND,
[AC_BEFORE([$0], [CC_PROG_CPP])
ifelse([$1], , gmp_cc_list="gcc cc", gmp_cc_list="[$1]")
ifelse([$2], , gmp_req_64bit_cc="no", gmp_req_64bit_cc="[$2]")

CC32=
CC64=
for c in $gmp_cc_list; do
  # Avoid cache hits.
  unset CC
  unset ac_cv_prog_CC
  AC_CHECK_TOOL(CC, $c, $c)
  if test -n "$CC"; then
    eval c_flags=\$gmp_cflags_$c
    GMP_PROG_CC_WORKS($CC, $c_flags,
		      gmp_prog_cc_works=yes, 
		      gmp_prog_cc_works=no)

    if test "$gmp_prog_cc_works" != "yes"; then
      continue
    fi

    # Save first working compiler, whether 32- or 64-bit capable.
    if test -z "$CC32"; then
      CC32="$CC"
    fi
    if test "$gmp_req_64bit_cc" = "yes"; then
      eval c_flags=\$gmp_cflags64_$c

      # Verify that the compiler works in 64-bit mode as well.
      # /usr/ucb/cc on Solaris 7 can *compile* in 64-bit mode, but not link.
      GMP_PROG_CC_WORKS($c, $c_flags,
		      	gmp_prog_cc_works=yes, 
		      	gmp_prog_cc_works=no)

      if test "$gmp_prog_cc_works" = "yes"; then
        GMP_CHECK_CC_64BIT($c, $c_flags)
        if test "$gmp_cv_cc_64bit" = "yes"; then
          test -z "$CC64" && CC64="$c"
          test -z "$CFLAGS64" && CFLAGS64="$c_flags"
	  # We have CC64 so we're done.
          break
        fi
      fi
    else
      # We have CC32, and we don't need a 64-bit compiler so we're done.
      break
    fi
  fi
done
CC="$CC32"
])dnl

dnl  GMP_PROG_CC_SELECT
dnl  Check that `CC' works with `CFLAGS'.  Check if `CC' is a GNU compiler.
dnl  Cache the result as `ac_cv_prog_CC'.
AC_DEFUN(GMP_PROG_CC_SELECT,
[AC_BEFORE([$0], [CC_PROG_CPP])
AC_PROG_CC_WORKS
AC_PROG_CC_GNU

if test "$ac_cv_prog_gcc" = "yes"; then
  GCC=yes
else
  GCC=
fi

# Set CFLAGS if not already set.
if test -z "$CFLAGS"; then
  CFLAGS="-g"
  if test "$GCC" = "yes"; then
    CFLAGS="$CFLAGS -O2"
  fi
fi

AC_SUBST(CC)
AC_CACHE_VAL(ac_cv_prog_CC, ac_cv_prog_CC="$CC")
AC_PROVIDE([AC_PROG_CC])
])dnl

dnl  GMP_CHECK_CC_64BIT(cc, cflags64)
dnl  Find out if `CC' can produce 64-bit code.
dnl  Requires NM to be set to nm for target.
dnl  FIXME: Cache result.
AC_DEFUN(GMP_CHECK_CC_64BIT,
[
  gmp_tmp_CC_save="$CC"
  CC="[$1]"
  AC_MSG_CHECKING([whether the C compiler ($CC) is 64-bit capable])
  if test -z "$NM"; then
    echo; echo ["configure: $0: fatal: need nm"]
    exit 1
  fi
  gmp_tmp_CFLAGS_save="$CFLAGS"
  CFLAGS="[$2]"

  case "$target" in 
    hppa2.0*-*-*)
      # FIXME: If gcc is installed under another name than "gcc", we will 
      # test the wrong thing.
      if test "$CC" != "gcc"; then
        dnl Let compiler version A.10.32.30 or higher be ok.
        dnl Bad compiler output:
        dnl   ccom: HP92453-01 G.10.32.05 HP C Compiler
        dnl Good compiler output:
        dnl   ccom: HP92453-01 A.10.32.30 HP C Compiler
        echo >conftest.c
        gmp_tmp_vs=`$CC $CFLAGS -V -c -o conftest.o conftest.c 2>&1 | grep "^ccom:"`
        rm conftest*
        gmp_tmp_v1=`echo $gmp_tmp_vs | sed 's/.* .\.\(.*\)\..*\..* HP C.*/\1/'`
        gmp_tmp_v2=`echo $gmp_tmp_vs | sed 's/.* .\..*\.\(.*\)\..* HP C.*/\1/'`
        gmp_tmp_v3=`echo $gmp_tmp_vs | sed 's/.* .\..*\..*\.\(.*\) HP C.*/\1/'`
	gmp_cv_cc_64bit=no
	test -n "$gmp_tmp_v1" && test "$gmp_tmp_v1" -ge "10" \
  	  && test -n "$gmp_tmp_v2" && test "$gmp_tmp_v2" -ge "32" \
    	  && test -n "$gmp_tmp_v3" && test "$gmp_tmp_v3" -ge "30" \
	  && gmp_cv_cc_64bit=yes
      else	# gcc
	# FIXME: Compile a minimal file and determine if the resulting object 
	# file is an ELF file.  If so, gcc can produce 64-bit code.
	# Do we have file(1) for target?
	gmp_cv_cc_64bit=no
      fi
      ;;
    mips-sgi-irix6.*)
      # We use `-n32' to cc and `-mabi=n32' to gcc, resulting in 64-bit 
      # arithmetic but not 64-bit pointers, so the general test for sizeof
      # (void *) is not valid.
      # Simply try to compile an empty main.  If that succeeds return
      # true.
      AC_TRY_COMPILE( , ,
                     gmp_cv_cc_64bit=yes, gmp_cv_cc_64bit=no,
                     gmp_cv_cc_64bit=no)
      ;;
    *-*-*)
      # Allocate an array of size sizeof (void *) and use nm to determine its 
      # size.  We depend on the first declared variable being put at address 0.
      cat >conftest.c <<EOF
[char arr[sizeof (void *)]={0};
char post=0;]
EOF
      gmp_compile="$CC $CFLAGS -c conftest.c 1>&AC_FD_CC"
      if AC_TRY_EVAL(gmp_compile); then
        changequote(<,>)dnl
	gmp_tmp_val=`$NM conftest.o | grep post | sed -e 's;[[][0-9][]]\(.*\);\1;' \
          -e 's;[^1-9]*\([0-9]*\).*;\1;'`
        changequote([, ])dnl
        if test "$gmp_tmp_val" = "8"; then
	  gmp_cv_cc_64bit=yes
	else
	  gmp_cv_cc_64bit=no
        fi
      else
        echo "configure: failed program was:" >&AC_FD_CC
        cat conftest.$ac_ext >&AC_FD_CC
        gmp_cv_cc_64bit=no
      fi
      rm -f conftest*
      ;;
  esac

  CC="$gmp_tmp_CC_save"
  CFLAGS="$gmp_tmp_CFLAGS_save"
  AC_MSG_RESULT($gmp_cv_cc_64bit)
])dnl

dnl  GMP_INIT([M4-DEF-FILE])
dnl  
AC_DEFUN(GMP_INIT,
[ifelse([$1], , gmp_configm4=config.m4, gmp_configm4="[$1]")
gmp_tmpconfigm4=cnfm4.tmp
gmp_tmpconfigm4i=cnfm4i.tmp
gmp_tmpconfigm4p=cnfm4p.tmp
test -f $gmp_tmpconfigm4 && rm $gmp_tmpconfigm4
test -f $gmp_tmpconfigm4i && rm $gmp_tmpconfigm4i
test -f $gmp_tmpconfigm4p && rm $gmp_tmpconfigm4p
])dnl

dnl  GMP_FINISH
dnl  ----------
dnl  Create config.m4 from its accumulated parts.
dnl
dnl  __CONFIG_M4_INCLUDED__ is used so that a second or subsequent include
dnl  of config.m4 is harmless.
dnl
dnl  A separate ifdef on the angle bracket quoted part ensures the quoting
dnl  style there is respected.  The basic defines from gmp_tmpconfigm4 are
dnl  fully quoted but are still put under an ifdef in case any have been
dnl  redefined by one of the m4 include files.
dnl
dnl  Doing a big ifdef within asm-defs.m4 and/or other macro files wouldn't
dnl  work, since it'd interpret parentheses and quotes in dnl comments, and
dnl  having a whole file as a macro argument would overflow the string space
dnl  on BSD m4.

AC_DEFUN(GMP_FINISH,
[AC_REQUIRE([GMP_INIT])
echo "creating $gmp_configm4"
echo ["dnl $gmp_configm4.  Generated automatically by configure."] > $gmp_configm4
if test -f $gmp_tmpconfigm4; then
  echo ["changequote(<,>)dnl"] >> $gmp_configm4
  echo ["ifdef(<__CONFIG_M4_INCLUDED__>,,<"] >> $gmp_configm4
  cat $gmp_tmpconfigm4 >> $gmp_configm4
  echo [">)"] >> $gmp_configm4
  echo ["changequote(\`,')dnl"] >> $gmp_configm4
  rm $gmp_tmpconfigm4
fi
echo ["ifdef(\`__CONFIG_M4_INCLUDED__',,\`"] >> $gmp_configm4
if test -f $gmp_tmpconfigm4i; then
  cat $gmp_tmpconfigm4i >> $gmp_configm4
  rm $gmp_tmpconfigm4i
fi
if test -f $gmp_tmpconfigm4p; then
  cat $gmp_tmpconfigm4p >> $gmp_configm4
  rm $gmp_tmpconfigm4p
fi
echo ["')"] >> $gmp_configm4
echo ["define(\`__CONFIG_M4_INCLUDED__')"] >> $gmp_configm4
])dnl

dnl  GMP_INCLUDE(FILE)
AC_DEFUN(GMP_INCLUDE,
[AC_REQUIRE([GMP_INIT])
echo ["include(\`$1')"] >> $gmp_tmpconfigm4i
])dnl

dnl  GMP_SINCLUDE(FILE)
AC_DEFUN(GMP_SINCLUDE,
[AC_REQUIRE([GMP_INIT])
echo ["sinclude(\`$1')"] >> $gmp_tmpconfigm4i
])dnl

dnl GMP_DEFINE(MACRO, DEFINITION [, LOCATION])
dnl [ Define M4 macro MACRO as DEFINITION in temporary file.		]
dnl [ If LOCATION is `POST', the definition will appear after any	]
dnl [ include() directives inserted by GMP_INCLUDE/GMP_SINCLUDE.	]
dnl [ Mind the quoting!  No shell variables will get expanded.		]
dnl [ Don't forget to invoke GMP_FINISH to create file config.m4.	]
dnl [ config.m4 uses `<' and '>' as quote characters for all defines.	]
AC_DEFUN(GMP_DEFINE, 
[AC_REQUIRE([GMP_INIT])
echo ['define(<$1>, <$2>)'] >> ifelse([$3], [POST], $gmp_tmpconfigm4p, $gmp_tmpconfigm4)
])dnl

dnl GMP_DEFINE_RAW(STRING, [, LOCATION])
dnl [ Put STRING in temporary file.					]
dnl [ If LOCATION is `POST', the definition will appear after any	]
dnl [ include() directives inserted by GMP_INCLUDE/GMP_SINCLUDE.	]
dnl [ Don't forget to invoke GMP_FINISH to create file config.m4.	]
AC_DEFUN(GMP_DEFINE_RAW,
[AC_REQUIRE([GMP_INIT])
echo [$1] >> ifelse([$2], [POST], $gmp_tmpconfigm4p, $gmp_tmpconfigm4)
])dnl

dnl  GMP_CHECK_ASM_LABEL_SUFFIX
dnl  Should a label have a colon or not?
AC_DEFUN(GMP_CHECK_ASM_LABEL_SUFFIX,
[AC_CACHE_CHECK([what assembly label suffix to use],
               gmp_cv_check_asm_label_suffix,
[case "$target" in 
  *-*-hpux*) gmp_cv_check_asm_label_suffix=[""] ;;
  *) gmp_cv_check_asm_label_suffix=[":"] ;;
esac
])
echo ["define(<LABEL_SUFFIX>, <\$][1$gmp_cv_check_asm_label_suffix>)"] >> $gmp_tmpconfigm4
])dnl

dnl  GMP_CHECK_ASM_UNDERSCORE([ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
dnl  Shamelessly borrowed from glibc.
AC_DEFUN(GMP_CHECK_ASM_UNDERSCORE,
[AC_CACHE_CHECK([if symbols are prefixed by underscore], 
	        gmp_cv_check_asm_underscore,
[cat > conftest.$ac_ext <<EOF
dnl This sometimes fails to find confdefs.h, for some reason.
dnl [#]line __oline__ "[$]0"
[#]line __oline__ "configure"
#include "confdefs.h"
int underscore_test() {
return; }
EOF
if AC_TRY_EVAL(ac_compile); then
  if grep _underscore_test conftest* >/dev/null; then
    gmp_cv_check_asm_underscore=yes
  else
    gmp_cv_check_asm_underscore=no
  fi
else
  echo "configure: failed program was:" >&AC_FD_CC
  cat conftest.$ac_ext >&AC_FD_CC
fi
rm -f conftest*
])
if test "$gmp_cv_check_asm_underscore" = "yes"; then
  GMP_DEFINE(GSYM_PREFIX, [_])
  ifelse([$1], , :, [$1])
else
  GMP_DEFINE(GSYM_PREFIX, [])
  ifelse([$2], , :, [$2])
fi    
])dnl

dnl  GMP_CHECK_ASM_ALIGN_LOG([ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
dnl  Is parameter to `.align' logarithmic?
dnl  Requires NM to be set to nm for target.
AC_DEFUN(GMP_CHECK_ASM_ALIGN_LOG,
[AC_REQUIRE([GMP_CHECK_ASM_GLOBL])
AC_REQUIRE([GMP_CHECK_ASM_DATA])
AC_REQUIRE([GMP_CHECK_ASM_LABEL_SUFFIX])
AC_CACHE_CHECK([if .align assembly directive is logarithmic],
		gmp_cv_check_asm_align_log,
[if test -z "$NM"; then
  echo; echo ["configure: $0: fatal: need nm"]
  exit 1
fi
cat > conftest.s <<EOF
      	$gmp_cv_check_asm_data
      	.align  4
	$gmp_cv_check_asm_globl	foo
	.byte	1
	.align	4
foo$gmp_cv_check_asm_label_suffix
	.byte	2
EOF
ac_assemble="$CCAS $CFLAGS conftest.s 1>&AC_FD_CC"
if AC_TRY_EVAL(ac_assemble); then
  changequote(<,>)
  gmp_tmp_val=`$NM conftest.o | grep foo | sed -e 's;[[][0-9][]]\(.*\);\1;' \
       -e 's;[^1-9]*\([0-9]*\).*;\1;'`
  changequote([, ])dnl
  if test "$gmp_tmp_val" = "10" || test "$gmp_tmp_val" = "16"; then
    gmp_cv_check_asm_align_log=yes
  else
    gmp_cv_check_asm_align_log=no
  fi
else 
  echo "configure: failed program was:" >&AC_FD_CC
  cat conftest.s >&AC_FD_CC
fi
rm -f conftest*
])
GMP_DEFINE_RAW(["define(<ALIGN_LOGARITHMIC>,<$gmp_cv_check_asm_align_log>)"])
if test "$gmp_cv_check_asm_align_log" = "yes"; then
  ifelse([$1], , :, [$1])
else
  ifelse([$2], , :, [$2])
fi  
])dnl


dnl  GMP_CHECK_ASM_ALIGN_FILL_0x90
dnl  -----------------------------
dnl  Determine whether a ",0x90" suffix works on a .align directive.
dnl  This is only meant for use on x86, where 0x90 is a "nop".
dnl
dnl  Old gas, eg. 1.92.3 - needs ",0x90" or else the fill is an invalid 0x00.
dnl  New gas, eg. 2.91 - generates the good multibyte nop fills even when
dnl                      ",0x90" is given.
dnl  Solaris 2.6 as - doesn't allow ",0x90", gives a fatal error.
dnl  Solaris 2.8 as - gives a warning for ",0x90", no ill effect.
dnl
dnl  Note that both solaris "as"s only care about ",0x90" if they actually
dnl  have to use it to fill something, hence the .byte in the sample.  It's
dnl  only the second .align that provokes an error or warning.
dnl
dnl  We prefer to suppress the warning from solaris 2.8 to stop anyone
dnl  worrying something might be wrong.

AC_DEFUN(GMP_CHECK_ASM_ALIGN_FILL_0x90,
[AC_CACHE_CHECK([if the .align directive accepts an 0x90 fill in .text],
                gmp_cv_check_asm_align_fill_0x90,
[AC_REQUIRE([GMP_CHECK_ASM_TEXT])
cat > conftest.s <<EOF
      	$gmp_cv_check_asm_text
      	.align  4, 0x90
	.byte   0
      	.align  4, 0x90
EOF
gmp_tmp_val="`$CCAS $CFLAGS conftest.s 2>&1`"
if test $? = 0; then
  echo "$gmp_tmp_val" 1>&AC_FD_CC
  if echo "$gmp_tmp_val" | grep "Warning: Fill parameter ignored for executable section"; then
    echo "Supressing this warning by omitting 0x90" 1>&AC_FD_CC
    gmp_cv_check_asm_align_fill_0x90=no
  else
    gmp_cv_check_asm_align_fill_0x90=yes
  fi
else
  echo "Non-zero exit code" 1>&AC_FD_CC
  echo "$gmp_tmp_val" 1>&AC_FD_CC
  gmp_cv_check_asm_align_fill_0x90=no
fi
rm -f conftest*
])
GMP_DEFINE_RAW(
["define(<ALIGN_FILL_0x90>,<$gmp_cv_check_asm_align_fill_0x90>)"])
])


dnl  GMP_CHECK_ASM_TEXT
AC_DEFUN(GMP_CHECK_ASM_TEXT,
[AC_CACHE_CHECK([how to switch to text section], gmp_cv_check_asm_text,
[case "$target" in
  *-*-aix*)
    changequote({, })
    gmp_cv_check_asm_text={".csect .text[PR]"}
    changequote([, ])
    ;;
  *-*-hpux*) gmp_cv_check_asm_text=[".code"] ;;
  *) gmp_cv_check_asm_text=[".text"] ;;
esac
])
echo ["define(<TEXT>, <$gmp_cv_check_asm_text>)"] >> $gmp_tmpconfigm4
])dnl

dnl  GMP_CHECK_ASM_DATA
dnl  Can we say `.data'?
AC_DEFUN(GMP_CHECK_ASM_DATA,
[AC_CACHE_CHECK([how to switch to data section], gmp_cv_check_asm_data,
[case "$target" in
  *-*-aix*)
    changequote({, })
    gmp_cv_check_asm_data={".csect .data[RW]"}
    changequote([, ])
    ;;
  *) gmp_cv_check_asm_data=[".data"] ;;
esac
])
echo ["define(<DATA>, <$gmp_cv_check_asm_data>)"] >> $gmp_tmpconfigm4
])dnl

dnl  GMP_CHECK_ASM_GLOBL
dnl  Can we say `.global'?
AC_DEFUN(GMP_CHECK_ASM_GLOBL,
[AC_CACHE_CHECK([how to export a symbol], gmp_cv_check_asm_globl,
[case "$target" in
  *-*-hpux*) gmp_cv_check_asm_globl=[".export"] ;;
  *) gmp_cv_check_asm_globl=[".globl"] ;;
esac
])
echo ["define(<GLOBL>, <$gmp_cv_check_asm_globl>)"] >> $gmp_tmpconfigm4
])dnl

dnl  GMP_CHECK_ASM_TYPE
dnl  Can we say `.type'?
AC_DEFUN(GMP_CHECK_ASM_TYPE,
[AC_CACHE_CHECK([how the .type assembly directive should be used],
gmp_cv_check_asm_type,
[ac_assemble="$CCAS $CFLAGS conftest.s 1>&AC_FD_CC"
for gmp_tmp_prefix in @ \# %; do
  echo "	.type	sym,${gmp_tmp_prefix}function" > conftest.s
  if AC_TRY_EVAL(ac_assemble); then
    gmp_cv_check_asm_type="[.type	\$][1,${gmp_tmp_prefix}\$][2]"
    break
  fi
done
if test -z "$gmp_cv_check_asm_type"; then
  gmp_cv_check_asm_type="[dnl]"
fi
])
echo ["define(<TYPE>, <$gmp_cv_check_asm_type>)"] >> $gmp_tmpconfigm4
])dnl

dnl  GMP_CHECK_ASM_SIZE
dnl  Can we say `.size'?
AC_DEFUN(GMP_CHECK_ASM_SIZE,
[AC_CACHE_CHECK([if the .size assembly directive works], gmp_cv_check_asm_size,
[ac_assemble="$CCAS $CFLAGS conftest.s 1>&AC_FD_CC"
echo '	.size	sym,1' > conftest.s
if AC_TRY_EVAL(ac_assemble); then
  gmp_cv_check_asm_size="[.size	\$][1,\$][2]"
else
  gmp_cv_check_asm_size="[dnl]"
fi
])
echo ["define(<SIZE>, <$gmp_cv_check_asm_size>)"] >> $gmp_tmpconfigm4
])dnl

dnl  GMP_CHECK_ASM_LSYM_PREFIX
dnl  What is the prefix for a local label?
dnl  Requires NM to be set to nm for target.
AC_DEFUN(GMP_CHECK_ASM_LSYM_PREFIX,
[AC_REQUIRE([GMP_CHECK_ASM_LABEL_SUFFIX])
AC_CACHE_CHECK([what prefix to use for a local label], 
gmp_cv_check_asm_lsym_prefix,
[if test -z "$NM"; then
  echo; echo ["$0: fatal: need nm"]
  exit 1
fi
ac_assemble="$CCAS $CFLAGS conftest.s 1>&AC_FD_CC"
gmp_cv_check_asm_lsym_prefix="L"
for gmp_tmp_pre in L .L $ L$; do
  cat > conftest.s <<EOF
dummy${gmp_cv_check_asm_label_suffix}
${gmp_tmp_pre}gurkmacka${gmp_cv_check_asm_label_suffix}
	.byte 0
EOF
  if AC_TRY_EVAL(ac_assemble); then
    $NM conftest.o >/dev/null 2>&1
    gmp_rc=$?
    if test "$gmp_rc" != "0"; then
      echo "configure: $NM failure, using default"
      break
    fi
    if $NM conftest.o | grep gurkmacka >/dev/null; then true; else
      gmp_cv_check_asm_lsym_prefix="$gmp_tmp_pre"
      break
    fi
  else
    echo "configure: failed program was:" >&AC_FD_CC
    cat conftest.s >&AC_FD_CC
    # Use default.
  fi
done
rm -f conftest*
])
echo ["define(<LSYM_PREFIX>, <${gmp_cv_check_asm_lsym_prefix}>)"] >> $gmp_tmpconfigm4
])

dnl  GMP_CHECK_ASM_W32
dnl  How to [define] a 32-bit word.
dnl  Requires NM to be set to nm for target.
AC_DEFUN(GMP_CHECK_ASM_W32,
[AC_REQUIRE([GMP_CHECK_ASM_DATA])
AC_REQUIRE([GMP_CHECK_ASM_GLOBL])
AC_REQUIRE([GMP_CHECK_ASM_LABEL_SUFFIX])
AC_CACHE_CHECK([how to [define] a 32-bit word],
	       gmp_cv_check_asm_w32,
[if test -z "$NM"; then
  echo; echo ["configure: $0: fatal: need nm"]
  exit 1
fi

# FIXME: HPUX puts first symbol at 0x40000000, breaking our assumption
# that it's at 0x0.  We'll have to declare another symbol before the
# .long/.word and look at the distance between the two symbols.  The
# only problem is that the sed expression(s) barfs (on Solaris, for
# example) for the symbol with value 0.  For now, HPUX uses .word.

case "$target" in 
  *-*-hpux*)
    gmp_cv_check_asm_w32=".word"
    ;;
  *-*-*)
    ac_assemble="$CCAS $CFLAGS conftest.s 1>&AC_FD_CC"
    for gmp_tmp_op in .long .word; do
      cat > conftest.s <<EOF
	$gmp_cv_check_asm_data
	$gmp_cv_check_asm_globl	foo
	$gmp_tmp_op	0
foo${gmp_cv_check_asm_label_suffix}
	.byte	0
EOF
      if AC_TRY_EVAL(ac_assemble); then
        changequote(<,>)
        gmp_tmp_val=`$NM conftest.o | grep foo | sed -e 's;[[][0-9][]]\(.*\);\1;' \
             -e 's;[^1-9]*\([0-9]*\).*;\1;'`
        changequote([, ])dnl
        if test "$gmp_tmp_val" = "4"; then
          gmp_cv_check_asm_w32="$gmp_tmp_op"
          break
        fi
      fi
    done
    ;;
esac

if test -z "$gmp_cv_check_asm_w32"; then
  echo; echo ["configure: $0: fatal: do not know how to define a 32-bit word"]
  exit 1
fi
rm -f conftest*
])
echo ["define(<W32>, <$gmp_cv_check_asm_w32>)"] >> $gmp_tmpconfigm4
])

dnl  GMP_CHECK_ASM_MMX([ACTION-IF-FOUND, [ACTION-IF-NOT-FOUND]])
dnl  Can we assemble MMX insns?
AC_DEFUN(GMP_CHECK_ASM_MMX,
[AC_REQUIRE([GMP_CHECK_ASM_TEXT])
AC_CACHE_CHECK([if the assembler knows about MMX instructions],
		gmp_cv_check_asm_mmx,
[cat > conftest.s <<EOF
	$gmp_cv_check_asm_text
	por	%mm0, %mm0
EOF
ac_assemble="$CCAS $CFLAGS conftest.s 1>&AC_FD_CC"
if AC_TRY_EVAL(ac_assemble); then
  gmp_cv_check_asm_mmx=yes
else 
  gmp_cv_check_asm_mmx=no
fi
rm -f conftest*
])
if test "$gmp_cv_check_asm_mmx" = "yes"; then
  ifelse([$1], , :, [$1])
else
  AC_MSG_WARN([+----------------------------------------------------------])
  AC_MSG_WARN([| WARNING WARNING WARNING])
  AC_MSG_WARN([| Target CPU has MMX code, but it can't be assembled by])
  AC_MSG_WARN([|     $CCAS $CFLAGS])
  AC_MSG_WARN([| Non-MMX replacements will be used.])
  AC_MSG_WARN([| This will be an inferior build.])
  AC_MSG_WARN([+----------------------------------------------------------])
  ifelse([$2], , :, [$2])
fi
])dnl

dnl  GMP_CHECK_ASM_SHLDL_CL([ACTION-IF-FOUND, [ACTION-IF-NOT-FOUND]])
AC_DEFUN(GMP_CHECK_ASM_SHLDL_CL,
[AC_REQUIRE([GMP_CHECK_ASM_TEXT])
AC_CACHE_CHECK([if the assembler takes cl with shldl],
		gmp_cv_check_asm_shldl_cl,
[cat > conftest.s <<EOF
	$gmp_cv_check_asm_text
	shldl	%cl, %eax, %ebx
EOF
ac_assemble="$CCAS $CFLAGS conftest.s 1>&AC_FD_CC"
if AC_TRY_EVAL(ac_assemble); then
  gmp_cv_check_asm_shldl_cl=yes
else 
  gmp_cv_check_asm_shldl_cl=no
fi
rm -f conftest*
])
if test "$gmp_cv_check_asm_shldl_cl" = "yes"; then
  ifelse([$1], , :, [$1])
else
  ifelse([$2], , :, [$2])
fi
])dnl

dnl  GMP_PROG_CC_WORKS(CC, CFLAGS, ACTION-IF-WORKS, [ACTION-IF-NOT-WORKS])
dnl  Check if CC can compile and link.  Perform various target specific tests.
dnl  FIXME: Require `$target'.
AC_DEFUN(GMP_PROG_CC_WORKS,
[AC_LANG_C	dnl  Note: Destructive.
CC="[$1]"
CFLAGS="[$2]"
AC_MSG_CHECKING([if the C compiler ($CC) works with flags $CFLAGS])

# Simple test for all targets.
AC_TRY_COMPILER([int main(){return(0);}],
                tmp_works, tmp_cross)

# Target specific tests.
if test "$tmp_works" = "yes"; then
  case "$target" in 
    *-*-aix*)	# Returning a funcptr.
      AC_TRY_COMPILE( , [} void *g(); void *f() { return g(); } int bar(){],
                      tmp_works=yes, tmp_works=no)
      ;;
  esac
fi

if test "$tmp_works" = "yes"; then
  [$3]
else
  ifelse([$4], , :, [$4])
fi

AC_MSG_RESULT($tmp_works)
])dnl


dnl  GMP_C_ANSI2KNR
dnl  --------------
dnl  Setup to use ansi2knr if necessary.
dnl
dnl  The test here is simply that if an ANSI style function works then
dnl  ansi2knr isn't needed.  The normal tests for whether $CC works mean we
dnl  don't need to worry here about anything badly broken.
dnl
dnl  AM_C_PROTOTYPES is the normal way to set up ansi2knr, but (in automake
dnl  March 2000) it gives the wrong answer on a C++ compiler because its
dnl  test requires that the compiler accept both ANSI and K&R, or otherwise
dnl  ansi2knr is used.  A C++ compiler fails on the K&R part, which makes
dnl  AM_C_PROTOTYPES think it needs ansi2knr!  GMP has no bare K&R so we
dnl  only need ANSI or K&R to work, not both.

AC_DEFUN(GMP_C_ANSI2KNR,
[AC_CACHE_CHECK([if ansi2knr should be used],
                gmp_cv_c_ansi2knr,
[cat >conftest.c <<EOF
int main (int argc, char *argv[]) { return 0; }
EOF
if AC_TRY_EVAL(ac_compile); then
  gmp_cv_c_ansi2knr=no
else
  gmp_cv_c_ansi2knr=yes
fi
rm -f conftest.*
])
if test $gmp_cv_c_ansi2knr = no; then
  U= ANSI2KNR=
else
  U=_ ANSI2KNR=./ansi2knr
  # Ensure some checks needed by ansi2knr itself.
  AC_HEADER_STDC
  AC_CHECK_HEADERS(string.h)
fi
AC_SUBST(U)
AC_SUBST(ANSI2KNR)
])


dnl  Deal with bad synchronization of Autoconf with Libtool.
AC_DEFUN(AC_CANONICAL_BUILD, [_AC_CANONICAL_BUILD])
AC_DEFUN(AC_CHECK_TOOL_PREFIX, [_AC_CHECK_TOOL_PREFIX])


# serial 1

AC_DEFUN(AM_C_PROTOTYPES,
[AC_REQUIRE([AM_PROG_CC_STDC])
AC_REQUIRE([AC_PROG_CPP])
AC_MSG_CHECKING([for function prototypes])
if test "$am_cv_prog_cc_stdc" != no; then
  AC_MSG_RESULT(yes)
  AC_DEFINE(PROTOTYPES,1,[Define if compiler has function prototypes])
  U= ANSI2KNR=
else
  AC_MSG_RESULT(no)
  U=_ ANSI2KNR=./ansi2knr
  # Ensure some checks needed by ansi2knr itself.
  AC_HEADER_STDC
  AC_CHECK_HEADERS(string.h)
fi
AC_SUBST(U)dnl
AC_SUBST(ANSI2KNR)dnl
])


# serial 1

# @defmac AC_PROG_CC_STDC
# @maindex PROG_CC_STDC
# @ovindex CC
# If the C compiler in not in ANSI C mode by default, try to add an option
# to output variable @code{CC} to make it so.  This macro tries various
# options that select ANSI C on some system or another.  It considers the
# compiler to be in ANSI C mode if it handles function prototypes correctly.
#
# If you use this macro, you should check after calling it whether the C
# compiler has been set to accept ANSI C; if not, the shell variable
# @code{am_cv_prog_cc_stdc} is set to @samp{no}.  If you wrote your source
# code in ANSI C, you can make an un-ANSIfied copy of it by using the
# program @code{ansi2knr}, which comes with Ghostscript.
# @end defmac

AC_DEFUN(AM_PROG_CC_STDC,
[AC_REQUIRE([AC_PROG_CC])
AC_BEFORE([$0], [AC_C_INLINE])
AC_BEFORE([$0], [AC_C_CONST])
dnl Force this before AC_PROG_CPP.  Some cpp's, eg on HPUX, require
dnl a magic option to avoid problems with ANSI preprocessor commands
dnl like #elif.
dnl FIXME: can't do this because then AC_AIX won't work due to a
dnl circular dependency.
dnl AC_BEFORE([$0], [AC_PROG_CPP])
AC_MSG_CHECKING(for ${CC-cc} option to accept ANSI C)
AC_CACHE_VAL(am_cv_prog_cc_stdc,
[am_cv_prog_cc_stdc=no
ac_save_CC="$CC"
# Don't try gcc -ansi; that turns off useful extensions and
# breaks some systems' header files.
# AIX			-qlanglvl=ansi
# Ultrix and OSF/1	-std1
# HP-UX 10.20 and later	-Ae
# HP-UX older versions	-Aa -D_HPUX_SOURCE
# SVR4			-Xc -D__EXTENSIONS__
for ac_arg in "" -qlanglvl=ansi -std1 -Ae "-Aa -D_HPUX_SOURCE" "-Xc -D__EXTENSIONS__"
do
  CC="$ac_save_CC $ac_arg"
  AC_TRY_COMPILE(
[#include <stdarg.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
/* Most of the following tests are stolen from RCS 5.7's src/conf.sh.  */
struct buf { int x; };
FILE * (*rcsopen) (struct buf *, struct stat *, int);
static char *e (p, i)
     char **p;
     int i;
{
  return p[i];
}
static char *f (char * (*g) (char **, int), char **p, ...)
{
  char *s;
  va_list v;
  va_start (v,p);
  s = g (p, va_arg (v,int));
  va_end (v);
  return s;
}
int test (int i, double x);
struct s1 {int (*f) (int a);};
struct s2 {int (*f) (double a);};
int pairnames (int, char **, FILE *(*)(struct buf *, struct stat *, int), int, int);
int argc;
char **argv;
], [
return f (e, argv, 0) != argv[0]  ||  f (e, argv, 1) != argv[1];
],
[am_cv_prog_cc_stdc="$ac_arg"; break])
done
CC="$ac_save_CC"
])
if test -z "$am_cv_prog_cc_stdc"; then
  AC_MSG_RESULT([none needed])
else
  AC_MSG_RESULT($am_cv_prog_cc_stdc)
fi
case "x$am_cv_prog_cc_stdc" in
  x|xno) ;;
  *) CC="$CC $am_cv_prog_cc_stdc" ;;
esac
])

# Do all the work for Automake.  This macro actually does too much --
# some checks are only needed if your package does certain things.
# But this isn't really a big deal.

# serial 1

dnl Usage:
dnl AM_INIT_AUTOMAKE(package,version, [no-define])

AC_DEFUN(AM_INIT_AUTOMAKE,
[AC_REQUIRE([AC_PROG_INSTALL])
dnl We require 2.13 because we rely on SHELL being computed by configure.
AC_PREREQ([2.13])
PACKAGE=[$1]
AC_SUBST(PACKAGE)
VERSION=[$2]
AC_SUBST(VERSION)
dnl test to see if srcdir already configured
if test "`CDPATH=: && cd $srcdir && pwd`" != "`pwd`" &&
   test -f $srcdir/config.status; then
  AC_MSG_ERROR([source directory already configured; run "make distclean" there first])
fi
ifelse([$3],,
AC_DEFINE_UNQUOTED(PACKAGE, "$PACKAGE", [Name of package])
AC_DEFINE_UNQUOTED(VERSION, "$VERSION", [Version number of package]))
AC_REQUIRE([AM_SANITY_CHECK])
AC_REQUIRE([AC_ARG_PROGRAM])
AM_MISSING_PROG(ACLOCAL, aclocal)
AM_MISSING_PROG(AUTOCONF, autoconf)
AM_MISSING_PROG(AUTOMAKE, automake)
AM_MISSING_PROG(AUTOHEADER, autoheader)
AM_MISSING_PROG(MAKEINFO, makeinfo)
AM_MISSING_PROG(AMTAR, tar)
AM_MISSING_INSTALL_SH
dnl We need awk for the "check" target.  The system "awk" is bad on
dnl some platforms.
AC_REQUIRE([AC_PROG_AWK])
AC_REQUIRE([AC_PROG_MAKE_SET])
AC_REQUIRE([AM_DEP_TRACK])
AC_REQUIRE([AM_SET_DEPDIR])
ifdef([AC_PROVIDE_AC_PROG_CC], [AM_DEPENDENCIES(CC)], [
   define([AC_PROG_CC], defn([AC_PROG_CC])[AM_DEPENDENCIES(CC)])])
ifdef([AC_PROVIDE_AC_PROG_CXX], [AM_DEPENDENCIES(CXX)], [
   define([AC_PROG_CXX], defn([AC_PROG_CXX])[AM_DEPENDENCIES(CXX)])])
])

#
# Check to make sure that the build environment is sane.
#

AC_DEFUN(AM_SANITY_CHECK,
[AC_MSG_CHECKING([whether build environment is sane])
# Just in case
sleep 1
echo timestamp > conftestfile
# Do `set' in a subshell so we don't clobber the current shell's
# arguments.  Must try -L first in case configure is actually a
# symlink; some systems play weird games with the mod time of symlinks
# (eg FreeBSD returns the mod time of the symlink's containing
# directory).
if (
   set X `ls -Lt $srcdir/configure conftestfile 2> /dev/null`
   if test "[$]*" = "X"; then
      # -L didn't work.
      set X `ls -t $srcdir/configure conftestfile`
   fi
   if test "[$]*" != "X $srcdir/configure conftestfile" \
      && test "[$]*" != "X conftestfile $srcdir/configure"; then

      # If neither matched, then we have a broken ls.  This can happen
      # if, for instance, CONFIG_SHELL is bash and it inherits a
      # broken ls alias from the environment.  This has actually
      # happened.  Such a system could not be considered "sane".
      AC_MSG_ERROR([ls -t appears to fail.  Make sure there is not a broken
alias in your environment])
   fi

   test "[$]2" = conftestfile
   )
then
   # Ok.
   :
else
   AC_MSG_ERROR([newly created file is older than distributed files!
Check your system clock])
fi
rm -f conftest*
AC_MSG_RESULT(yes)])

dnl AM_MISSING_PROG(NAME, PROGRAM)
AC_DEFUN(AM_MISSING_PROG, [
AC_REQUIRE([AM_MISSING_HAS_RUN])
$1=${$1-"${am_missing_run}$2"}
AC_SUBST($1)])

dnl Like AM_MISSING_PROG, but only looks for install-sh.
dnl AM_MISSING_INSTALL_SH()
AC_DEFUN(AM_MISSING_INSTALL_SH, [
AC_REQUIRE([AM_MISSING_HAS_RUN])
if test -z "$install_sh"; then
   install_sh="$ac_aux_dir/install-sh"
   test -f "$install_sh" || install_sh="$ac_aux_dir/install.sh"
   test -f "$install_sh" || install_sh="${am_missing_run}${ac_auxdir}/install-sh"
   dnl FIXME: an evil hack: we remove the SHELL invocation from
   dnl install_sh because automake adds it back in.  Sigh.
   install_sh="`echo $install_sh | sed -e 's/\${SHELL}//'`"
fi
AC_SUBST(install_sh)])

dnl AM_MISSING_HAS_RUN.
dnl Define MISSING if not defined so far and test if it supports --run.
dnl If it does, set am_missing_run to use it, otherwise, to nothing.
AC_DEFUN([AM_MISSING_HAS_RUN], [
test x"${MISSING+set}" = xset || \
  MISSING="\${SHELL} `CDPATH=: && cd $ac_aux_dir && pwd`/missing"
dnl Use eval to expand $SHELL
if eval "$MISSING --run :"; then
  am_missing_run="$MISSING --run "
else
  am_missing_run=
  am_backtick='`'
  AC_MSG_WARN([${am_backtick}missing' script is too old or missing])
fi
])

dnl See how the compiler implements dependency checking.
dnl Usage:
dnl AM_DEPENDENCIES(NAME)
dnl NAME is "CC", "CXX" or "OBJC".

dnl We try a few techniques and use that to set a single cache variable.

AC_DEFUN(AM_DEPENDENCIES,[
AC_REQUIRE([AM_SET_DEPDIR])
AC_REQUIRE([AM_OUTPUT_DEPENDENCY_COMMANDS])
ifelse([$1],CC,[
AC_REQUIRE([AC_PROG_CC])
AC_REQUIRE([AC_PROG_CPP])
depcc="$CC"
depcpp="$CPP"],[$1],CXX,[
AC_REQUIRE([AC_PROG_CXX])
AC_REQUIRE([AC_PROG_CXXCPP])
depcc="$CXX"
depcpp="$CXXCPP"],[$1],OBJC,[
am_cv_OBJC_dependencies_compiler_type=gcc],[
AC_REQUIRE([AC_PROG_][$1])
depcc="$[$1]"
depcpp=""])
AC_MSG_CHECKING([dependency style of $depcc])
AC_CACHE_VAL(am_cv_[$1]_dependencies_compiler_type,[
if test -z "$AMDEP"; then
  echo '#include "conftest.h"' > conftest.c
  echo 'int i;' > conftest.h

  am_cv_[$1]_dependencies_compiler_type=none
  for depmode in `sed -n 's/^#*\([a-zA-Z0-9]*\))$/\1/p' < "$am_depcomp"`; do
    case "$depmode" in
    nosideeffect)
      # after this tag, mechanisms are not by side-effect, so they'll
      # only be used when explicitly requested
      if test "x$enable_dependency_tracking" = xyes; then
	continue
      else
	break
      fi
      ;;
    none) break ;;
    esac
    if depmode="$depmode" \
       source=conftest.c object=conftest.o \
       depfile=conftest.Po tmpdepfile=conftest.TPo \
       $SHELL $am_depcomp $depcc -c conftest.c 2>/dev/null &&
       grep conftest.h conftest.Po > /dev/null 2>&1; then
      am_cv_[$1]_dependencies_compiler_type="$depmode"
      break
    fi
  done

  rm -f conftest.*
else
  am_cv_[$1]_dependencies_compiler_type=none
fi
])
AC_MSG_RESULT($am_cv_[$1]_dependencies_compiler_type)
[$1]DEPMODE="depmode=$am_cv_[$1]_dependencies_compiler_type"
AC_SUBST([$1]DEPMODE)
])

dnl Choose a directory name for dependency files.
dnl This macro is AC_REQUIREd in AM_DEPENDENCIES

AC_DEFUN(AM_SET_DEPDIR,[
if test -d .deps || mkdir .deps 2> /dev/null || test -d .deps; then
  DEPDIR=.deps
else
  DEPDIR=_deps
fi
AC_SUBST(DEPDIR)
])

AC_DEFUN(AM_DEP_TRACK,[
AC_ARG_ENABLE(dependency-tracking,
[  --disable-dependency-tracking Speeds up one-time builds
  --enable-dependency-tracking  Do not reject slow dependency extractors])
if test "x$enable_dependency_tracking" = xno; then
  AMDEP="#"
else
  am_depcomp="$ac_aux_dir/depcomp"
  if test ! -f "$am_depcomp"; then
    AMDEP="#"
  else
    AMDEP=
  fi
fi
AC_SUBST(AMDEP)
if test -z "$AMDEP"; then
  AMDEPBACKSLASH='\'
else
  AMDEPBACKSLASH=
fi
pushdef([subst], defn([AC_SUBST]))
subst(AMDEPBACKSLASH)
popdef([subst])
])

dnl Generate code to set up dependency tracking.
dnl This macro should only be invoked once -- use via AC_REQUIRE.
dnl Usage:
dnl AM_OUTPUT_DEPENDENCY_COMMANDS

dnl
dnl This code is only required when automatic dependency tracking
dnl is enabled.  FIXME.  This creates each `.P' file that we will
dnl need in order to bootstrap the dependency handling code.
AC_DEFUN(AM_OUTPUT_DEPENDENCY_COMMANDS,[
AC_OUTPUT_COMMANDS([
test x"$AMDEP" != x"" ||
for mf in $CONFIG_FILES; do
  case "$mf" in
  Makefile) dirpart=.;;
  */Makefile) dirpart=`echo "$mf" | sed -e 's|/[^/]*$||'`;;
  *) continue;;
  esac
  grep '^DEP_FILES *= *[^ #]' < "$mf" > /dev/null || continue
  # Extract the definition of DEP_FILES from the Makefile without
  # running `make'.
  DEPDIR=`sed -n -e '/^DEPDIR = / s///p' < "$mf"`
  test -z "$DEPDIR" && continue
  # When using ansi2knr, U may be empty or an underscore; expand it
  U=`sed -n -e '/^U = / s///p' < "$mf"`
  test -d "$dirpart/$DEPDIR" || mkdir "$dirpart/$DEPDIR"
  # We invoke sed twice because it is the simplest approach to
  # changing $(DEPDIR) to its actual value in the expansion.
  for file in `sed -n -e '
    /^DEP_FILES = .*\\\\$/ {
      s/^DEP_FILES = //
      :loop
	s/\\\\$//
	p
	n
	/\\\\$/ b loop
      p
    }
    /^DEP_FILES = / s/^DEP_FILES = //p' < "$mf" | \
       sed -e 's/\$(DEPDIR)/'"$DEPDIR"'/g' -e 's/\$U/'"$U"'/g'`; do
    # Make sure the directory exists.
    test -f "$dirpart/$file" && continue
    fdir=`echo "$file" | sed -e 's|/[^/]*$||'`
    $ac_aux_dir/mkinstalldirs "$dirpart/$fdir" > /dev/null 2>&1
    # echo "creating $dirpart/$file"
    echo '# dummy' > "$dirpart/$file"
  done
done
], [AMDEP="$AMDEP"
ac_aux_dir="$ac_aux_dir"])])

# Like AC_CONFIG_HEADER, but automatically create stamp file.

AC_DEFUN(AM_CONFIG_HEADER,
[AC_PREREQ([2.12])
AC_CONFIG_HEADER([$1])
dnl When config.status generates a header, we must update the stamp-h file.
dnl This file resides in the same directory as the config header
dnl that is generated.  We must strip everything past the first ":",
dnl and everything past the last "/".
AC_OUTPUT_COMMANDS(changequote(<<,>>)dnl
ifelse(patsubst(<<$1>>, <<[^ ]>>, <<>>), <<>>,
<<test -z "<<$>>CONFIG_HEADERS" || echo timestamp > patsubst(<<$1>>, <<^\([^:]*/\)?.*>>, <<\1>>)stamp-h<<>>dnl>>,
<<am_indx=1
for am_file in <<$1>>; do
  case " <<$>>CONFIG_HEADERS " in
  *" <<$>>am_file "*<<)>>
    echo timestamp > `echo <<$>>am_file | sed -e 's%:.*%%' -e 's%[^/]*$%%'`stamp-h$am_indx
    ;;
  esac
  am_indx=`expr "<<$>>am_indx" + 1`
done<<>>dnl>>)
changequote([,]))])

# Add --enable-maintainer-mode option to configure.
# From Jim Meyering

# serial 1

AC_DEFUN(AM_MAINTAINER_MODE,
[AC_MSG_CHECKING([whether to enable maintainer-specific portions of Makefiles])
  dnl maintainer-mode is disabled by default
  AC_ARG_ENABLE(maintainer-mode,
[  --enable-maintainer-mode enable make rules and dependencies not useful
                          (and sometimes confusing) to the casual installer],
      USE_MAINTAINER_MODE=$enableval,
      USE_MAINTAINER_MODE=no)
  AC_MSG_RESULT($USE_MAINTAINER_MODE)
  AM_CONDITIONAL(MAINTAINER_MODE, test $USE_MAINTAINER_MODE = yes)
  MAINT=$MAINTAINER_MODE_TRUE
  AC_SUBST(MAINT)dnl
]
)

# Define a conditional.

AC_DEFUN(AM_CONDITIONAL,
[AC_SUBST($1_TRUE)
AC_SUBST($1_FALSE)
if $2; then
  $1_TRUE=
  $1_FALSE='#'
else
  $1_TRUE='#'
  $1_FALSE=
fi])


# serial 42 AC_PROG_LIBTOOL
AC_DEFUN(AC_PROG_LIBTOOL,
[AC_REQUIRE([AC_LIBTOOL_SETUP])dnl

# Save cache, so that ltconfig can load it
AC_CACHE_SAVE

# Actually configure libtool.  ac_aux_dir is where install-sh is found.
AR="$AR" CC="$CC" CFLAGS="$CFLAGS" CPPFLAGS="$CPPFLAGS" \
MAGIC="$MAGIC" LD="$LD" LDFLAGS="$LDFLAGS" LIBS="$LIBS" \
LN_S="$LN_S" NM="$NM" RANLIB="$RANLIB" STRIP="$STRIP" \
AS="$AS" DLLTOOL="$DLLTOOL" OBJDUMP="$OBJDUMP" \
objext="$OBJEXT" exeext="$EXEEXT" reload_flag="$reload_flag" \
deplibs_check_method="$deplibs_check_method" file_magic_cmd="$file_magic_cmd" \
${CONFIG_SHELL-/bin/sh} $ac_aux_dir/ltconfig --no-reexec \
$libtool_flags --no-verify --build="$build" $ac_aux_dir/ltmain.sh $lt_target \
|| AC_MSG_ERROR([libtool configure failed])

# Reload cache, that may have been modified by ltconfig
AC_CACHE_LOAD

# This can be used to rebuild libtool when needed
LIBTOOL_DEPS="$ac_aux_dir/ltconfig $ac_aux_dir/ltmain.sh"

# Always use our own libtool.
LIBTOOL='$(SHELL) $(top_builddir)/libtool'
AC_SUBST(LIBTOOL)dnl

# Redirect the config.log output again, so that the ltconfig log is not
# clobbered by the next message.
exec 5>>./config.log
])

AC_DEFUN(AC_LIBTOOL_SETUP,
[AC_PREREQ(2.13)dnl
AC_REQUIRE([AC_ENABLE_SHARED])dnl
AC_REQUIRE([AC_ENABLE_STATIC])dnl
AC_REQUIRE([AC_ENABLE_FAST_INSTALL])dnl
AC_REQUIRE([AC_CANONICAL_HOST])dnl
AC_REQUIRE([AC_CANONICAL_BUILD])dnl
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_PROG_LD])dnl
AC_REQUIRE([AC_PROG_LD_RELOAD_FLAG])dnl
AC_REQUIRE([AC_PROG_NM])dnl
AC_REQUIRE([AC_PROG_LN_S])dnl
AC_REQUIRE([AC_DEPLIBS_CHECK_METHOD])dnl
AC_REQUIRE([AC_OBJEXT])dnl
AC_REQUIRE([AC_EXEEXT])dnl
dnl

# Only perform the check for file, if the check method requires it
case "$deplibs_check_method" in
file_magic*)
  if test "$file_magic_cmd" = '${MAGIC}'; then
    AC_PATH_MAGIC
  fi
  ;;
esac

case "$target" in
NONE) lt_target="$host" ;;
*) lt_target="$target" ;;
esac

AC_CHECK_TOOL(RANLIB, ranlib, :)
AC_CHECK_TOOL(STRIP, strip, :)

# Check for any special flags to pass to ltconfig.
libtool_flags="--cache-file=$cache_file"
test "$enable_shared" = no && libtool_flags="$libtool_flags --disable-shared"
test "$enable_static" = no && libtool_flags="$libtool_flags --disable-static"
test "$enable_fast_install" = no && libtool_flags="$libtool_flags --disable-fast-install"
test "$ac_cv_prog_gcc" = yes && libtool_flags="$libtool_flags --with-gcc"
test "$ac_cv_prog_gnu_ld" = yes && libtool_flags="$libtool_flags --with-gnu-ld"
ifdef([AC_PROVIDE_AC_LIBTOOL_DLOPEN],
[libtool_flags="$libtool_flags --enable-dlopen"])
ifdef([AC_PROVIDE_AC_LIBTOOL_WIN32_DLL],
[libtool_flags="$libtool_flags --enable-win32-dll"])
AC_ARG_ENABLE(libtool-lock,
  [  --disable-libtool-lock  avoid locking (might break parallel builds)])
test "x$enable_libtool_lock" = xno && libtool_flags="$libtool_flags --disable-lock"
test x"$silent" = xyes && libtool_flags="$libtool_flags --silent"

AC_ARG_WITH(pic,
  [  --with-pic              try to use only PIC/non-PIC objects [default=use both]],
     pic_mode="$withval", pic_mode=default)
test x"$pic_mode" = xyes && libtool_flags="$libtool_flags --prefer-pic"
test x"$pic_mode" = xno && libtool_flags="$libtool_flags --prefer-non-pic"

# Some flags need to be propagated to the compiler or linker for good
# libtool support.
case "$lt_target" in
*-*-irix6*)
  # Find out which ABI we are using.
  echo '[#]line __oline__ "configure"' > conftest.$ac_ext
  if AC_TRY_EVAL(ac_compile); then
    case "`/usr/bin/file conftest.o`" in
    *32-bit*)
      LD="${LD-ld} -32"
      ;;
    *N32*)
      LD="${LD-ld} -n32"
      ;;
    *64-bit*)
      LD="${LD-ld} -64"
      ;;
    esac
  fi
  rm -rf conftest*
  ;;

*-*-sco3.2v5*)
  # On SCO OpenServer 5, we need -belf to get full-featured binaries.
  SAVE_CFLAGS="$CFLAGS"
  CFLAGS="$CFLAGS -belf"
  AC_CACHE_CHECK([whether the C compiler needs -belf], lt_cv_cc_needs_belf,
    [AC_LANG_SAVE
     AC_LANG_C
     AC_TRY_LINK([],[],[lt_cv_cc_needs_belf=yes],[lt_cv_cc_needs_belf=no])
     AC_LANG_RESTORE])
  if test x"$lt_cv_cc_needs_belf" != x"yes"; then
    # this is probably gcc 2.8.0, egcs 1.0 or newer; no need for -belf
    CFLAGS="$SAVE_CFLAGS"
  fi
  ;;

ifdef([AC_PROVIDE_AC_LIBTOOL_WIN32_DLL],
[*-*-cygwin* | *-*-mingw*)
  AC_CHECK_TOOL(DLLTOOL, dlltool, false)
  AC_CHECK_TOOL(AS, as, false)
  AC_CHECK_TOOL(OBJDUMP, objdump, false)

  # recent cygwin and mingw systems supply a stub DllMain which the user
  # can override, but on older systems we have to supply one
  AC_CACHE_CHECK([if libtool should supply DllMain function], lt_cv_need_dllmain,
    [AC_TRY_LINK([],
      [extern int __attribute__((__stdcall__)) DllMain(void*, int, void*);
      DllMain (0, 0, 0);],
      [lt_cv_need_dllmain=no],[lt_cv_need_dllmain=yes])])

  case "$lt_target/$CC" in
  *-*-cygwin*/gcc*-mno-cygwin*|*-*-mingw*)
    # old mingw systems require "-dll" to link a DLL, while more recent ones
    # require "-mdll"
    SAVE_CFLAGS="$CFLAGS"
    CFLAGS="$CFLAGS -mdll"
    AC_CACHE_CHECK([how to link DLLs], lt_cv_cc_dll_switch,
      [AC_TRY_LINK([], [], [lt_cv_cc_dll_switch=-mdll],[lt_cv_cc_dll_switch=-dll])])
    CFLAGS="$SAVE_CFLAGS" ;;
  *-*-cygwin*)
    # cygwin systems need to pass --dll to the linker, and not link
    # crt.o which will require a WinMain@16 definition.
    lt_cv_cc_dll_switch="-Wl,--dll -nostartfiles" ;;
  esac
  ;;
  ])
esac
])

# AC_LIBTOOL_DLOPEN - enable checks for dlopen support
AC_DEFUN(AC_LIBTOOL_DLOPEN, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])])

# AC_LIBTOOL_WIN32_DLL - declare package support for building win32 dll's
AC_DEFUN(AC_LIBTOOL_WIN32_DLL, [AC_BEFORE([$0], [AC_LIBTOOL_SETUP])])

# AC_ENABLE_SHARED - implement the --enable-shared flag
# Usage: AC_ENABLE_SHARED[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN(AC_ENABLE_SHARED, [dnl
define([AC_ENABLE_SHARED_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(shared,
changequote(<<, >>)dnl
<<  --enable-shared[=PKGS]  build shared libraries [default=>>AC_ENABLE_SHARED_DEFAULT],
changequote([, ])dnl
[p=${PACKAGE-default}
case "$enableval" in
yes) enable_shared=yes ;;
no) enable_shared=no ;;
*)
  enable_shared=no
  # Look at the argument we got.  We use all the common list separators.
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
  for pkg in $enableval; do
    if test "X$pkg" = "X$p"; then
      enable_shared=yes
    fi
  done
  IFS="$ac_save_ifs"
  ;;
esac],
enable_shared=AC_ENABLE_SHARED_DEFAULT)dnl
])

# AC_DISABLE_SHARED - set the default shared flag to --disable-shared
AC_DEFUN(AC_DISABLE_SHARED, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_SHARED(no)])

# AC_ENABLE_STATIC - implement the --enable-static flag
# Usage: AC_ENABLE_STATIC[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN(AC_ENABLE_STATIC, [dnl
define([AC_ENABLE_STATIC_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(static,
changequote(<<, >>)dnl
<<  --enable-static[=PKGS]  build static libraries [default=>>AC_ENABLE_STATIC_DEFAULT],
changequote([, ])dnl
[p=${PACKAGE-default}
case "$enableval" in
yes) enable_static=yes ;;
no) enable_static=no ;;
*)
  enable_static=no
  # Look at the argument we got.  We use all the common list separators.
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
  for pkg in $enableval; do
    if test "X$pkg" = "X$p"; then
      enable_static=yes
    fi
  done
  IFS="$ac_save_ifs"
  ;;
esac],
enable_static=AC_ENABLE_STATIC_DEFAULT)dnl
])

# AC_DISABLE_STATIC - set the default static flag to --disable-static
AC_DEFUN(AC_DISABLE_STATIC, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_STATIC(no)])


# AC_ENABLE_FAST_INSTALL - implement the --enable-fast-install flag
# Usage: AC_ENABLE_FAST_INSTALL[(DEFAULT)]
#   Where DEFAULT is either `yes' or `no'.  If omitted, it defaults to
#   `yes'.
AC_DEFUN(AC_ENABLE_FAST_INSTALL, [dnl
define([AC_ENABLE_FAST_INSTALL_DEFAULT], ifelse($1, no, no, yes))dnl
AC_ARG_ENABLE(fast-install,
changequote(<<, >>)dnl
<<  --enable-fast-install[=PKGS]  optimize for fast installation [default=>>AC_ENABLE_FAST_INSTALL_DEFAULT],
changequote([, ])dnl
[p=${PACKAGE-default}
case "$enableval" in
yes) enable_fast_install=yes ;;
no) enable_fast_install=no ;;
*)
  enable_fast_install=no
  # Look at the argument we got.  We use all the common list separators.
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}:,"
  for pkg in $enableval; do
    if test "X$pkg" = "X$p"; then
      enable_fast_install=yes
    fi
  done
  IFS="$ac_save_ifs"
  ;;
esac],
enable_fast_install=AC_ENABLE_FAST_INSTALL_DEFAULT)dnl
])

# AC_ENABLE_FAST_INSTALL - set the default to --disable-fast-install
AC_DEFUN(AC_DISABLE_FAST_INSTALL, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
AC_ENABLE_FAST_INSTALL(no)])


# AC_PATH_TOOL_PREFIX - find a file program which can recognise shared library
AC_DEFUN(AC_PATH_TOOL_PREFIX,
[AC_MSG_CHECKING([for $1])
AC_CACHE_VAL(lt_cv_path_MAGIC,
[case "$MAGIC" in
  /*)
  lt_cv_path_MAGIC="$MAGIC" # Let the user override the test with a path.
  ;;
  ?:/*)
  ac_cv_path_MAGIC="$MAGIC" # Let the user override the test with a dos path.
  ;;
  *)
  ac_save_MAGIC="$MAGIC"
  IFS="${IFS=   }"; ac_save_ifs="$IFS"; IFS=":"
dnl $ac_dummy forces splitting on constant user-supplied paths.
dnl POSIX.2 word splitting is done only on the output of word expansions,
dnl not every word.  This closes a longstanding sh security hole.
  ac_dummy="ifelse([$2], , $PATH, [$2])"
  for ac_dir in $ac_dummy; do
    test -z "$ac_dir" && ac_dir=.
    if test -f $ac_dir/$1; then
      lt_cv_path_MAGIC="$ac_dir/$1"
      if test -n "$file_magic_test_file"; then
	case "$deplibs_check_method" in
	"file_magic "*)
	  file_magic_regex="`expr \"$deplibs_check_method\" : \"file_magic \(.*\)\"`"
	  MAGIC="$lt_cv_path_MAGIC"
	  if eval $file_magic_cmd \$file_magic_test_file 2> /dev/null |
	    egrep "$file_magic_regex" > /dev/null; then
	    :
	  else
	    cat <<EOF 1>&2

*** Warning: the command libtool uses to detect shared libraries,
*** $file_magic_cmd, produces output that libtool cannot recognize.
*** The result is that libtool may fail to recognize shared libraries
*** as such.  This will affect the creation of libtool libraries that
*** depend on shared libraries, but programs linked with such libtool
*** libraries will work regardless of this problem.  Nevertheless, you
*** may want to report the problem to your system manager and/or to
*** bug-libtool@gnu.org

EOF
	  fi ;;
	esac
      fi
      break
    fi
  done
  IFS="$ac_save_ifs"
  MAGIC="$ac_save_MAGIC"
  ;;
esac])
MAGIC="$lt_cv_path_MAGIC"
if test -n "$MAGIC"; then
  AC_MSG_RESULT($MAGIC)
else
  AC_MSG_RESULT(no)
fi
])


# AC_PATH_MAGIC - find a file program which can recognise a shared library
AC_DEFUN(AC_PATH_MAGIC,
[AC_REQUIRE([AC_CHECK_TOOL_PREFIX])dnl
AC_PATH_TOOL_PREFIX(${ac_tool_prefix}file, /usr/bin:$PATH)
if test -z "$lt_cv_path_MAGIC"; then
  if test -n "$ac_tool_prefix"; then
    AC_PATH_TOOL_PREFIX(file, /usr/bin:$PATH)
  else
    MAGIC=:
  fi
fi
])


# AC_PROG_LD - find the path to the GNU or non-GNU linker
AC_DEFUN(AC_PROG_LD,
[AC_ARG_WITH(gnu-ld,
[  --with-gnu-ld           assume the C compiler uses GNU ld [default=no]],
test "$withval" = no || with_gnu_ld=yes, with_gnu_ld=no)
AC_REQUIRE([AC_PROG_CC])dnl
AC_REQUIRE([AC_CANONICAL_HOST])dnl
AC_REQUIRE([AC_CANONICAL_BUILD])dnl
ac_prog=ld
if test "$ac_cv_prog_gcc" = yes; then
  # Check if gcc -print-prog-name=ld gives a path.
  AC_MSG_CHECKING([for ld used by GCC])
  case $lt_target in
  *-*-mingw*)
    # gcc leaves a trailing carriage return which upsets mingw
    ac_prog=`($CC -print-prog-name=ld) 2>&5 | tr -d '\015'` ;;
  *)
    ac_prog=`($CC -print-prog-name=ld) 2>&5` ;;
  esac
  case "$ac_prog" in
    # Accept absolute paths.
changequote(,)dnl
    [\\/]* | [A-Za-z]:[\\/]*)
      re_direlt='/[^/][^/]*/\.\./'
changequote([,])dnl
      # Canonicalize the path of ld
      ac_prog=`echo $ac_prog| sed 's%\\\\%/%g'`
      while echo $ac_prog | grep "$re_direlt" > /dev/null 2>&1; do
	ac_prog=`echo $ac_prog| sed "s%$re_direlt%/%"`
      done
      test -z "$LD" && LD="$ac_prog"
      ;;
  "")
    # If it fails, then pretend we aren't using GCC.
    ac_prog=ld
    ;;
  *)
    # If it is relative, then search for the first ld in PATH.
    with_gnu_ld=unknown
    ;;
  esac
elif test "$with_gnu_ld" = yes; then
  AC_MSG_CHECKING([for GNU ld])
else
  AC_MSG_CHECKING([for non-GNU ld])
fi
AC_CACHE_VAL(ac_cv_path_LD,
[if test -z "$LD"; then
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}${PATH_SEPARATOR-:}"
  for ac_dir in $PATH; do
    test -z "$ac_dir" && ac_dir=.
    if test -f "$ac_dir/$ac_prog" || test -f "$ac_dir/$ac_prog$ac_exeext"; then
      ac_cv_path_LD="$ac_dir/$ac_prog"
      # Check to see if the program is GNU ld.  I'd rather use --version,
      # but apparently some GNU ld's only accept -v.
      # Break only if it was the GNU/non-GNU ld that we prefer.
      if "$ac_cv_path_LD" -v 2>&1 < /dev/null | egrep '(GNU|with BFD)' > /dev/null; then
	test "$with_gnu_ld" != no && break
      else
	test "$with_gnu_ld" != yes && break
      fi
    fi
  done
  IFS="$ac_save_ifs"
else
  ac_cv_path_LD="$LD" # Let the user override the test with a path.
fi])
LD="$ac_cv_path_LD"
if test -n "$LD"; then
  AC_MSG_RESULT($LD)
else
  AC_MSG_RESULT(no)
fi
test -z "$LD" && AC_MSG_ERROR([no acceptable ld found in \$PATH])
AC_PROG_LD_GNU
])

AC_DEFUN(AC_PROG_LD_GNU,
[AC_CACHE_CHECK([if the linker ($LD) is GNU ld], ac_cv_prog_gnu_ld,
[# I'd rather use --version here, but apparently some GNU ld's only accept -v.
if $LD -v 2>&1 </dev/null | egrep '(GNU|with BFD)' 1>&5; then
  ac_cv_prog_gnu_ld=yes
else
  ac_cv_prog_gnu_ld=no
fi])
with_gnu_ld=$ac_cv_prog_gnu_ld
])

# AC_PROG_LD_RELOAD_FLAG - find reload flag for linker
#   -- PORTME Some linkers may need a different reload flag.
AC_DEFUN(AC_PROG_LD_RELOAD_FLAG,
[AC_CACHE_CHECK([for $LD option to reload object files], lt_cv_ld_reload_flag,
[lt_cv_ld_reload_flag='-r'])
reload_flag=$lt_cv_ld_reload_flag
test -n "$reload_flag" && reload_flag=" $reload_flag"
])

# AC_DEPLIBS_CHECK_METHOD - how to check for library dependencies
#  -- PORTME fill in with the dynamic library characteristics
AC_DEFUN(AC_DEPLIBS_CHECK_METHOD,
[AC_CACHE_CHECK([how to recognise dependant libraries],
lt_cv_deplibs_check_method,
[lt_cv_file_magic_cmd='${MAGIC}'
lt_cv_file_magic_test_file=
lt_cv_deplibs_check_method='unknown'
# Need to set the preceding variable on all platforms that support
# interlibrary dependencies.
# 'none' -- dependencies not supported.
# `unknown' -- same as none, but documents that we really don't know.
# 'pass_all' -- all dependencies passed with no checks.
# 'test_compile' -- check by making test program.
# 'file_magic [regex]' -- check by looking for files in library path
# which responds to the $file_magic_cmd with a given egrep regex.
# If you have `file' or equivalent on your system and you're not sure
# whether `pass_all' will *always* work, you probably want this one.

case "$host_os" in
aix4* | beos*)
  lt_cv_deplibs_check_method=pass_all
  ;;

bsdi4*)
  changequote(,)dnl
  lt_cv_deplibs_check_method='file_magic ELF [0-9][0-9]*-bit [ML]SB (shared object|dynamic lib)'
  changequote([, ])dnl
  lt_cv_file_magic_test_file=/shlib/libc.so
  ;;

cygwin* | mingw*)
  lt_cv_deplibs_check_method='file_magic file format pei*-i386(.*architecture: i386)?'
  lt_cv_file_magic_cmd='${OBJDUMP} -f'
  ;;

freebsd*)
  case "$version_type" in
  freebsd-elf*)
    lt_cv_deplibs_check_method=pass_all
    ;;
  esac
  ;;

gnu*)
  lt_cv_deplibs_check_method=pass_all
  ;;

irix5* | irix6*)
  case "$host_os" in
  irix5*)
    # this will be overridden with pass_all, but let us keep it just in case
    lt_cv_deplibs_check_method="file_magic ELF 32-bit MSB dynamic lib MIPS - version 1"
    ;;
  *)
    case "$LD" in
    *-32|*"-32 ") libmagic=32-bit;;
    *-n32|*"-n32 ") libmagic=N32;;
    *-64|*"-64 ") libmagic=64-bit;;
    *) libmagic=never-match;;
    esac
    # this will be overridden with pass_all, but let us keep it just in case
    changequote(,)dnl
    lt_cv_deplibs_check_method="file_magic ELF ${libmagic} MSB mips-[1234] dynamic lib MIPS - version 1"
    changequote([, ])dnl
    ;;
  esac
  lt_cv_file_magic_test_file=`echo /lib${libsuff}/libc.so*`
  lt_cv_deplibs_check_method=pass_all
  ;;

# This must be Linux ELF.
linux-gnu*)
  case "$host_cpu" in
  alpha* | i*86 | powerpc* | sparc* )
    lt_cv_deplibs_check_method=pass_all ;;
  *)
    # glibc up to 2.1.1 does not perform some relocations on ARM
    changequote(,)dnl
    lt_cv_deplibs_check_method='file_magic ELF [0-9][0-9]*-bit [LM]SB (shared object|dynamic lib )' ;;
    changequote([, ])dnl
  esac
  lt_cv_file_magic_test_file=`echo /lib/libc.so* /lib/libc-*.so`
  ;;

osf3* | osf4* | osf5*)
  # this will be overridden with pass_all, but let us keep it just in case
  lt_cv_deplibs_check_method='file_magic COFF format alpha shared library'
  lt_cv_file_magic_test_file=/shlib/libc.so
  lt_cv_deplibs_check_method=pass_all
  ;;

sco3.2v5*)
  lt_cv_deplibs_check_method=pass_all
  ;;

solaris*)
  lt_cv_deplibs_check_method=pass_all
  lt_cv_file_magic_test_file=/lib/libc.so
  ;;

sysv4 | sysv4.2uw2* | sysv4.3* | sysv5*)
  case "$host_vendor" in
  ncr)
    lt_cv_deplibs_check_method=pass_all
    ;;
  motorola)
    changequote(,)dnl
    lt_cv_deplibs_check_method='file_magic ELF [0-9][0-9]*-bit [ML]SB (shared object|dynamic lib) M[0-9][0-9]* Version [0-9]'
    changequote([, ])dnl
    lt_cv_file_magic_test_file=`echo /usr/lib/libc.so*`
    ;;
  esac
  ;;
esac
])
file_magic_cmd=$lt_cv_file_magic_cmd
deplibs_check_method=$lt_cv_deplibs_check_method
])


# AC_PROG_NM - find the path to a BSD-compatible name lister
AC_DEFUN(AC_PROG_NM,
[AC_MSG_CHECKING([for BSD-compatible nm])
AC_CACHE_VAL(ac_cv_path_NM,
[if test -n "$NM"; then
  # Let the user override the test.
  ac_cv_path_NM="$NM"
else
  IFS="${IFS= 	}"; ac_save_ifs="$IFS"; IFS="${IFS}${PATH_SEPARATOR-:}"
  for ac_dir in $PATH /usr/ccs/bin /usr/ucb /bin; do
    test -z "$ac_dir" && ac_dir=.
    if test -f $ac_dir/nm || test -f $ac_dir/nm$ac_exeext ; then
      # Check to see if the nm accepts a BSD-compat flag.
      # Adding the `sed 1q' prevents false positives on HP-UX, which says:
      #   nm: unknown option "B" ignored
      if ($ac_dir/nm -B /dev/null 2>&1 | sed '1q'; exit 0) | egrep /dev/null >/dev/null; then
	ac_cv_path_NM="$ac_dir/nm -B"
	break
      elif ($ac_dir/nm -p /dev/null 2>&1 | sed '1q'; exit 0) | egrep /dev/null >/dev/null; then
	ac_cv_path_NM="$ac_dir/nm -p"
	break
      else
	ac_cv_path_NM=${ac_cv_path_NM="$ac_dir/nm"} # keep the first match, but
	continue # so that we can try to find one that supports BSD flags
      fi
    fi
  done
  IFS="$ac_save_ifs"
  test -z "$ac_cv_path_NM" && ac_cv_path_NM=nm
fi])
NM="$ac_cv_path_NM"
AC_MSG_RESULT([$NM])
])

# AC_CHECK_LIBM - check for math library
AC_DEFUN(AC_CHECK_LIBM,
[AC_REQUIRE([AC_CANONICAL_HOST])dnl
LIBM=
case "$lt_target" in
*-*-beos* | *-*-cygwin*)
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

# AC_LIBLTDL_CONVENIENCE[(dir)] - sets LIBLTDL to the link flags for
# the libltdl convenience library, adds --enable-ltdl-convenience to
# the configure arguments.  Note that LIBLTDL is not AC_SUBSTed, nor
# is AC_CONFIG_SUBDIRS called.  If DIR is not provided, it is assumed
# to be `${top_builddir}/libltdl'.  Make sure you start DIR with
# '${top_builddir}/' (note the single quotes!) if your package is not
# flat, and, if you're not using automake, define top_builddir as
# appropriate in the Makefiles.
AC_DEFUN(AC_LIBLTDL_CONVENIENCE, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
  case "$enable_ltdl_convenience" in
  no) AC_MSG_ERROR([this package needs a convenience libltdl]) ;;
  "") enable_ltdl_convenience=yes
      ac_configure_args="$ac_configure_args --enable-ltdl-convenience" ;;
  esac
  LIBLTDL=ifelse($#,1,$1,['${top_builddir}/libltdl'])/libltdlc.la
  INCLTDL=ifelse($#,1,-I$1,['-I${top_srcdir}/libltdl'])
])

# AC_LIBLTDL_INSTALLABLE[(dir)] - sets LIBLTDL to the link flags for
# the libltdl installable library, and adds --enable-ltdl-install to
# the configure arguments.  Note that LIBLTDL is not AC_SUBSTed, nor
# is AC_CONFIG_SUBDIRS called.  If DIR is not provided, it is assumed
# to be `${top_builddir}/libltdl'.  Make sure you start DIR with
# '${top_builddir}/' (note the single quotes!) if your package is not
# flat, and, if you're not using automake, define top_builddir as
# appropriate in the Makefiles.
# In the future, this macro may have to be called after AC_PROG_LIBTOOL.
AC_DEFUN(AC_LIBLTDL_INSTALLABLE, [AC_BEFORE([$0],[AC_LIBTOOL_SETUP])dnl
  AC_CHECK_LIB(ltdl, main,
  [test x"$enable_ltdl_install" != xyes && enable_ltdl_install=no],
  [if test x"$enable_ltdl_install" = xno; then
     AC_MSG_WARN([libltdl not installed, but installation disabled])
   else
     enable_ltdl_install=yes
   fi
  ])
  if test x"$enable_ltdl_install" = x"yes"; then
    ac_configure_args="$ac_configure_args --enable-ltdl-install"
    LIBLTDL=ifelse($#,1,$1,['${top_builddir}/libltdl'])/libltdl.la
    INCLTDL=ifelse($#,1,-I$1,['-I${top_srcdir}/libltdl'])
  else
    ac_configure_args="$ac_configure_args --enable-ltdl-install=no"
    LIBLTDL="-lltdl"
    INCLTDL=
  fi
])

dnl old names
AC_DEFUN(AM_PROG_LIBTOOL, [indir([AC_PROG_LIBTOOL])])dnl
AC_DEFUN(AM_ENABLE_SHARED, [indir([AC_ENABLE_SHARED], $@)])dnl
AC_DEFUN(AM_ENABLE_STATIC, [indir([AC_ENABLE_STATIC], $@)])dnl
AC_DEFUN(AM_DISABLE_SHARED, [indir([AC_DISABLE_SHARED], $@)])dnl
AC_DEFUN(AM_DISABLE_STATIC, [indir([AC_DISABLE_STATIC], $@)])dnl
AC_DEFUN(AM_PROG_LD, [indir([AC_PROG_LD])])dnl
AC_DEFUN(AM_PROG_NM, [indir([AC_PROG_NM])])dnl

dnl This is just to silence aclocal about the macro not being used
ifelse([AC_DISABLE_FAST_INSTALL])dnl

