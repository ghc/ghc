#
# extra autoconf macros for Glasgow fptools distribution
#

#
# Has timezone the type time_t or long (HP-UX 10.20 apparently
# has `long'..)
#
AC_DEFUN(AC_TYPE_TIMEZONE,
[AC_CACHE_CHECK([type of timezone], ac_cv_type_timezone,
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
[int i;], ac_cv_type_timezone=time_t, ac_cv_type_timezone=long)])
AC_DEFINE_UNQUOTED(TYPE_TIMEZONE, $ac_cv_type_timezone)
])

#
# Is altzone available?
#
AC_DEFUN(AC_ALTZONE,
[AC_CACHE_CHECK([altzone], ac_cv_altzone,
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
ac_cv_altzone=yes, ac_cv_altzone=no)])
if test "$ac_cv_altzone" = yes; then
  AC_DEFINE(HAVE_ALTZONE)
fi
])

#
dnl ** check for leading underscores in symbol names
#
# Test for determining whether symbol names have a leading
# underscore.
#
# We assume that they _haven't_ if anything goes wrong.
#
AC_DEFUN(AC_UNDERSCORE,
[AC_CHECK_LIB(elf, elf_begin, LIBS="-lelf $LIBS")dnl
AC_CACHE_CHECK([leading underscore in symbol names], ac_cv_lead_uscore,
AC_TRY_RUN([#ifdef HAVE_NLIST_H
#include <nlist.h>
changequote(<<, >>)dnl
<<
struct nlist xYzzY[] = {{"_xYzzY", 0},{0}};
#endif

main(argc, argv)
int argc;
char **argv;
{
#ifdef HAVE_NLIST_H
    if(nlist(argv[0], xYzzY) == 0 && xYzzY[0].n_value != 0)
        exit(0);>>
changequote([, ])dnl
#endif
    exit(1);
}], ac_cv_lead_uscore=yes, ac_cv_lead_uscore=no, ac_cv_lead_uscore=NO)),
#
# Hack!: nlist() under Digital UNIX insist on there being an _,
# but symbol table listings show none. What is going on here?!?
#
if test $HostPlatform = "alpha-dec-osf1"; then
   LeadingUnderscore=NO
else
   LeadingUnderscore=`echo $ac_cv_lead_uscore | sed 'y/yesno/YESNO/'`
fi;
test -n "$verbose" && echo "    setting LeadingUnderscore to $LeadingUnderscore"
AC_SUBST(LeadingUnderscore)
])


