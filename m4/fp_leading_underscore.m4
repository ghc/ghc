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
#
AC_DEFUN([FP_LEADING_UNDERSCORE],
[AC_CHECK_LIB([elf], [nlist], [LIBS="-lelf $LIBS"])
AC_CACHE_CHECK([leading underscore in symbol names], [fptools_cv_leading_underscore], [
# Hack!: nlist() under Digital UNIX insist on there being an _,
# but symbol table listings shows none. What is going on here?!?
case $TargetPlatform in
    # Apples mach-o platforms use leading underscores
    *-apple-*) fptools_cv_leading_underscore=yes;;
    *linux-android*) fptools_cv_leading_underscore=no;;
    *openbsd*) # x86 openbsd is ELF from 3.4 >, meaning no leading uscore
      case $build in
        i386-*2\.@<:@0-9@:>@ | i386-*3\.@<:@0-3@:>@ ) fptools_cv_leading_underscore=yes ;;
        *) fptools_cv_leading_underscore=no ;;
      esac ;;
    x86_64-unknown-mingw32) fptools_cv_leading_underscore=no;;
    *) AC_RUN_IFELSE([AC_LANG_SOURCE([[#if defined(HAVE_NLIST_H)
#include <nlist.h>
struct nlist xYzzY1[] = {{"xYzzY1", 0},{0}};
struct nlist xYzzY2[] = {{"_xYzzY2", 0},{0}};
#endif

int main(int argc, char **argv)
{
#if defined(HAVE_NLIST_H)
    if(nlist(argv[0], xYzzY1) == 0 && xYzzY1[0].n_value != 0)
        return 1;
    if(nlist(argv[0], xYzzY2) == 0 && xYzzY2[0].n_value != 0)
        return 0;
#endif
    return 1;
}]])],[fptools_cv_leading_underscore=yes],[fptools_cv_leading_underscore=no],[fptools_cv_leading_underscore=no])
;;
esac]);
])# FP_LEADING_UNDERSCORE
