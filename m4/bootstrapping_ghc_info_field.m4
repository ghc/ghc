# BOOTSTRAPPING_GHC_INFO_FIELD
# --------------------------------
# Set the variable $1 to the value of the ghc --info field $2.
AC_DEFUN([BOOTSTRAPPING_GHC_INFO_FIELD],[
$1=`"$WithGhc" --info | grep "^ ,(\"$2\"," | sed -e 's/.*","//' -e 's/")$//'`
tmp=${$1#\$topdir/}
if test "${$1}" != "$tmp"
then
    topdir=`"$WithGhc" --print-libdir | sed 's#\\\\#/#g'`
    $1="$topdir/$tmp"
fi
AC_SUBST($1)
])
