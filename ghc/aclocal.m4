# FP_SETUP_PROJECT_INFO
# ---------------------
AC_DEFUN([FP_SETUP_PROJECT_INFO],
[# Some renamings
AC_SUBST([ProjectName], [$PACKAGE_NAME])
AC_SUBST([ProjectNameShort], [$PACKAGE_TARNAME])
AC_SUBST([ProjectVersion], [$PACKAGE_VERSION])

# Split PACKAGE_VERSION into (possibly empty) parts
VERSION_MAJOR=`echo $PACKAGE_VERSION | sed 's/^\(@<:@^.@:>@*\)\(\.\?\(.*\)\)$/\1'/`
VERSION_TMP=`echo $PACKAGE_VERSION | sed 's/^\(@<:@^.@:>@*\)\(\.\?\(.*\)\)$/\3'/`
VERSION_MINOR=`echo $VERSION_TMP | sed 's/^\(@<:@^.@:>@*\)\(\.\?\(.*\)\)$/\1'/`
ProjectPatchLevel=`echo $VERSION_TMP | sed 's/^\(@<:@^.@:>@*\)\(\.\?\(.*\)\)$/\3'/`

# Calculate project version as an integer, using 2 digits for minor version
case $VERSION_MINOR in
  ?) ProjectVersionInt=${VERSION_MAJOR}0${VERSION_MINOR} ;;
  ??) ProjectVersionInt=${VERSION_MAJOR}${VERSION_MINOR} ;;
  *) AC_MSG_ERROR([bad minor version in $PACKAGE_VERSION]) ;;
esac
AC_SUBST([ProjectVersionInt])

# The project patchlevel is zero unless stated otherwise
test -z "$ProjectPatchLevel" && ProjectPatchLevel=0
AC_SUBST([ProjectPatchLevel])
])# FP_SETUP_PROJECT_INFO
