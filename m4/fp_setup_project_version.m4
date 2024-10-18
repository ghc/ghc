# FP_SETUP_PROJECT_VERSION
# ---------------------
AC_DEFUN([FP_SETUP_PROJECT_VERSION],
[
    # number of version number components
    NumVersionComponents="$(( $(echo "$PACKAGE_VERSION" | tr -cd . | wc -c) + 1 ))"

    if test "$RELEASE" = "NO"; then
        AC_MSG_CHECKING([for GHC version date])
        if test -f VERSION_DATE; then
            PACKAGE_VERSION=${PACKAGE_VERSION}.`cat VERSION_DATE`
            AC_MSG_RESULT(given $PACKAGE_VERSION)
        elif test -e .git; then
            changequote(, )dnl
            ver_posixtime=`git log -1 --pretty=format:%ct`
            ver_date=`perl -MPOSIX -e "print strftime('%Y%m%d', gmtime($ver_posixtime));"`
            if echo $ver_date | grep '^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$' 2>&1 >/dev/null; then true; else
            changequote([, ])dnl
                    AC_MSG_ERROR([failed to detect version date: check that git and perl are in your path])
            fi
            PACKAGE_VERSION=${PACKAGE_VERSION}.$ver_date
            AC_MSG_RESULT(inferred $PACKAGE_VERSION)
        elif test -f VERSION; then
            PACKAGE_VERSION=`cat VERSION`
            AC_MSG_RESULT(given $PACKAGE_VERSION)
        else
            AC_MSG_WARN([cannot determine snapshot version: no .git directory and no VERSION file])
            dnl We'd really rather this case didn't happen, but it might
            dnl do (in particular, people using lndir trees may find that
            dnl the build system can't find any other date). If it does
            dnl happen, then we use the current date.
            dnl This way we get some idea about how recent a build is.
            dnl It also means that packages built for 2 different builds
            dnl will probably use different version numbers, so things are
            dnl less likely to go wrong.
            PACKAGE_VERSION=${PACKAGE_VERSION}.`date +%Y%m%d`
        fi
    fi

    AC_MSG_CHECKING([for GHC Git commit id])
    if test -e .git; then
        git_commit_id=`git rev-parse HEAD`
        if test -n "$git_commit_id" 2>&1 >/dev/null; then true; else
            AC_MSG_ERROR([failed to detect revision: check that git is in your path])
        fi
        PACKAGE_GIT_COMMIT_ID=$git_commit_id
        AC_MSG_RESULT(inferred $PACKAGE_GIT_COMMIT_ID)
    elif test -f GIT_COMMIT_ID; then
        PACKAGE_GIT_COMMIT_ID=`cat GIT_COMMIT_ID`
        AC_MSG_RESULT(given $PACKAGE_GIT_COMMIT_ID)
    else
        AC_MSG_WARN([cannot determine snapshot revision: no .git directory and no 'GIT_COMMIT_ID' file])
        PACKAGE_GIT_COMMIT_ID="0000000000000000000000000000000000000000"
    fi


    # Some renamings
    AC_SUBST([ProjectName], [$PACKAGE_NAME])
    AC_SUBST([ProjectVersion], [$PACKAGE_VERSION])
    AC_SUBST([ProjectGitCommitId], [$PACKAGE_GIT_COMMIT_ID])

    # Split PACKAGE_VERSION into (possibly empty) parts
    VERSION_MAJOR=`echo $PACKAGE_VERSION | sed 's/^\(@<:@^.@:>@*\)\(\.\{0,1\}\(.*\)\)$/\1'/`
    VERSION_TMP=`echo $PACKAGE_VERSION | sed 's/^\(@<:@^.@:>@*\)\(\.\{0,1\}\(.*\)\)$/\3'/`
    VERSION_MINOR=`echo $VERSION_TMP | sed 's/^\(@<:@^.@:>@*\)\(\.\{0,1\}\(.*\)\)$/\1'/`
    ProjectPatchLevel=`echo $VERSION_TMP | sed 's/^\(@<:@^.@:>@*\)\(\.\{0,1\}\(.*\)\)$/\3'/`

    # Verify that the version number has three components if a release version
    # (that is, even minor version number).
    AC_MSG_CHECKING([package version validity])
    StableRelease="$(( ($VERSION_MINOR & 1) == 0))"
    if test "$StableRelease" = "1" -a "$NumVersionComponents" != "3"; then
        AC_MSG_ERROR([Stable (even) version numbers must have three components])
    elif test "$StableRelease" = "0" -a "$NumVersionComponents" != "2"; then
        AC_MSG_ERROR([Unstable (odd) version numbers must have two components])
    elif test "$RELEASE" = "YES" -a "$StableRelease" = "0"; then
        AC_MSG_ERROR([RELEASE=YES despite having an unstable odd minor version number])
    elif test "$StableRelease" = "1"; then
        AC_MSG_RESULT([okay stable branch version])
    else
        AC_MSG_RESULT([okay unstable branch version])
    fi

    # Calculate project version as an integer, using 2 digits for minor version
    case $VERSION_MINOR in
      ?) ProjectVersionInt=${VERSION_MAJOR}0${VERSION_MINOR} ;;
      ??) ProjectVersionInt=${VERSION_MAJOR}${VERSION_MINOR} ;;
      *) AC_MSG_ERROR([bad minor version in $PACKAGE_VERSION]) ;;
    esac
    AC_SUBST([ProjectVersionInt])

    # The project patchlevel is zero unless stated otherwise
    test -z "$ProjectPatchLevel" && ProjectPatchLevel=0

    # Save split version of ProjectPatchLevel
    ProjectPatchLevel1=`echo $ProjectPatchLevel | sed 's/^\(@<:@^.@:>@*\)\(\.\{0,1\}\(.*\)\)$/\1/'`
    ProjectPatchLevel2=`echo $ProjectPatchLevel | sed 's/^\(@<:@^.@:>@*\)\(\.\{0,1\}\(.*\)\)$/\3/'`

    # The project patchlevel1/2 is zero unless stated otherwise
    test -z "$ProjectPatchLevel1" && ProjectPatchLevel1=0
    test -z "$ProjectPatchLevel2" && ProjectPatchLevel2=0

    AC_SUBST([ProjectPatchLevel1])
    AC_SUBST([ProjectPatchLevel2])

    # Remove dots from the patch level; this allows us to have versions like 6.4.1.20050508
    ProjectPatchLevel=`echo $ProjectPatchLevel | sed 's/\.//'`

    AC_SUBST([ProjectPatchLevel])

    # The version of the GHC package changes every day, since the
    # patchlevel is the current date.  We don't want to force
    # recompilation of the entire compiler when this happens, so for
    # GHC HEAD we omit the patchlevel from the package version number.
    #
    # The ProjectPatchLevel1 > 20000000 iff GHC HEAD. If it's for a stable
    # release like 7.10.1 or for a release candidate such as 7.10.1.20141224
    # then we don't omit the patchlevel components.

    ProjectVersionMunged="$ProjectVersion"
    if test "$ProjectPatchLevel1" -gt 20000000; then
      ProjectVersionMunged="${VERSION_MAJOR}.${VERSION_MINOR}"
    fi
    AC_SUBST([ProjectVersionMunged])

    # The version used for libraries tightly coupled with GHC (e.g.
    # ghc-internal) which need a major version bump for every minor/patchlevel
    # GHC version.
    # Example: for GHC=9.10.1, ProjectVersionForLib=9.1001
    #
    # Just like with project version munged, we don't want to use the
    # patchlevel version which changes every day, so if using GHC HEAD, the
    # patchlevel = 00.
    case $VERSION_MINOR in
      ?) ProjectVersionForLibUpperHalf=${VERSION_MAJOR}.0${VERSION_MINOR} ;;
      ??) ProjectVersionForLibUpperHalf=${VERSION_MAJOR}.${VERSION_MINOR} ;;
      *) AC_MSG_ERROR([bad minor version in $PACKAGE_VERSION]) ;;
    esac
    # GHC HEAD uses patch level version > 20000000
    case $ProjectPatchLevel1 in
      ?) ProjectVersionForLib=${ProjectVersionForLibUpperHalf}0${ProjectPatchLevel1} ;;
      ??) ProjectVersionInt=${ProjectVersionForLibUpperHalf}${ProjectPatchLevel1} ;;
      *) ProjectVersionForLib=${ProjectVersionForLibUpperHalf}00
    esac
    AC_SUBST([ProjectVersionForLib])
])# FP_SETUP_PROJECT_VERSION
