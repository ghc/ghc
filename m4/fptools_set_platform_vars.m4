# FPTOOLS_SET_PLATFORMS_VARS
# ----------------------------------
# Set all the platform variables. First massage the default autoconf
# choices for build, host, and target, then parse it into
# <platform>Arch, <platform>Vendor, and <platform>OS, and finally define
# the other variables in terms of those.
AC_DEFUN([FPTOOLS_SET_PLATFORMS_VARS],
[
    # If no argument was given for a configuration variable, then discard
    # the guessed canonical system and use the configuration of the
    # bootstrapping ghc. If an argument was given, map it from gnu format
    # to ghc format.
    #
    # For why we do it this way, see: #3637, #1717, #2951
    #
    # In bindists, we haven't called AC_CANONICAL_{BUILD,HOST,TARGET}
    # so this justs uses $bootstrap_target.

    if test "$build_alias" = ""
    then
        FPTOOLS_OVERRIDE_PLATFORM_FROM_BOOTSTRAP([build], [Build])
    else
        GHC_CONVERT_PLATFORM_PARTS([build], [Build])
    fi

    if test "$host_alias" = ""
    then
        FPTOOLS_OVERRIDE_PLATFORM_FROM_BOOTSTRAP([host], [Host])
    else
        GHC_CONVERT_PLATFORM_PARTS([host], [Host])
    fi

    if test "$target_alias" = ""
    then
        if test "$host_alias" != ""
        then
            GHC_CONVERT_PLATFORM_PARTS([host], [Target])
        else
            FPTOOLS_OVERRIDE_PLATFORM_FROM_BOOTSTRAP([target], [Target])
        fi
    else
        GHC_CONVERT_PLATFORM_PARTS([target], [Target])
    fi

    FPTOOLS_SET_PLATFORM_VARS([build], [Build])
    FPTOOLS_SET_PLATFORM_VARS([host], [Host])
    FPTOOLS_SET_PLATFORM_VARS([target], [Target])

    windows=NO
    case $host in
    *-unknown-mingw32)
        windows=YES
        ;;
    esac
])

dnl Attempt at arch agnostic distillation of the above, but it
dnl doesn't quite work yet. Perhaps after the configure script is
dnl more split up (#17191) this wil become more feasible.

dnl if test "[$]$1_alias" = ""
dnl then
dnl     if test "[$]$3_alias" != ""
dnl     then
dnl         GHC_CONVERT_PLATFORM_PARTS($3, $2)
dnl     else
dnl         FPTOOLS_SET_PLATFORMS_VARS($1, $2)
dnl     fi
dnl else
dnl     GHC_CONVERT_PLATFORM_PARTS($1, $2)
dnl fi

# FPTOOLS_OVERRIDE_PLATFORM_FROM_BOOTSTRAP(platform,Platform)
# ----------------------------------
# Per the comment in FPTOOLS_OVERRIDE_PLATFORM_FROM_BOOTSTRAP's body, we
# need to sometimes replace inferred platforms with the bootstrap
# compiler's target platform.
AC_DEFUN([FPTOOLS_OVERRIDE_PLATFORM_FROM_BOOTSTRAP],
[
    if test "$bootstrap_$1" != ""
    then
        $1=$bootstrap_$1
        echo "$1 platform inferred as: [$]$1"
    else
        echo "Can't work out $1 platform"
        exit 1
    fi

    $2[Arch]=`echo "[$]$1" | sed 's/-.*//'`
    $2[Vendor]=`echo "[$]$1" | sed -e 's/.*-\(.*\)-.*/\1/'`
    $2[OS]=`echo "[$]$1" | sed 's/.*-//'`
])

# FPTOOLS_SET_PLATFORM_VARS(platform,Platform)
# ----------------------------------
# Set the platform variables for a single plaform (one of build, host,
# or target). Assumes <platform>Arch, <platform>Vendor, and <platform>OS
# are defined, and does everything else in terms of them.
AC_DEFUN([FPTOOLS_SET_PLATFORM_VARS],
[

    $2Platform="[$]$2Arch-[$]$2Vendor-[$]$2OS"
    $2Platform_CPP=`echo "[$]$2Platform" | sed -e 's/\./_/g' -e 's/-/_/g'`
    $2Arch_CPP=`    echo "[$]$2Arch"     | sed -e 's/\./_/g' -e 's/-/_/g'`
    $2Vendor_CPP=`  echo "[$]$2Vendor"   | sed -e 's/\./_/g' -e 's/-/_/g'`
    $2OS_CPP=`      echo "[$]$2OS"       | sed -e 's/\./_/g' -e 's/-/_/g'`

    AC_MSG_NOTICE([GHC ]$1[ : $]$2[Platform])

    AC_SUBST($2Platform)
    AC_SUBST($2Platform_CPP)

    AC_SUBST($2Arch_CPP)
    AC_SUBST($2OS_CPP)
    AC_SUBST($2Vendor_CPP)

    GHC_SELECT_FILE_EXTENSIONS([$]$1, [exeext_]$1, [soext_]$1)

    AC_SUBST(exeext_$1)
    AC_SUBST(soext_$1)
])
