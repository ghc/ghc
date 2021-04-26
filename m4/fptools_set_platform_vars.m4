# FPTOOLS_SET_PLATFORM_VARS
# ----------------------------------
# Set the platform variables
AC_DEFUN([FPTOOLS_SET_PLATFORM_VARS],
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
        if test "$bootstrap_target" != ""
        then
            build=$bootstrap_target
            echo "Build platform inferred as: $build"
        else
            echo "Can't work out build platform"
            exit 1
        fi

        BuildArch=`echo "$build" | sed 's/-.*//'`
        BuildVendor=`echo "$build" | sed -e 's/.*-\(.*\)-.*/\1/'`
        BuildOS=`echo "$build" | sed 's/.*-//'`
    else
        GHC_CONVERT_CPU([$build_cpu], [BuildArch])
        GHC_CONVERT_VENDOR([$build_vendor], [BuildVendor])
        GHC_CONVERT_OS([$build_os], [$BuildArch], [BuildOS])
    fi

    if test "$host_alias" = ""
    then
        if test "$bootstrap_target" != ""
        then
            host=$bootstrap_target
            echo "Host platform inferred as: $host"
        else
            echo "Can't work out host platform"
            exit 1
        fi

        HostArch=`echo "$host" | sed 's/-.*//'`
        HostVendor=`echo "$host" | sed -e 's/.*-\(.*\)-.*/\1/'`
        HostOS=`echo "$host" | sed 's/.*-//'`
    else
        GHC_CONVERT_CPU([$host_cpu], [HostArch])
        GHC_CONVERT_VENDOR([$host_vendor], [HostVendor])
        GHC_CONVERT_OS([$host_os], [$HostArch], [HostOS])
    fi

    if test "$target_alias" = ""
    then
        if test "$host_alias" != ""
        then
            GHC_CONVERT_CPU([$host_cpu], [TargetArch])
            GHC_CONVERT_VENDOR([$host_vendor], [TargetVendor])
            GHC_CONVERT_OS([$host_os], [$TargetArch],[TargetOS])
        else
            if test "$bootstrap_target" != ""
            then
                target=$bootstrap_target
                echo "Target platform inferred as: $target"
            else
                echo "Can't work out target platform"
                exit 1
            fi

            TargetArch=`echo "$target" | sed 's/-.*//'`
            TargetVendor=`echo "$target" | sed -e 's/.*-\(.*\)-.*/\1/'`
            TargetOS=`echo "$target" | sed 's/.*-//'`
        fi
    else
        GHC_CONVERT_CPU([$target_cpu], [TargetArch])
        GHC_CONVERT_VENDOR([$target_vendor], [TargetVendor])
        GHC_CONVERT_OS([$target_os], [$TargetArch], [TargetOS])
    fi

    GHC_LLVM_TARGET([$target],[$target_cpu],[$target_vendor],[$target_os],[LlvmTarget])

    GHC_SELECT_FILE_EXTENSIONS([$host], [exeext_host], [soext_host])
    GHC_SELECT_FILE_EXTENSIONS([$target], [exeext_target], [soext_target])
    windows=NO
    case $host in
    *-unknown-mingw32)
        windows=YES
        ;;
    esac

    BuildPlatform="$BuildArch-$BuildVendor-$BuildOS"
    BuildPlatform_CPP=`echo "$BuildPlatform" | sed -e 's/\./_/g' -e 's/-/_/g'`
    BuildArch_CPP=`    echo "$BuildArch"     | sed -e 's/\./_/g' -e 's/-/_/g'`
    BuildVendor_CPP=`  echo "$BuildVendor"   | sed -e 's/\./_/g' -e 's/-/_/g'`
    BuildOS_CPP=`      echo "$BuildOS"       | sed -e 's/\./_/g' -e 's/-/_/g'`

    HostPlatform="$HostArch-$HostVendor-$HostOS"
    HostPlatform_CPP=`echo "$HostPlatform" | sed -e 's/\./_/g' -e 's/-/_/g'`
    HostArch_CPP=`    echo "$HostArch"     | sed -e 's/\./_/g' -e 's/-/_/g'`
    HostVendor_CPP=`  echo "$HostVendor"   | sed -e 's/\./_/g' -e 's/-/_/g'`
    HostOS_CPP=`      echo "$HostOS"       | sed -e 's/\./_/g' -e 's/-/_/g'`

    TargetPlatform="$TargetArch-$TargetVendor-$TargetOS"
    TargetPlatform_CPP=`echo "$TargetPlatform" | sed -e 's/\./_/g' -e 's/-/_/g'`
    TargetArch_CPP=`    echo "$TargetArch"     | sed -e 's/\./_/g' -e 's/-/_/g'`
    TargetVendor_CPP=`  echo "$TargetVendor"   | sed -e 's/\./_/g' -e 's/-/_/g'`
    TargetOS_CPP=`      echo "$TargetOS"       | sed -e 's/\./_/g' -e 's/-/_/g'`

    # we intend to pass trough --targets to llvm as is.
    LLVMTarget_CPP=`    echo "$LlvmTarget"`

    echo "GHC build  : $BuildPlatform"
    echo "GHC host   : $HostPlatform"
    echo "GHC target : $TargetPlatform"
    echo "LLVM target: $LlvmTarget"

    AC_SUBST(BuildPlatform)
    AC_SUBST(HostPlatform)
    AC_SUBST(TargetPlatform)
    AC_SUBST(HostPlatform_CPP)
    AC_SUBST(BuildPlatform_CPP)
    AC_SUBST(TargetPlatform_CPP)

    AC_SUBST(HostArch_CPP)
    AC_SUBST(BuildArch_CPP)
    AC_SUBST(TargetArch_CPP)

    AC_SUBST(HostOS_CPP)
    AC_SUBST(BuildOS_CPP)
    AC_SUBST(TargetOS_CPP)
    AC_SUBST(LLVMTarget_CPP)

    AC_SUBST(HostVendor_CPP)
    AC_SUBST(BuildVendor_CPP)
    AC_SUBST(TargetVendor_CPP)

    AC_SUBST(exeext_host)
    AC_SUBST(exeext_target)
    AC_SUBST(soext_host)
    AC_SUBST(soext_target)
])

