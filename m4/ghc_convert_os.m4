# GHC_CONVERT_OS(os, converted_cpu, target_var)
# --------------------------------
# converts os from gnu to ghc naming, and assigns the result to $target_var
AC_DEFUN([GHC_CONVERT_OS],[
    case "$1" in
      gnu*) # e.g. i686-unknown-gnu0.9
        $3="gnu"
        ;;
      # watchos and tvos are ios variants as of May 2017.
      ios|watchos|tvos)
        $3="ios"
        ;;
      linux-android*)
        $3="linux-android"
        ;;
      linux-*|linux)
        $3="linux"
        ;;
      netbsd*)
        $3="netbsd"
        ;;
      openbsd*)
        $3="openbsd"
        ;;
      # As far as I'm aware, none of these have relevant variants
      freebsd|dragonfly|hpux|linuxaout|kfreebsdgnu|freebsd2|mingw32|darwin|nextstep2|nextstep3|sunos4|ultrix|haiku)
        $3="$1"
        ;;
      msys)
        AC_MSG_ERROR([Building GHC using the msys toolchain is not supported; please use mingw instead. Perhaps you need to set 'MSYSTEM=MINGW64 or MINGW32?'])
        ;;
      aix*) # e.g. powerpc-ibm-aix7.1.3.0
        $3="aix"
        ;;
      darwin*) # e.g. aarch64-apple-darwin14
        $3="darwin"
        ;;
      solaris2*)
        $3="solaris2"
        ;;
      freebsd*) # like i686-gentoo-freebsd7
                #      i686-gentoo-freebsd8
                #      i686-gentoo-freebsd8.2
        $3="freebsd"
        ;;
      nto-qnx*)
        $3="nto-qnx"
        ;;
      ghcjs*)
        $3="ghcjs"
        ;;
      *)
        echo "Unknown OS $1"
        exit 1
        ;;
      esac
])
