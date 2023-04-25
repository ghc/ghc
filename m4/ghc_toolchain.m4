dnl $1  argument name
dnl $2  variable
AC_DEFUN([ADD_GHC_TOOLCHAIN_ARG],
[
    set -- $2
    for x do
        echo "--$1=$x" >> acargs
    done
])

AC_DEFUN([FIND_GHC_TOOLCHAIN],
[
    "$GHC" -v0 \
        -ilibraries/ghc-boot -iutils/ghc-toolchain/src \
        -XNoImplicitPrelude \
        -odir actmp-ghc-toolchain -hidir actmp-ghc-toolchain \
        Main -o acghc-toolchain

    rm -f acargs
    echo "--triple=$target" >> acargs
    echo "--cc=$CC" >> acargs
    ADD_GHC_TOOLCHAIN_ARG([cc-opt], [$CONF_CC_OPTS_STAGE1])
    # TODO
    #echo "--cpp=$CPP" >> acargs
    ADD_GHC_TOOLCHAIN_ARG([cpp-opt], [$CONF_CPP_OPTS_STAGE1])
    echo "--cc-link=$CC" >> acargs
    ADD_GHC_TOOLCHAIN_ARG([cc-link-opt], [$CONF_GCC_LINK_OPTS_STAGE1])
    echo "--cxx=$CXX" >> acargs
    ADD_GHC_TOOLCHAIN_ARG([cxx-opt], [$CONF_CXX_OPTS_STAGE1])
    echo "--ar=$AR" >> acargs
    ADD_GHC_TOOLCHAIN_ARG([ar-opt], [$ARFLAGS])
    echo "--ranlib=$RANLIB" >> acargs
    ADD_GHC_TOOLCHAIN_ARG([ranlib-opt], [$RANLIBFLAGS])
    echo "--nm=$NM" >> acargs
    ADD_GHC_TOOLCHAIN_ARG([nm-opt], [$NMFLAGS])
    echo "--readelf=$READELF" >> acargs
    ADD_GHC_TOOLCHAIN_ARG([readelf-opt], [$READELFFLAGS])

    (
        set --
        while read -r arg; do
            set -- "[$]@" "$arg"
        done
        ./acghc-toolchain "[$]@" || exit 1
        python -c 'import sys; print(sys.argv)' "[$]@"
    ) <acargs || exit 1

    #rm -Rf acargs acghc-toolchain actmp-ghc-toolchain
])

