# FP_FIND_CXX_STD_LIB
# -------------------
#
# Identify which C++ standard library implementation the C++ toolchain links
# against.
AC_DEFUN([FP_FIND_CXX_STD_LIB],[
    # If this is non-empty then assume that the user has specified these
    # manually.
    if test -z "$CXX_STD_LIB_LIBS"; then
        cat >actest.cpp <<-EOF
#include <iostream>
#if defined(_LIBCPP_VERSION)
libc++
#elif defined(__GLIBCXX__)
libstdc++
#else
unknown
#endif
EOF
        AC_MSG_CHECKING([C++ standard library flavour])
        if "$CXX" -E actest.cpp -o actest.out; then
            if grep "libc++" actest.out; then
                CXX_STD_LIB_LIBS="c++ c++abi"
                p="`"$CXX" --print-file-name libc++.so`"
                d="`dirname "$p"`"
                dnl On some platforms (e.g. Windows) the C++ standard library
                dnl can be found in the system search path. In this case $CXX
                dnl --print-file-name will simply print the filename without a
                dnl directory part. Consequently, dirname will return `.`. However,
                dnl we don't want to include such paths in the package database.
                if test "$d" = "."; then d=""; fi
                CXX_STD_LIB_LIB_DIRS="$d"
                CXX_STD_LIB_DYN_LIB_DIRS="$d"
                AC_MSG_RESULT([libc++])
            elif grep "libstdc++" actest.out; then
                CXX_STD_LIB_LIBS="stdc++"
                p="`"$CXX" --print-file-name libstdc++.so`"
                d="`dirname "$p"`"
                if test "$d" = "."; then d=""; fi
                CXX_STD_LIB_LIB_DIRS="$d"
                CXX_STD_LIB_DYN_LIB_DIRS="$d"
                AC_MSG_RESULT([libstdc++])
            else
                rm -f actest.cpp actest.out
                AC_MSG_ERROR([Unknown C++ standard library implementation.])
            fi
            rm -f actest.cpp actest.out
        else
            rm -f actest.cpp actest.out
            AC_MSG_ERROR([Failed to compile test program])
        fi
    fi

    AC_SUBST([CXX_STD_LIB_LIBS])
    AC_SUBST([CXX_STD_LIB_LIB_DIRS])
    AC_SUBST([CXX_STD_LIB_DYN_LIB_DIRS])
])

