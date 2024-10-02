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
        if ! "$CXX" -E actest.cpp -o actest.out; then
            rm -f actest.cpp actest.out
            AC_MSG_ERROR([Failed to compile test program])
        fi

        dnl Identify standard library type
        if grep "libc++" actest.out >/dev/null; then
            CXX_STD_LIB_FLAVOUR="c++"
            AC_MSG_RESULT([libc++])
        elif grep "libstdc++" actest.out >/dev/null; then
            CXX_STD_LIB_FLAVOUR="stdc++"
            AC_MSG_RESULT([libstdc++])
        else
            rm -f actest.cpp actest.out
            AC_MSG_ERROR([Unknown C++ standard library implementation.])
        fi
        rm -f actest.cpp actest.out

        dnl -----------------------------------------
        dnl Figure out how to link...
        dnl -----------------------------------------
        cat >actest.cpp <<-EOF
#include <iostream>
int main(int argc, char** argv) {
    std::cout << "hello world\n";
    return 0;
}
EOF
        if ! "$CXX" -c actest.cpp; then
            AC_MSG_ERROR([Failed to compile test object])
        fi

        try_libs() {
            dnl Try to link a plain object with CC manually
            AC_MSG_CHECKING([for linkage against '${3}'])
            dnl Ensures that CC uses same library path of CXX.
            p="`"$CXX" --print-file-name ${2}`"
            d="`dirname "$p"`"
            if "$CC" -o actest actest.o ${1} -L"$d" 2>/dev/null; then
                CXX_STD_LIB_LIBS="${3}"
                dnl On some platforms (e.g. Windows) the C++ standard library
                dnl can be found in the system search path. In this case $CXX
                dnl --print-file-name will simply print the filename without a
                dnl directory part. Consequently, dirname will return `.`. However,
                dnl we don't want to include such paths in the package database.
                if test "$d" = "."; then d=""; fi
                CXX_STD_LIB_LIB_DIRS="$d"
                CXX_STD_LIB_DYN_LIB_DIRS="$d"
                AC_MSG_RESULT([success])
                true
            else
                AC_MSG_RESULT([failed])
                false
            fi
        }
        case $CXX_STD_LIB_FLAVOUR in
        c++)
            try_libs "-lc++ -lc++abi" "libc++.so" "c++ c++abi" || \
            try_libs "-lc++ -lc++abi -lpthread" "libc++.so" "c++ c++abi pthread" || \
            try_libs "-lc++ -lcxxrt" "libc++.so" "c++ cxxrt" ||
            AC_MSG_ERROR([Failed to find C++ standard library]) ;;
        stdc++)
            try_libs "-lstdc++" "libstdc++.so" "stdc++" || \
            try_libs "-lstdc++ -lsupc++" "libstdc++.so" "stdc++ supc++" || \
            AC_MSG_ERROR([Failed to find C++ standard library]) ;;
        esac

        rm -f actest.cpp actest.o actest
    fi

    AC_SUBST([CXX_STD_LIB_LIBS])
    AC_SUBST([CXX_STD_LIB_LIB_DIRS])
    AC_SUBST([CXX_STD_LIB_DYN_LIB_DIRS])
])

