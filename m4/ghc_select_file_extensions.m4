AC_DEFUN([GHC_SELECT_FILE_EXTENSIONS],
[
    $2=''
    $3='.so'
    case $1 in
    *-unknown-cygwin32)
        AC_MSG_WARN([GHC does not support the Cygwin target at the moment])
        AC_MSG_WARN([I'm assuming you wanted to build for x86_64-w64-mingw32])
        exit 1
        ;;
    # examples: x86_64-w64-mingw32
    *-mingw32)
        windows=YES
        $2='.exe'
        $3='.dll'
        ;;
    # apple platform uses .dylib (macOS, iOS, ...)
    *-apple-*)
        $3='.dylib'
        ;;
    esac
])
