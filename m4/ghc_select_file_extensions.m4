AC_DEFUN([GHC_SELECT_FILE_EXTENSIONS],
[
    $2=''
    $3='.so'
    case $1 in
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
