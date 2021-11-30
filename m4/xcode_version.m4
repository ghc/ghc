# XCODE_VERSION()
# --------------------------------
# Gets the version number of Xcode, if on a Mac
AC_DEFUN([XCODE_VERSION],[
    if test "$TargetVendor_CPP" = "apple"
    then
        AC_MSG_CHECKING(Xcode version)
        XcodeVersion=`(xcode-select -p > /dev/null 2>&1 && xcodebuild -version) | grep Xcode | sed "s/Xcode //"`
        # Old Xcode versions don't actually give the Xcode version
        if test "$XcodeVersion" = ""
        then
            AC_MSG_RESULT(not found (too old?))
            XcodeVersion1=0
            XcodeVersion2=0
        else
            AC_MSG_RESULT($XcodeVersion)
            XcodeVersion1=`echo "$XcodeVersion" | sed 's/\..*//'`
            changequote(, )dnl
            XcodeVersion2=`echo "$XcodeVersion" | sed 's/[^.]*\.\([^.]*\).*/\1/'`
            changequote([, ])dnl
            AC_MSG_NOTICE(Xcode version component 1: $XcodeVersion1)
            AC_MSG_NOTICE(Xcode version component 2: $XcodeVersion2)
        fi
    fi
])
