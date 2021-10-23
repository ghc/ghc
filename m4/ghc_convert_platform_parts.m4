# GHC_CONVERT_PLATFORM_PARTS(input_platform, OutputPlatform)
# --------------------------------
# Call all 3 of the underlying `GHC_CONVERT_*` functions to convert the
# parsed platform from GNU to GHC naming.
AC_DEFUN([GHC_CONVERT_PLATFORM_PARTS],[
    GHC_CONVERT_CPU([$]$1[_cpu], $2[Arch])
    GHC_CONVERT_VENDOR([$]$1[_vendor], $2[Vendor])
    GHC_CONVERT_OS([$]$1[_os], [$]$2[Arch], $2[OS])
])
