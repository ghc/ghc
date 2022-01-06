# GHC_GET_POWER_ABI
# ----------------------------------
# Get version of the PowerPC ABI
AC_DEFUN([GHC_GET_POWER_ABI],
[
    AC_COMPILE_IFELSE([
        AC_LANG_PROGRAM(
            [],
            [#if defined(_CALL_ELF) && _CALL_ELF == 2
                 return 0;
             #else
                 not ELF v2
             #endif]
        )],
        [POWER_ABI=ELF_V2],
        [POWER_ABI=ELF_V1])

        AC_SUBST(POWER_ABI)
])
