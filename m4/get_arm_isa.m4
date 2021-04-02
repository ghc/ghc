# GET_ARM_ISA
# ----------------------------------
# Get info about the ISA on the ARM arch
AC_DEFUN([GET_ARM_ISA],
[
    AC_COMPILE_IFELSE([
        AC_LANG_PROGRAM(
            [],
            [#if defined(__ARM_ARCH_2__)  || \
                 defined(__ARM_ARCH_3__)  || \
                 defined(__ARM_ARCH_3M__) || \
                 defined(__ARM_ARCH_4__)  || \
                 defined(__ARM_ARCH_4T__) || \
                 defined(__ARM_ARCH_5__)  || \
                 defined(__ARM_ARCH_5T__) || \
                 defined(__ARM_ARCH_5E__) || \
                 defined(__ARM_ARCH_5TE__)
                 return 0;
             #else
                 not pre arm v6
             #endif]
        )],
        [AC_DEFINE(arm_HOST_ARCH_PRE_ARMv6, 1, [ARM pre v6])
         AC_DEFINE(arm_HOST_ARCH_PRE_ARMv7, 1, [ARM pre v7])
         changequote(, )dnl
         ARM_ISA=ARMv5
         ARM_ISA_EXT="[]"
         changequote([, ])dnl
        ],
        [
            AC_COMPILE_IFELSE([
                AC_LANG_PROGRAM(
                    [],
                    [#if defined(__ARM_ARCH_6__)   || \
                         defined(__ARM_ARCH_6J__)  || \
                         defined(__ARM_ARCH_6T2__) || \
                         defined(__ARM_ARCH_6Z__)  || \
                         defined(__ARM_ARCH_6ZK__) || \
                         defined(__ARM_ARCH_6K__)  || \
                         defined(__ARM_ARCH_6KZ__) || \
                         defined(__ARM_ARCH_6M__)
                         return 0;
                     #else
                         not pre arm v7
                     #endif]
                )],
                [AC_DEFINE(arm_HOST_ARCH_PRE_ARMv7, 1, [ARM pre v7])
                 if grep -q Raspbian /etc/issue && uname -m | grep -q armv7; then
                   # Raspbian unfortunately makes some extremely questionable
                   # packaging decisions, configuring gcc to compile for ARMv6
                   # despite the fact that the RPi4 is ARMv8. As ARMv8 doesn't
                   # support all instructions supported by ARMv6 this can
                   # break. Work around this by checking uname to verify
                   # that we aren't running on armv7.
                   # See #17856.
                   AC_MSG_NOTICE([Found compiler which claims to target ARMv6 running on ARMv7, assuming this is ARMv7 on Raspbian (see T17856)])
                   ARM_ISA=ARMv7
                   changequote(, )dnl
                   ARM_ISA_EXT="[VFPv2]"
                   changequote([, ])dnl
                 else
                   ARM_ISA=ARMv6
                   AC_COMPILE_IFELSE([
                          AC_LANG_PROGRAM(
                                  [],
                                  [#if defined(__VFP_FP__)
                                       return 0;
                                  #else
                                       no vfp
                                  #endif]
                          )],
                          [changequote(, )dnl
                           ARM_ISA_EXT="[VFPv2]"
                           changequote([, ])dnl
                          ],
                          [changequote(, )dnl
                           ARM_ISA_EXT="[]"
                           changequote([, ])dnl
                          ]
                  )
                fi],
                [changequote(, )dnl
                 ARM_ISA=ARMv7
                 ARM_ISA_EXT="[VFPv3,NEON]"
                 changequote([, ])dnl
                ])
        ])

        AC_COMPILE_IFELSE(
               [AC_LANG_PROGRAM(
                       [],
                       [#if defined(__SOFTFP__)
                            return 0;
                       #else
                            not softfp
                       #endif]
               )],
               [changequote(, )dnl
                ARM_ABI="SOFT"
                changequote([, ])dnl
               ],
               [AC_COMPILE_IFELSE(
                    [AC_LANG_PROGRAM(
                       [],
                       [#if defined(__ARM_PCS_VFP)
                            return 0;
                       #else
                            no hard float ABI
                       #endif]
                    )],
                    [ARM_ABI="HARD"],
                    [ARM_ABI="SOFTFP"]
               )]
        )

        AC_SUBST(ARM_ISA)
])
