# FP_ARMV8_OUTLINE_ATOMICS
# ----------
#
# Note [ARM outline atomics and the RTS linker]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Sets HAVE_ARM_OUTLINE_ATOMICS depending upon whether the target compiler
# provides ARMv8's outline atomics symbols. In this case we ensure that the
# runtime system linker's symbol table includes these symbols since code generated
# by the C compiler may include references to them.
#
# This is surprisingly tricky as not all implementations provide all symbols.
# For instance:
#
#  - some compilers don't include 128-bit atomics
#  - some (misconfigured?) toolchains don't define certain _sync operations
#    (see https://bugs.gentoo.org/868018)
#
# For this reason we do not link directly against the symbols provided by
# compiler-rt/libgcc. Instead, we provide our own wrappers (defined in
# rts/ARMOutlineAtomicsSymbols.h), which should compile to equivalent code.
# This is all horrible.
#

AC_DEFUN([FP_ARM_OUTLINE_ATOMICS], [
    AC_CHECK_FUNC(
        [__aarch64_ldadd1_acq],
        [AC_DEFINE([HAVE_ARM_OUTLINE_ATOMICS], [1], [Does the toolchain use ARMv8 outline atomics])]
    )
])

