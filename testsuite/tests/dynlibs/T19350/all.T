# Test for #19350
# ================
#
# The plan
# ---------
# We build a C shared library (`libhello`, contained in ./clib) and then build
# a Haskell library (`T19350-lib`, in ./lib) which depends upon it via `foreign
# import`. We make sure that the libhello shared object can only be found via
# the extra-lib-dirs from the package database registration (which we do by
# moving libhello.so from its original place).
#
# Finally, we enter GHCi, load the Haskell library, and try to use it to call
# into libhello.
#

test('T19350', [req_shared_libs, expect_broken(19350), extra_files(['clib', 'lib'])], makefile_test, [])
