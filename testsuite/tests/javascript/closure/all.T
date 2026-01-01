# These are JavaScript-specific tests based on Google Closure Compiler
setTestOpts(when(not(js_arch()),skip))

# Runs fine locally. Fails on CI with:
#
# Wrong exit code for T24602()(expected 0 , actual 2 )
# Stderr  run  ( T24602 ):
# gmake: node: Permission denied
# gmake: *** [Makefile:11: T24602] Error 127
# *** unexpected failure for T24602(normal)
test('T24602', fragile(26712), makefile_test, ['T24602'])
