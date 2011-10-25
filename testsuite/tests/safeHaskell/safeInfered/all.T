# safeInfered tests are for testing that the Safe Haskell
# mode safe inference works correctly.

# Just do the normal way, SafeHaskell is all in the frontend
def f( opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

# Tests that should compile fine as they should be infered safe
test('SafeInfered01', normal, multimod_compile, ['SafeInfered01', ''])
test('SafeInfered02', normal, multimod_compile, ['SafeInfered02', ''])
test('SafeInfered03', normal, multimod_compile, ['SafeInfered03', ''])
test('SafeInfered04', normal, multimod_compile, ['SafeInfered04', ''])

# Tests that should fail to compile as they should be infered unsafe
test('UnsafeInfered01', normal, multimod_compile_fail, ['UnsafeInfered01', ''])
test('UnsafeInfered02', normal, multimod_compile_fail, ['UnsafeInfered02', ''])
test('UnsafeInfered03', normal, multimod_compile_fail, ['UnsafeInfered03', ''])
test('UnsafeInfered05', normal, multimod_compile_fail, ['UnsafeInfered05', ''])
test('UnsafeInfered06', normal, multimod_compile_fail, ['UnsafeInfered06', ''])
test('UnsafeInfered07', normal, multimod_compile_fail, ['UnsafeInfered07', ''])
test('UnsafeInfered08', normal, multimod_compile_fail, ['UnsafeInfered08', ''])
test('UnsafeInfered09', normal, multimod_compile_fail, ['UnsafeInfered09', ''])

# Mixed tests
test('Mixed01', normal, compile_fail, [''])
test('Mixed02', normal, compile_fail, [''])
test('Mixed03', normal, compile_fail, [''])

