# safeInfered tests are for testing that the Safe Haskell
# mode safe inference works correctly.

# Just do the normal way, SafeHaskell is all in the frontend
def f( name, opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

# Tests that should compile fine as they should be inferred safe
test('SafeInfered01', [], multimod_compile, ['SafeInfered01', ''])
test('SafeInfered02', [], multimod_compile, ['SafeInfered02', ''])
test('SafeInfered03', [], multimod_compile, ['SafeInfered03', ''])
test('SafeInfered04', [], multimod_compile, ['SafeInfered04', ''])
test('SafeInfered05', [], multimod_compile, ['SafeInfered05', ''])

# Tests that should fail to compile as they should be inferred unsafe
test('UnsafeInfered01', [], multimod_compile_fail, ['UnsafeInfered01', ''])
test('UnsafeInfered02', [], multimod_compile_fail, ['UnsafeInfered02', ''])
test('UnsafeInfered03', [], multimod_compile_fail, ['UnsafeInfered03', ''])
test('UnsafeInfered05', [], multimod_compile_fail, ['UnsafeInfered05', ''])
test('UnsafeInfered06', [], multimod_compile_fail, ['UnsafeInfered06', ''])
test('UnsafeInfered08', [], multimod_compile, ['UnsafeInfered08', ''])
test('UnsafeInfered09', [], multimod_compile_fail, ['UnsafeInfered09', ''])

# Test that should fail as we disable safe inference
test('UnsafeInfered10', [], multimod_compile_fail, ['UnsafeInfered10', ''])
test('UnsafeInfered11', [], multimod_compile_fail, ['UnsafeInfered11', ''])

# Test TH is unsafe
test('UnsafeInfered12', normal, compile_fail, [''])

# Test various overlapping instance flags
# GHC 7.10 and earlier we regarded them as unsafe, but we now take an approach
# based on looking only at sites of actual overlaps
test('UnsafeInfered13', normal, compile, [''])
test('UnsafeInfered14', normal, compile, [''])
test('UnsafeInfered15', normal, compile, [''])
test('UnsafeInfered16', normal, compile, [''])
test('UnsafeInfered17', normal, compile, [''])
test('UnsafeInfered18', normal, compile, [''])
test('UnsafeInfered19', normal, compile, [''])

# Mixed tests
test('Mixed01', normal, compile_fail, [''])
test('Mixed02', normal, compile_fail, [''])
test('Mixed03', normal, compile_fail, [''])

# Trustworthy Safe modules
test('TrustworthySafe01', normal, compile, [''])
test('TrustworthySafe02', normal, compile, [''])
test('TrustworthySafe03', normal, compile, [''])
test('TrustworthySafe04', normal, compile, [''])

# Check -fwarn-unsafe works
test('UnsafeWarn01', normal, compile, [''])
test('UnsafeWarn02', normal, compile, [''])
test('UnsafeWarn03', normal, compile, [''])
test('UnsafeWarn04', normal, compile, [''])
test('UnsafeWarn05', normal, compile, [''])
test('UnsafeWarn06', normal, compile, [''])
test('UnsafeWarn07', normal, compile, [''])

# Check -fwa-safe works
test('SafeWarn01', normal, compile, [''])

test('T16689', normal, compile, ['-Wsafe'])

