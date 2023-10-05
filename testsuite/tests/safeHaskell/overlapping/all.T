# overlapping tests Safe Haskell's handling of overlapping instances.

# Just do the normal way, SafeHaskell is all in the frontend
def f( name, opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

test('SH_Overlap1', [], multimod_compile_fail, ['SH_Overlap1', ''])

test('SH_Overlap2', [], multimod_compile_fail, ['SH_Overlap2', ''])

test('SH_Overlap3', [], multimod_compile, ['SH_Overlap3', ''])

test('SH_Overlap4', [], multimod_compile, ['SH_Overlap4', ''])

test('SH_Overlap5', [], multimod_compile_fail, ['SH_Overlap5', ''])

test('SH_Overlap6', [], multimod_compile_fail, ['SH_Overlap6', ''])

test('SH_Overlap7', [], multimod_compile_fail, ['SH_Overlap7', '-Werror'])

test('SH_Overlap8', [], multimod_compile, ['SH_Overlap8', ''])

test('SH_Overlap9', [], multimod_compile, ['SH_Overlap9', '-Werror'])

test('SH_Overlap10', [], multimod_compile, ['SH_Overlap10', '-Werror'])

test('SH_Overlap11', [], multimod_compile, ['SH_Overlap11', ''])
