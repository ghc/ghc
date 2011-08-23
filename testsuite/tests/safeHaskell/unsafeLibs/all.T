# Just do the normal way, SafeHaskell is all in the frontend
def f( opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

test('Dep01', normal, compile, [''])
test('Dep02', normal, compile, [''])
test('Dep03', normal, compile, [''])
test('Dep04', normal, compile, [''])
test('Dep05', normal, compile, [''])
test('Dep06', normal, compile, [''])
test('Dep07', normal, compile, [''])
test('Dep08', normal, compile, [''])
test('Dep09', normal, compile, [''])
test('Dep10', normal, compile, [''])

test('BadImport01', normal, compile_fail, [''])
test('BadImport02', extra_clean(['BadImport02_A.hi', 'BadImport02_A.o']), multimod_compile_and_run, ['BadImport02', ''])
test('BadImport03', normal, multimod_compile_fail, ['BadImport03', ''])

