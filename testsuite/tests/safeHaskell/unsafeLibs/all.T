# unsafeLib tests are all about testing that the correct
# standard library modules have been marked as unsafe.
# e.g no importing unsafePerformIO
# Checking base package is properly safe basically

# Just do the normal way, SafeHaskell is all in the frontend
def f( name, opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

# Check correct methods are deprecated
test('Dep05', normal, compile_fail, [''])
test('Dep06', normal, compile_fail, [''])
test('Dep07', normal, compile_fail, [''])
test('Dep08', normal, compile_fail, [''])
test('Dep09', normal, compile_fail, [''])
test('Dep10', normal, compile_fail, [''])

# check unsafe modules are marked unsafe
test('BadImport01', normal, compile_fail, [''])
test('BadImport06', normal, compile_fail, [''])
test('BadImport07', normal, compile_fail, [''])
test('BadImport08', normal, compile_fail, [''])
test('BadImport09', normal, compile_fail, [''])
test('T21433', normal, compile_fail, [''])

# check safe modules are marked safe
test('GoodImport01', normal, compile, [''])
test('GoodImport02', normal, compile, [''])
test('GoodImport03', normal, compile, [''])

