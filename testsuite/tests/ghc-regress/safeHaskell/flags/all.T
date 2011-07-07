# Just do the normal way, SafeHaskell is all in the frontend
def f( opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

test('SafeFlags01', normal, compile, ['-trust base'])
test('SafeFlags02', normal, compile, [''])
test('SafeFlags03', normal, compile, [''])
test('SafeFlags04', normal, compile, ['-trust base'])
test('SafeFlags05', normal, compile, ['-trust base'])
test('SafeFlags06', normal, compile, [''])
test('SafeFlags07', normal, compile, ['-trust base'])
test('SafeFlags08', normal, compile, ['-trust base'])
test('SafeFlags09', normal, compile_fail, [''])
test('SafeFlags10', normal, compile_fail, [''])
test('SafeFlags11', normal, compile, ['-trust base'])

test('Flags01', normal, compile, [''])
test('Flags02', normal, compile, [''])

