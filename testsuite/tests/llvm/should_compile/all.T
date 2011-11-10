# Tests for LLVM code generator

def f( opts ):
  opts.only_ways = ['optllvm', 'llvm', 'debugllvm']

setTestOpts(f)

test('5054', normal, compile, [''])
test('5054_2', normal, compile, [''])

