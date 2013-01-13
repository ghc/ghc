# Tests for LLVM code generator

def f( opts ):
  opts.only_ways = ['optllvm', 'llvm', 'debugllvm']

setTestOpts(f)

test('5054', reqlib('hmatrix'), compile, ['-package hmatrix'])
test('5054_2', reqlib('hmatrix'), compile, ['-package hmatrix'])
test('5486', normal, compile, [''])
test('5681', normal, compile, [''])
test('6158', [reqlib('vector'), reqlib('primitive')], compile, ['-package vector -package primitive'])
test('T7571', cmm_src, compile, [''])
