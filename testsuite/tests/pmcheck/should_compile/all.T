
# Tests for pattern match checker (coverage and exhaustiveness)

# Just do the normal way...
def f( name, opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

# Bug reports / feature requests
test('T2006', only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T2204', only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T3078', only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T322',  only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T366',  only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T3927a',only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T3927b',only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T3927', only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T4139', only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T6124', only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T7669a', only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T8970', only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T9951b',only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T9951', only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T11303', normal, compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns +RTS -M1G -RTS'])
test('T11276', compile_timeout_multiplier(0.01), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns +RTS -M1G -RTS'])

# Other tests
test('pmc001', only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc002', only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc003', only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc004', only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc005', only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc006', only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc007', only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T11245', only_compiler_types(['ghc']), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
