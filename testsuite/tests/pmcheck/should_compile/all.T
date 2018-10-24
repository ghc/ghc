
# Tests for pattern match checker (coverage and exhaustiveness)

# Just do the normal way...
def f( name, opts ):
  opts.only_ways = ['normal']

setTestOpts(f)

# Bug reports / feature requests
test('T2006', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T2204', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T3078', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T322', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T366', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T3927a', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T3927b', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T3927', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T4139', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T6124', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T7669a', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T8970', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T9951b', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T9951', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T11303', normal, compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns +RTS -M1G -RTS'])
test('T11276', compiler_stats_num_field('bytes allocated',
  [(wordsize(64), 165890392, 10)]
    # 2018-07-14: 165890392   INITIAL
  ), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns +RTS -M1G -RTS'])

test('T11303b', compiler_stats_num_field('bytes allocated',
  [(wordsize(64), 54373936, 10)]
    # 2018-07-14: 54373936    INITIAL
  ), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns +RTS -M1G -RTS'])

test('T11374', compiler_stats_num_field('bytes allocated',
  [(wordsize(64), 280144864, 10)]
    # 2018-07-14: 280144864   INITIAL
  ), compile, ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns +RTS -M1G -RTS'])

test('T11195', compiler_stats_num_field('bytes allocated',
  [(wordsize(64), 7852567480, 10)]
    # 2018-07-14: 7852567480   INITIAL
  ), compile, ['-package ghc -fwarn-incomplete-patterns -fwarn-overlapping-patterns +RTS -M2G -RTS'])

test('T11984', normal, compile,
    ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T14086', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T14098', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T15385', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T15450', normal, compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])

# Other tests
test('pmc001', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc002', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc003', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc004', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc005', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc006', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('pmc007', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T11245', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('T12957', [], compile, ['-fwarn-overlapping-patterns'])
test('T12957a', [], compile, ['-fwarn-overlapping-patterns'])

# EmptyCase
test('T10746', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase001', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase002', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase003', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase004', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase005', [], compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase006', [],  compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase007', [],  compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase008', [],  compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase009', [],  compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
test('EmptyCase010', [],  compile,
     ['-fwarn-incomplete-patterns -fwarn-overlapping-patterns'])
