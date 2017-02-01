# Only compile with optimisation
def f( name, opts ):
  opts.only_ways = ['optasm']

setTestOpts(f)

test('join001', normal, compile, [''])

test('join002',
  [stats_num_field('bytes allocated', [(wordsize(64), 2000290792, 5)])],
  compile_and_run,
  [''])
test('join003',
  [stats_num_field('bytes allocated', [(wordsize(64), 2000290792, 5)])],
  compile_and_run,
  [''])
test('join004',
  [stats_num_field('bytes allocated', [(wordsize(64), 48146720, 5)])],
  compile_and_run,
  [''])

test('join005', normal, compile, [''])
test('join006', normal, compile, [''])

test('join007',
  [stats_num_field('bytes allocated', [(wordsize(64), 50944, 5)])],
  compile_and_run,
  [''])
