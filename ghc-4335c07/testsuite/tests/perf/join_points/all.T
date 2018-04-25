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
  [stats_num_field('bytes allocated', [(wordsize(64), 16130592, 5)])],
   # 2017-01-24   48146720    Join point rework
   # 2017-02-05   16130592    Do Worker/Wrapper for NOINLINE things
  compile_and_run,
  [''])

test('join005', normal, compile, [''])
test('join006', normal, compile, [''])

test('join007',
  [stats_num_field('bytes allocated',
      [(platform('x86_64-unknown-mingw32'), 47368, 5),
       # 2017-02-19                         47368 (x64/Windows) - Unknown

       (wordsize(64), 50944, 5)])],
       # Initial 50944
  compile_and_run,
  [''])
