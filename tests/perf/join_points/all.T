# Only compile with optimisation
def f( name, opts ):
  opts.only_ways = ['optasm']

setTestOpts(f)

test('join001', normal, compile, [''])

test('join002',
  [collect_stats('bytes allocated',5),],
  compile_and_run,
  [''])
test('join003',
  [collect_stats('bytes allocated',5),],
  compile_and_run,
  [''])
test('join004',
  [collect_stats('bytes allocated',5),],
  compile_and_run,
  ['-fno-stg-lift-lams'])

test('join005', normal, compile, [''])
test('join006', normal, compile, [''])

test('join007',
  [collect_stats('bytes allocated',5),],
  compile_and_run,
  [''])

