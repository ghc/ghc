# Only compile with optimisation
def f( name, opts ):
  opts.only_ways = ['optasm']

setTestOpts(f)

test('join001', normal, compile, [''])

test('join002',
  [comparison('bytes allocated',5),],
  compile_and_run,
  [''])
test('join003',
  [comparison('bytes allocated',5),],
  compile_and_run,
  [''])
test('join004',
  [comparison('bytes allocated',5),],
  compile_and_run,
  [''])

test('join005', normal, compile, [''])
test('join006', normal, compile, [''])

test('join007',
  [comparison('bytes allocated',5),],
  compile_and_run,
  [''])
