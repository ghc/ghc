# We only want to run these tests with GHCi
def f( opts ):
  opts.only_ways = ['ghci']
setTestOpts(f)

test('ghci001', normal, compile_and_run, [''])
test('ghci002', normal, compile_and_run, [''])
test('ghci003', normal, compile_and_run, [''])
