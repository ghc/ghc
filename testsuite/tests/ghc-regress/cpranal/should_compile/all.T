# Just do the opt way...
def f( opts ):
  opts.only_ways = ['opt']

setTestOpts(f)

test('Cpr001_imp', normal, compile, [''])
test('Cpr001', normal, compile, [''])
