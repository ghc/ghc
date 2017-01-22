# Just do the opt way...
def f( name, opts ):
  opts.only_ways = ['optasm']

setTestOpts(f)

test('Cpr001', [], multimod_compile, ['Cpr001', '-v0'])
