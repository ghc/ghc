# Just do the opt way...
def f( opts ):
  opts.only_ways = ['optc','optasm']

setTestOpts(f)

test('Cpr001_imp', no_clean, compile, [''])
test('Cpr001', normal, compile, [''])
clean(['Cpr001_imp.hi', 'Cpr001_imp.o', 'Cpr001_imp.comp.stderr'])
