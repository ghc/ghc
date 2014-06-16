# Just do the opt way...
def f( name, opts ):
  opts.only_ways = ['optasm']

setTestOpts(f)

test('Cpr001',
     extra_clean(['Cpr001_imp.hi', 'Cpr001_imp.o', 'Cpr001_imp.comp.stderr']),
     multimod_compile,
     ['Cpr001', '-v0'])
