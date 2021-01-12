# Just do the opt way...
def f( name, opts ):
  opts.only_ways = ['optasm']

setTestOpts(f)

test('Cpr001', [], multimod_compile, ['Cpr001', '-v0'])
# The following test greps for type signatures of worker functions.
test('T18109', [ grep_errmsg(r'^T18109\.\$w\S+ ::') ], compile, ['-ddump-simpl -dsuppress-idinfo -dppr-cols=9999'])
test('T18174', [ grep_errmsg(r'^T18174\.\$w\S+ ::') ], compile, ['-ddump-simpl -dsuppress-idinfo -dppr-cols=9999'])
