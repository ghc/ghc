# Just do the opt way...
def f( name, opts ):
  opts.only_ways = ['optasm']

setTestOpts(f)

test('Cpr001', [], multimod_compile, ['Cpr001', '-v0'])
# The following tests grep for type signatures of worker functions.
test('T18109', [ grep_errmsg(r'^T18109\.\$w\S+ ::') ], compile, ['-ddump-simpl -dsuppress-idinfo -dppr-cols=9999'])
test('T18174', [ grep_errmsg(r'^T18174\.\$w\S+ ::') ], compile, ['-ddump-simpl -dsuppress-idinfo -dppr-cols=9999'])
# T18401 probably needs -flate-dmd-anal so that it runs after SpecConstr.
# It is currently broken, but not marked expect_broken. We can't know the exact
# name of the function before it is fixed, so expect_broken doesn't make sense.
test('T18401', [ grep_errmsg(r'^T18401\.\S+ ::') ], compile, ['-ddump-simpl -dsuppress-idinfo -dppr-cols=9999 -flate-dmd-anal'])
