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

# Exitify is (one of) the first passes after worker/wrapper.
# The CPR sig on the join point that sucked in a `case of` should have been nuked here.
# The Regex works by grepping for the IdDetails line of the join point, which used to look like
#     [LclId[JoinId(1)], Arity=1, Str=<L>, Cpr=1, Unf=...]
# It won't match if the Cpr=1 is missing, which is what we're trying to assess.
test('T18824', [ grep_errmsg(r'JoinId[^\n]*Cpr') ], compile, ['-ddump-exitify -dppr-cols=1000 -dsuppress-uniques'])

test('T20539', [], compile, ['']) # simply should not crash
