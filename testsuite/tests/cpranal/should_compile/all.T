# Just do the opt way...
def f( name, opts ):
  opts.only_ways = ['optasm']

setTestOpts(f)

test('Cpr001', [], multimod_compile, ['Cpr001', '-v0'])

# Exitify is (one of) the first passes after worker/wrapper.
# The CPR sig on the join point that sucked in a `case of` should have been nuked here.
# The Regex works by grepping for the IdDetails line of the join point, which used to look like
#     [LclId[JoinId(1)], Arity=1, Str=<L>, Cpr=1, Unf=...]
# It won't match if the Cpr=1 is missing, which is what we're trying to assess.
test('T18824', [ grep_errmsg(r'JoinId[^\n]*Cpr') ], compile, ['-ddump-exitify -dppr-cols=1000 -dsuppress-uniques'])
