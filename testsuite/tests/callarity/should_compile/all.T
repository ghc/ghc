# Only compile with optimisation
setTestOpts( only_ways(['optasm']) )

# The gist here is that the n_X1 :: Double -> Double binding we see in
# CallArity will be eta-expanded, inlined and thus not be seen in simplified
# output. There should be no other Double -> Double bindings, so testing for
# the absence of the sig should be reasonably precise.
test('T20283',  [ grep_errmsg(r':: Double -> Double') ], compile, ['-dppr-cols=1000 -ddump-simpl'])
