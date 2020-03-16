# This directory contains tests where we annotate functions with expected
# type signatures, and verify that these actually those found by the compiler

setTestOpts(extra_hc_opts('-ddump-str-signatures -ddump-cpr-signatures'))

# We are testing the result of an optimization, so no use
# running them in various runtimes
setTestOpts(only_ways(['optasm']))

test('StrAnalExample', normal, compile, [''])
test('T8569', normal, compile, [''])
test('HyperStrUse', normal, compile, [''])
test('T8598', normal, compile, [''])
test('FacState', expect_broken(1600), compile, [''])
test('UnsatFun', normal, compile, [''])
test('BottomFromInnerLambda', normal, compile, [''])
test('DmdAnalGADTs', normal, compile, [''])
test('T12370', normal, compile, [''])
test('CaseBinderCPR', normal, compile, [''])
test('NewtypeArity', normal, compile, [''])
test('T5075', normal, compile, [''])
test('T17932', normal, compile, [''])
test('T13380c', normal, compile, [''])
