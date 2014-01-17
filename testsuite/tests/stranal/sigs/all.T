# This directory contains tests where we annotate functions with expected
# type signatures, and verify that these actually those found by the compiler

setTestOpts(extra_hc_opts('-ddump-strsigs'))

# We are testing the result of an optimization, so no use
# running them in various runtimes
setTestOpts(only_ways(['optasm']))

test('StrAnalExample', normal, compile, [''])
test('T8569', expect_broken(8569), compile, [''])
test('HyperStrUse', normal, compile, [''])
test('T8598', normal, compile, [''])
test('FacState', normal, compile, [''])
test('UnsatFun', normal, compile, [''])
test('BottomFromInnerLambda', normal, compile, [''])
test('DmdAnalGADTs', normal, compile, [''])
test('CaseBinderCPR', normal, compile, [''])
test('InfiniteCPR', normal, compile, [''])
