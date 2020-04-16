# This directory contains tests where we annotate functions with expected
# type signatures, and verify that these actually those found by the compiler

# Testcases like DmdAnalGADTs checks both demand and CPR signatures.
# CPR-only tests should go into tests/cpranal/sigs
setTestOpts(extra_hc_opts('-ddump-str-signatures -ddump-cpr-signatures'))

# We are testing the result of an optimization, so no use
# running them in various runtimes
setTestOpts(only_ways(['optasm']))

test('StrAnalExample', normal, compile, [''])
test('T8569', normal, compile, [''])
test('HyperStrUse', normal, compile, [''])
test('T8598', normal, compile, [''])
test('UnsatFun', normal, compile, [''])
test('BottomFromInnerLambda', normal, compile, [''])
test('DmdAnalGADTs', normal, compile, [''])
test('T12370', normal, compile, [''])
test('NewtypeArity', normal, compile, [''])
test('T17932', normal, compile, [''])
test('T13380c', expect_broken('!3014'), compile, [''])
test('T13380f', normal, compile, [''])
test('T18086', normal, compile, ['-package ghc'])
