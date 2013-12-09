# This directory contains tests where we annotate functions with expected
# type signatures, and verify that these actually those found by the compiler

setTestOpts(extra_hc_opts('-ddump-strsigs'))

# We are testing the result of an optimization, so no use
# running them in various runtimes
setTestOpts(only_ways(['optasm']))

test('StrAnalExample', normal, compile, [''])
test('T8569', expect_broken(8569), compile, [''])
test('HyperStrUse', normal, compile, [''])
test('T8598', expect_broken(8598), compile, [''])

