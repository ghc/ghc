# This directory contains tests where we annotate functions with expected
# cpr signatures, and verify that these are actually found by the compiler

setTestOpts(extra_hc_opts('-ddump-cpr-signatures'))

# We are testing the result of an optimization, so no use
# running them in various runtimes
setTestOpts(only_ways(['optasm']))

test('FacState', normal, compile, [''])
test('CaseBinderCPR', normal, compile, [''])
test('T5075', normal, compile, [''])
test('T10694', normal, compile, [''])
test('T1600', normal, compile, [''])
test('DataConWrapperCpr', normal, compile, [''])
