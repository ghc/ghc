# We are testing the result of an optimization, so no use
# running them in various runtimes
setTestOpts(only_ways(['optasm']))
# This directory contains tests where we annotate functions with expected
# CPR signatures, and verify that these are actually those found by the compiler
setTestOpts(extra_hc_opts('-dno-typeable-binds -ddump-cpr-signatures'))

test('CaseBinderCPR', normal, compile, [''])
test('T19232', normal, compile, [''])
test('T19398', normal, compile, [''])
test('T19822', normal, compile, [''])
