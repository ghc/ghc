# Only compile with optimisation
setTestOpts( only_ways(['opt']) )

test('head', normal, compile, ['-fesc -ddump-esc'])
test('sum', normal, compile, ['-fesc -ddump-esc'])
test('dataA', normal, compile, ['-fesc -ddump-esc'])
