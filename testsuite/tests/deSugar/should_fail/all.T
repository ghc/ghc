# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

test('DsStrictFail', expect_fail, compile_and_run, [''])
