# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

test('DsStrictFail', [javascript_failures, exit_code(1)], compile_and_run, [''])
