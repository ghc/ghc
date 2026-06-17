# Args to compile_and_run are:
#	extra compile flags
#	extra run flags
#	expected process return value, if not zero

test('DsStrictFail', exit_code(1), compile_and_run, [''])
test(
    'T21701',
    [omit_ways(llvm_ways)], # -finfo-table-map does not work with -fllvm (#26435)
    compile,
    ['-Wall -finfo-table-map']
)
