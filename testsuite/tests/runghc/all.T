test('T7859', req_interp, makefile_test, [])

test('T8601', req_interp, makefile_test, [])

test('T11247', [req_interp, expect_broken(11247)], makefile_test, [])

test('T6132', [
     when(opsys('darwin') or opsys('freebsd'), expect_broken(6132)),
     # When using clang as CC, which more recent LLVM toolchains will
     # require as llc will produce assembly that GAS doesn't like,
     # we'll end up with the following issue where clang does not
     # like the shebang.
     # https://bugs.llvm.org/show_bug.cgi?id=40689
     omit_ways(llvm_ways)],
     compile, [''])

test('T17171a',
     [req_interp, exit_code(2), ignore_stdout, grep_errmsg(r'main')],
     run_command, ['$MAKE -s --no-print-directory T17171a'])
test('T17171b', req_interp, run_command,
     ['$MAKE -s --no-print-directory T17171b'])

test('T-signals-child',
     [ when(opsys('mingw32'), skip), req_interp
     , only_ways(['threaded1', 'threaded2'])
     , fragile_for(17307, ['threaded1', 'threaded2', 'nonmoving_thr'])
     ],
     makefile_test, [])
