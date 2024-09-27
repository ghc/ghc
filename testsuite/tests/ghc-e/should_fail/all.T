test('T7962', req_interp, makefile_test, ['T7962'])

test('T9905fail1', req_interp, makefile_test, ['T9905fail1'])

test('T9905fail2', req_interp, makefile_test, ['T9905fail2'])

test('T9905fail3', req_interp, makefile_test, ['T9905fail3'])

test('ghc-e-fail1', req_interp, makefile_test, ['ghc-e-fail1'])

test('ghc-e-fail2', req_interp, makefile_test, ['ghc-e-fail2'])

# Don't run on Windows, as executable is written to T9930.exe
# and no failure is induced.
test('T9930fail',
     [extra_files(['T9930']),
      when(opsys('mingw32'), skip),
      normalise_errmsg_fun(lambda s: normalise_version_("ghc")(s).replace('ghc-<VERSION>-<HASH>','ghc')),
      ],
     makefile_test, ['T9930fail'])

test('T18441fail0', req_interp, makefile_test, ['T18441fail0'])

test('T18441fail1', req_interp, makefile_test, ['T18441fail1'])

test('T18441fail2', [req_interp, normalise_version('ghc')], makefile_test, ['T18441fail2'])

test('T18441fail3', [req_interp, ignore_stderr, exit_code(1)], run_command, ['{compiler} -e ":! abcde"'])

test('T18441fail4', req_interp, makefile_test, ['T18441fail4'])

test('T18441fail5', req_interp, makefile_test, ['T18441fail5'])

test('T18441fail6', req_interp, makefile_test, ['T18441fail6'])

test('T18441fail7', [req_interp, normalise_version('ghc')], makefile_test, ['T18441fail7'])

test('T18441fail8', [req_interp, normalise_version('ghc')], makefile_test, ['T18441fail8'])

test('T18441fail9', req_interp, makefile_test, ['T18441fail9'])

test('T18441fail10', req_interp, makefile_test, ['T18441fail10'])

test('T18441fail11', req_interp, makefile_test, ['T18441fail11'])

test('T18441fail12', req_interp, makefile_test, ['T18441fail12'])

test('T18441fail13', req_interp, makefile_test, ['T18441fail13'])

test('T18441fail14', req_interp, makefile_test, ['T18441fail14'])

test('T18441fail15', req_interp, makefile_test, ['T18441fail15'])

test('T18441fail16', req_interp, makefile_test, ['T18441fail16'])

test('T18441fail17', req_interp, makefile_test, ['T18441fail17'])

test('T18441fail18', req_interp, makefile_test, ['T18441fail18'])

test('T18441fail19', [req_interp, ignore_stderr, exit_code(1)], run_command, ['{compiler} -e ":cd abcd"'])

test('T23663', [req_interp, normalise_version('ghc')], makefile_test, ['T23663'])

test('T24172', normal, compile_fail, ['-fdiagnostics-color=always'])
