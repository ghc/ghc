test('T7962', req_interp, makefile_test, ['T7962'])

test('T9905fail1', req_interp, makefile_test, ['T9905fail1'])

test('T9905fail2', req_interp, makefile_test, ['T9905fail2'])

test('T9905fail3', req_interp, makefile_test, ['T9905fail3'])

test('ghc-e-fail1', req_interp, makefile_test, ['ghc-e-fail1'])

test('ghc-e-fail2', req_interp, makefile_test, ['ghc-e-fail2'])

# Don't run on Windows, as executable is written to T9930.exe
# and no failure is induced.
test('T9930fail', [extra_files(['T9930']), when(opsys('mingw32'), skip)],
     makefile_test, ['T9930fail'])
