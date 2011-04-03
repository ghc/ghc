test('T4891', [skip_if_fast, extra_clean(['X.hi', 'X.o'])],
              run_command,
              ['$MAKE -s --no-print-directory T4891'])
