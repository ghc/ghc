test('T4891', [when(fast(), skip), extra_clean(['X.hi', 'X.o'])],
              run_command,
              ['$MAKE -s --no-print-directory T4891'])
