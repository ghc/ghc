test('T4891', extra_clean(['X.hi', 'X.o']),
              run_command,
              ['$MAKE -s --no-print-directory T4891'])
