test('T6145', when(fast(), skip),
              run_command,
              ['$MAKE -s --no-print-directory T6145'])
