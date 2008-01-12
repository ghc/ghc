test('bug1677', if_compiler_ge('ghc', '6.9', expect_broken(2034)),
                run_command, ['$MAKE bug1677 -s --no-print-directory'])
