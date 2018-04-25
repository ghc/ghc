test('bug1677', extra_files(['Bar.hs', 'Foo.hs']), run_command, ['$MAKE bug1677 -s --no-print-directory'])
