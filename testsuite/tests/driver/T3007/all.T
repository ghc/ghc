test('T3007',
     extra_files(['A/', 'B/']),
     run_command,
     ['$MAKE -s --no-print-directory T3007'])
