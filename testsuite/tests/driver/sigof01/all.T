test('sigof01',
     [ clean_cmd('rm -rf tmp_sigof01') ],
     run_command,
     ['$MAKE -s --no-print-directory sigof01'])

test('sigof01m',
     [ clean_cmd('rm -rf tmp_sigof01m') ],
     run_command,
     ['$MAKE -s --no-print-directory sigof01m'])

test('sigof01i',
     [],
     run_command,
     ['$MAKE -s --no-print-directory sigof01i'])

test('sigof01i2',
     [],
     run_command,
     ['$MAKE -s --no-print-directory sigof01i2'])
