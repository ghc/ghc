test('sigof01',
     [ clean_cmd('rm -rf tmp_sigof01') ],
     run_command,
     ['$MAKE -s --no-print-directory sigof01'])

test('sigof01m',
     [ clean_cmd('rm -rf tmp_sigof01m') ],
     run_command,
     ['$MAKE -s --no-print-directory sigof01m'])
