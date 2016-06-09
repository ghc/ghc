test('sigof01',
     [ clean_cmd('rm -rf tmp_sigof01') ],
     run_command,
     ['$MAKE -s --no-print-directory sigof01'])

test('sigof01m',
     [ expect_broken(12189), clean_cmd('rm -rf tmp_sigof01m'), normalise_slashes ],
     run_command,
     ['$MAKE -s --no-print-directory sigof01m'])
