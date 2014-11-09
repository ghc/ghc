test('sigof02',
     [ clean_cmd('rm -rf tmp_sigof02') ],
     run_command,
     ['$MAKE -s --no-print-directory sigof02'])

test('sigof02t',
     [ clean_cmd('rm -rf tmp_sigof02t') ],
     run_command,
     ['$MAKE -s --no-print-directory sigof02t'])

test('sigof02m',
     [ clean_cmd('rm -rf tmp_sigof02m') ],
     run_command,
     ['$MAKE -s --no-print-directory sigof02m'])

test('sigof02mt',
     [ clean_cmd('rm -rf tmp_sigof02mt') ],
     run_command,
     ['$MAKE -s --no-print-directory sigof02mt'])

test('sigof02d',
     [ clean_cmd('rm -rf tmp_sigof02d') ],
     run_command,
     ['$MAKE -s --no-print-directory sigof02d'])

test('sigof02dt',
     [ clean_cmd('rm -rf tmp_sigof02dt') ],
     run_command,
     ['$MAKE -s --no-print-directory sigof02dt'])


test('sigof02dm',
     [ clean_cmd('rm -rf tmp_sigof02dm') ],
     run_command,
     ['$MAKE -s --no-print-directory sigof02dm'])

test('sigof02dmt',
     [ clean_cmd('rm -rf tmp_sigof02dmt') ],
     run_command,
     ['$MAKE -s --no-print-directory sigof02dmt'])

