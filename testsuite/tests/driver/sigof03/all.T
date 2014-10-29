test('sigof03',
     [ clean_cmd('rm -rf tmp_sigof03') ],
     run_command,
     ['$MAKE -s --no-print-directory sigof03'])

# This doesn't work yet, because the instances aren't found the
# right way (they don't go in the EPS, differently from one-shot)
test('sigof03m',
     [ clean_cmd('rm -rf tmp_sigof03m'), expect_fail ],
     run_command,
     ['$MAKE -s --no-print-directory sigof03m'])
