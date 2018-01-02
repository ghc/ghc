if config.cleanup:
   cleanup = 'CLEANUP=1'
else:
   cleanup = 'CLEANUP=0'

test('T14304',
     [extra_files(['p', 'indef', 'th', 'Setup.hs']),
      unless(have_dynamic(), skip)],
     run_command,
     ['$MAKE -s --no-print-directory T14304 ' + cleanup])
