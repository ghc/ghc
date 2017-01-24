test('path_with_commas',
     [ignore_stderr,
      unless(have_dynamic(),skip)],
     run_command,
     ['$MAKE -s --no-print-directory path_with_commas'])
