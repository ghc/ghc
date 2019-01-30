test('T14854',
     [only_ways(threaded_ways),
      omit_ways('ghci'),
      reqlib('random'),
      ignore_stderr],
     compile_and_run,
     ['-package ghc'])

test('T15953', [ignore_stdout], run_command, ['$MAKE --no-print-directory -s T15953'])
