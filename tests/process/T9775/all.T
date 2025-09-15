
test('T9775_fail',
     [extra_files(['ok.c', 'main.c']),
      unless(opsys('mingw32'),skip),
      pre_cmd('$MAKE -s --no-print-directory T9775'),
      req_process],
      compile_and_run, [''])

test('T9775_good',
     [unless(opsys('mingw32'),skip),
      extra_files(['ok.c', 'main.c']),
      pre_cmd('$MAKE -s --no-print-directory T9775'),
      req_process],
      compile_and_run, [''])
