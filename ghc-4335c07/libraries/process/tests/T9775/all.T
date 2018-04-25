
test('T9775_fail',
     [extra_clean(['ok.o', 'ok.exe', 'main.o', 'main.exe']),
      extra_files(['ok.c', 'main.c']),
      unless(opsys('mingw32'),skip),
      pre_cmd('$MAKE -s --no-print-directory T9775')],
      compile_and_run, [''])

test('T9775_good',
     [extra_clean(['ok.o', 'ok.exe', 'main.o', 'main.exe']),
      unless(opsys('mingw32'),skip),
      extra_files(['ok.c', 'main.c']),
      pre_cmd('$MAKE -s --no-print-directory T9775')],
      compile_and_run, [''])
