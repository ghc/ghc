setTestOpts(only_compiler_types(['ghc']))

test('dynamicToo001',
     [extra_clean(['A001.o',      'B001.o',      'C001.o',
                   'A001.hi',     'B001.hi',     'C001.hi',
                   'A001.dyn_o',  'B001.dyn_o',  'C001.dyn_o',
                   'A001.dyn_hi', 'B001.dyn_hi', 'C001.dyn_hi',
                   's001', 'd001']),
      when(opsys('mingw32'), expect_broken(7665)),
      unless(have_vanilla(), skip),
      unless(have_dynamic(), skip)],
     run_command,
     ['$MAKE -s --no-print-directory dynamicToo001'])

test('dynamicToo002',
     [extra_clean(['A002.o',      'B002.o',      'C002.o',
                   'A002.hi',     'B002.hi',     'C002.hi',
                   'A002.dyn_o',  'B002.dyn_o',  'C002.dyn_o',
                   'A002.dyn_hi', 'B002.dyn_hi', 'C002.dyn_hi']),
      unless(have_vanilla(), skip),
      unless(have_dynamic(), skip)],
     run_command,
     ['$MAKE -s --no-print-directory dynamicToo002'])

