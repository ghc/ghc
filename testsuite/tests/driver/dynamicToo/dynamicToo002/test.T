
test('dynamicToo002',
     [extra_clean(['A.o', 'A.hi', 'A.dyn_o', 'A.dyn_hi',
                   'B.o', 'B.hi', 'B.dyn_o', 'B.dyn_hi',
                   'C.o', 'C.hi', 'C.dyn_o', 'C.dyn_hi']),
      only_compiler_types(['ghc']),
      unless(have_vanilla(), skip),
      unless(have_dynamic(), skip)],
     run_command,
     ['$MAKE -s --no-print-directory dynamicToo002'])

