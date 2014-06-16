
test('dynamicToo003',
     [extra_clean(['A003.o', 'A003.hi', 'A003.dyn_o', 'A003.dyn_hi']),
      only_compiler_types(['ghc']),
      unless(have_vanilla(), skip),
      unless(have_dynamic(), skip)],
     run_command,
     ['$MAKE -s --no-print-directory dynamicToo003'])

