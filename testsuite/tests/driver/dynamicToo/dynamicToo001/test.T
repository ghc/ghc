
test('dynamicToo001',
     [extra_clean(['A.o', 'A.hi', 'A.dyn_o', 'A.dyn_hi',
                   'B.o', 'B.hi', 'B.dyn_o', 'B.dyn_hi',
                   'C.o', 'C.hi', 'C.dyn_o', 'C.dyn_hi',
                   's', 's.exe', 'd', 'd.exe']),
      only_compiler_types(['ghc']),
      when(opsys('mingw32'), expect_broken(7665)),
      unless(have_vanilla(), skip),
      unless(have_dynamic(), skip)],
     run_command,
     ['$MAKE -s --no-print-directory dynamicToo001'])

