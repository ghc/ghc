test('T15894',
     [extra_files(['copysign.c', 'main.hs']), when(ghc_dynamic(), skip)],
     run_command, ['$MAKE -s --no-print-directory T15894'])
