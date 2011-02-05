test('recomp003',
     extra_clean(['Data/Char.hs', 'Data/Char.hi', 'Data/Char.o',
                  'A.o', 'A.hi',
                  'err']),
     run_command,
     ['$MAKE -s --no-print-directory recomp003'])
