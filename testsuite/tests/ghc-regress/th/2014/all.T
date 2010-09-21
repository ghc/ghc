setTestOpts(if_compiler_profiled(skip))

test('2014',
     extra_clean(['A.hi-boot','A.hi','A.o','A.o-boot',
                  'B.hi', 'B.o', 'C.hi', 'C.o']),
     run_command,
     ['$MAKE -s --no-print-directory 2014'])
