test('2014', [ ignore_output,
               extra_clean(['A.hi-boot','A.hi','A.o','A.o-boot',
                            'B.hi', 'B.o', 'C.hi', 'C.o']) ],
              run_command, ['$MAKE 2014'])
