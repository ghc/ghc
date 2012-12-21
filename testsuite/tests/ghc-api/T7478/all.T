test('T7478', extra_clean(['A','A.exe','B.o','B.hi','C.o','C.hi']), run_command, ['$MAKE -s --no-print-directory T7478'])

