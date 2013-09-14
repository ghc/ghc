test('T7478', [extra_clean(['A','A.exe','B.o','B.hi','C.o','C.hi']), when(opsys('darwin'), expect_broken(8294))], run_command, ['$MAKE -s --no-print-directory T7478'])

