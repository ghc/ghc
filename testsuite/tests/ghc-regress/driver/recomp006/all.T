test('recomp006',
     [ skip_if_fast, extra_clean(['A.o','A.hi','B.o','B.hi','err','out']) ],
     run_command, ['$MAKE -s --no-print-directory recomp006'])
