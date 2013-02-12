test('recomp006',
     [ when(fast(), skip),
       extra_clean(['A.o','A.hi','B.o','B.hi','err','out']) ],
     run_command, ['$MAKE -s --no-print-directory recomp006'])
