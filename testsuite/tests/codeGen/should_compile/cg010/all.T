
test( 'cg010'
    , [extra_files(['A.hs','Main.hs'])]
    , makefile_test
    , ['cg010'])

# test('concprog003',
#      [extra_files(['CASList.hs', 'Collection.hs', 'IOList.lhs', 'ImmList.hs', 'MVarListLockCoupling.hs', 'Main.lhs', 'RefInterface.hs', 'TestData.hs', 'TestDataParser.hs', 'TestRun.hs', 'test-8-3000-3000-2-1-4']),
#       when(fast(), skip), reqlib('random'),
#       extra_run_opts('IMM -t test-8-3000-3000-2-1-4')],
#      multimod_compile_and_run, ['Main', ''])

# test('T10482',  [ grep_errmsg(r'wfoo.*Int#') ], compile, ['-dppr-cols=200 -ddump-cmm'])