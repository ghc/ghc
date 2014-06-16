# This is a program for benchmarking mutable linked list
# implementations by Martin Sulzmann and Simon Marlow.  The reason it
# is here is that one variant of it demonstrated a nasty bug in the
# RTS, see the patch "Fix a bug that can lead to noDuplicate# not
# working sometimes", and the comments with stg_noDuplicatzh in
# rts/PrimOps.cmm.
#
# This test won't actually demonstrate the bug since it needs -N3 or
# greater (and hence a 3-core box) and the bug only manifests very
# occasionally, but at least the test is here for posterity.

test('concprog003',
     [when(fast(), skip),
      extra_clean([
           'BackList2.hi', 'BackList2.o',
           'ImmList.hi', 'ImmList.o',
           'MainMVarList.hi', 'MainMVarList.o',
           'TestDataParser.hi', 'TestDataParser.o',
           'CASList.hi', 'CASList.o',
           'IOList.hi', 'IOList.o',
           'MVarListLockCoupling.hi', 'MVarListLockCoupling.o',
           'TestRun.hi', 'TestRun.o',
           'Collate.hi', 'Collate.o',
           'LazyList2.hi', 'LazyList2.o',
           'RefInterface.hi', 'RefInterface.o',
           'Collection.hi', 'Collection.o',
           'Main.hi', 'Main.o',
           'TestData.hi', 'TestData.o'
         ]),
      reqlib('random'),
      extra_run_opts('IMM -t test-8-3000-3000-2-1-4'),
     ],
     multimod_compile_and_run,
     ['Main',''])
