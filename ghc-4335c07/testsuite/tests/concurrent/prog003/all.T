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
     [extra_files(['CASList.hs', 'Collection.hs', 'IOList.lhs', 'ImmList.hs', 'MVarListLockCoupling.hs', 'Main.lhs', 'RefInterface.hs', 'TestData.hs', 'TestDataParser.hs', 'TestRun.hs', 'test-8-3000-3000-2-1-4']),
      when(fast(), skip), reqlib('random'),
      extra_run_opts('IMM -t test-8-3000-3000-2-1-4')],
     multimod_compile_and_run, ['Main', ''])
