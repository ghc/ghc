# Test for bug #713, results in crashes in GHC prior to 20060315 with +RTS -N2

# Add 'threaded2_hT' so that we have at least one test for bug #5127
if (ghc_with_threaded_rts == 1 and ghc_with_smp == 1):
   ways = ['threaded2_hT']
else:
   ways = []

# the conditions are fairly delicate.  It must be compiled without optimisation,
# hence -O0:

test('concprog002',
     [extra_files(['Event.hs', 'Scheduler.hs', 'Server.hs', 'Thread.hs']),
      only_ways(['threaded2', 'threaded2_hT']), extra_ways(ways), exit_code(1),
      when(fast(), skip), reqlib('random')],
     multimod_compile_and_run, ['Server', '-O0'])
