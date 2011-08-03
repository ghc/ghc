# Test for bug #713, results in crashes in GHC prior to 20060315 with +RTS -N2

# the conditions are fairly delicate.  It must be compiled without optimisation,
# hence -O0:

test('concprog002',
        # Add 'threaded2_hT' so that we have at least one test for bug #5127
     [only_ways(['threaded2','threaded2_hT']),
      extra_ways(['threaded2_hT']),
      exit_code(1),
      skip_if_fast,
      reqlib('random'),
      extra_clean(['Event.hi',     'Event.o',
                   'Scheduler.hi', 'Scheduler.o',
                   'Server.hi',    'Server.o',
                   'Thread.hi',    'Thread.o'])],
     multimod_compile_and_run,
     ['Server','-O0'])
