# Test for bug #713, results in crashes in GHC prior to 20060315 with +RTS -N2

# the conditions are fairly delicate.  It must be compiled without optimisation,
# hence -O0:

test('concprog002',
     [only_ways(['threaded2','threaded2_qw']),
      exit_code(1), 
      skip_if_fast,
      extra_clean(['Event.hi',     'Event.o',
                   'Scheduler.hi', 'Scheduler.o',
                   'Server.hi',    'Server.o',
                   'Thread.hi',    'Thread.o'])],
     multimod_compile_and_run,
     ['Server','-O0'])
