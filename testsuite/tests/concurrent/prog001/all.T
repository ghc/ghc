# Test for bug #1285326, results in "internal error: scavenge_one:
# strange object 47" with GHC 6.4, fixed in 6.4.1.

# Also tests for bug #1466.

# NB. This is a VERY IMPORTANT test!  It is the only good test we have
# for throwTo.  It has shown up several bugs that were not caught by the
# other concurrency tests.

# The program appears to be sensitive to scheduling, and can diverge
# in some cases.  I find that it only reliably completes when given
# multiple cores, which is why it is only running the 'threaded2' way
# right now. --SDM 1/4/2010

test('concprog001', [extra_files(['Arithmetic.hs', 'Converter.hs', 'Mult.hs', 'Stream.hs', 'Thread.hs', 'Trit.hs', 'Utilities.hs']),
                     when(fast(), skip), only_ways(['threaded2']),
                     fragile(16604),
                     run_timeout_multiplier(2)],
     multimod_compile_and_run, ['Mult', ''])
