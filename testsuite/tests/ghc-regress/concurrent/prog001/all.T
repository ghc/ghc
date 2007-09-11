# Test for bug #1285326, results in "internal error: scavenge_one:
# strange object 47" with GHC 6.4, fixed in 6.4.1.

# Also tests for bug #1466.

# NB. This is a VERY IMPORTANT test!  It is the only good test we have
# for throwTo.  It has shown up several bugs that were not caught by the 
# other concurrency tests.

test('concprog001', skip_if_fast, multimod_compile_and_run, ['Mult',''])
clean_o_hi()
