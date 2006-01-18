# Test for bug #1285326, results in "internal error: scavenge_one:
# strange object 47" with GHC 6.4, fixed in 6.4.1.

test('concprog001', skip_if_fast, multimod_compile_and_run, ['Mult',''])
clean_o_hi()
