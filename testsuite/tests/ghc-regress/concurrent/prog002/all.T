# Test for bug #713, results in crashes in GHC prior to 20060315 with +RTS -N2

# the conditions are fairly delicate.  It must be compiled without optimisation,
# hence -O0:

test('concprog002', compose(only_ways(['threaded2']), 
		    compose(exit_code(1), 
			    skip_if_fast)), 
	multimod_compile_and_run, ['Server','-debug -O0'])
clean_o_hi()
