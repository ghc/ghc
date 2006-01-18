
# this one causes the compiler to run out of heap in the simplifier

def set_opts( opts ):
  opts.expect = 'fail'
	# Skip the test on alpha-dec-osf3, where we
	# run into an infinite loop on some builds
  if config.platform == "alpha-dec-osf3":
	opts.skip = 1;

test('okeefe_neural', compose(skip_if_fast,set_opts), multimod_compile_and_run, \
	['Main', '-package lang +RTS -M64m -RTS'])
clean(['Main.hi'])
