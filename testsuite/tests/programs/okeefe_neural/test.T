
# this one causes the compiler to run out of heap in the simplifier

def set_opts( name, opts ):
  opts.expect = 'fail'

test('okeefe_neural',
     [skip_if_fast,
      set_opts,
      extra_clean(['Main.hi'])],
     multimod_compile_and_run,
     ['Main', '-package lang +RTS -M64m -RTS'])
