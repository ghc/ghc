
# this one causes the compiler to run out of heap in the simplifier

# TODO. What's the purpose of this test? If you give it 100Mb of heap, it
# compiles fine (though it takes a while). Is that too much?
# Does the fact that this test is marked expect_fail (instead of expect_broken)
# mean anything?
# Is it necessary to also run the resulting program? It doesn't seem to ever
# complete, at least not in a few minutes. What is the expected output?
def set_opts( name, opts ):
  opts.expect = 'fail'

test('okeefe_neural', [extra_files(['Main.hs']),
                       when(fast(), skip), set_opts], multimod_compile_and_run,
     ['Main', '+RTS -M64m -RTS'])
