# These are JavaScript-specific tests for the JS backend optimizer
setTestOpts(when(not(js_arch()),skip))

test('deadCodeElim', normal, compile_and_run, ['-package ghc'])
test('jsOptimizer', normal, compile_and_run, ['-package ghc'])
