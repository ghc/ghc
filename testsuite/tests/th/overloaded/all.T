# NOTICE TO DEVELOPERS
# ~~~~~~~~~~~~~~~~~~~~
# Adding a TemplateHaskell test?  If it only contains (non-quasi) quotes
# and no splices, consider adding it to the quotes/ directory instead
# of the th/ directory; this way, we can test it on the stage 1 compiler too!

def f(name, opts):
    opts.extra_hc_opts = '-XTemplateHaskell -package template-haskell'
setTestOpts(f)
setTestOpts(req_interp)
# TH should work with -fexternal-interpreter too
if config.have_ext_interp :
     setTestOpts(extra_ways(['ext-interp']))
     setTestOpts(only_ways(['normal','ghci','ext-interp']))

     if llvm_build():
         setTestOpts(fragile_for(16087, ['ext-interp']))

test('TH_overloaded_extract', normal, compile_and_run, [''])
test('TH_overloaded_constraints', normal, compile, ['-v0'])
test('TH_overloaded_constraints_fail', normal, compile_fail, ['-v0'])
test('TH_overloaded_no_instance', normal, compile_fail, ['-v0'])
test('TH_overloaded_csp', normal, compile_and_run, ['-v0'])
test('T17839', normal, compile, ['-v0 -package mtl -package containers'])
