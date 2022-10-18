# These tests are very slow due to their use of package GHC
setTestOpts(when(fast(), skip))

# Annotations, like Template Haskell, require runtime evaluation.  In
# order for this to work with profiling, we would have to build the
# program twice and use -osuf p_o (see the TH_spliceE5_prof test).  For
# now, just disable the profiling and dynamic ways, and use
# config.ghc_th_way_flags.
test('annrun01',
     [extra_files(['Annrun01_Help.hs']),
      pre_cmd('$MAKE -s --no-print-directory config'),
      omit_ways(['dyn'] + prof_ways),
      req_interp],
     multimod_compile_and_run,
     ['annrun01', '-package ghc ' + config.ghc_th_way_flags])

