setTestOpts(when(compiler_profiled(), skip))

# Annotations and Template Haskell, require runtime evaluation.  In
# order for this to work with profiling, we would have to build the
# program twice and use -osuf p_o (see the TH_splitE5_prof test).  For
# now, just disable the profiling ways.

test('annth_make',
     [req_interp, omit_ways(['profasm','profthreaded']),
      clean_cmd('$MAKE -s clean_annth_make')],
     run_command,
     ['$MAKE -s --no-print-directory annth_make'])

test('annth_compunits',
     [req_interp, omit_ways(['profasm','profthreaded']),
      clean_cmd('$MAKE -s clean_annth_compunits')],
     run_command,
     ['$MAKE -s --no-print-directory annth_compunits'])
