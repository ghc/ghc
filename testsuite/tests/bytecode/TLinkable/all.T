# Test ideas
# Bytecode libraries
# Depend on that bytecode, look at the bytecode library tests to make sure this ends up in the EPS

def normaliseDynlibNames(str):
    return re.sub(r'-ghc[0-9.]+\.', '-ghc<VERSION>.', str)

test('TLinkable',
     [ collect_compiler_stats('bytes allocated',2),
       pre_cmd('$MAKE -s --no-print-directory TLinkable_Prep'),
       extra_files(['genSplices']),
       compile_timeout_multiplier(5),
     ],
     compile,
     ['-fprefer-byte-code -fforce-recomp ' + config.ghc_th_way_flags])

test('TLinkable2',
     [ collect_compiler_stats('bytes allocated',2),
       pre_cmd('$MAKE -s --no-print-directory TLinkable2_Prep'),
       extra_files(['genSplices']),
       compile_timeout_multiplier(5),
     ],
     compile,
     ['-fprefer-byte-code -fforce-recomp ' + config.ghc_th_way_flags])

test('linkable_bytecodelib',
      [ extra_files(["genSplices2"])
      , when(have_profiling(), extra_ways(['prof']))
      , normalise_errmsg_fun(normaliseDynlibNames)
      , pre_cmd('$MAKE -s --no-print-directory linkable_bytecodelib_Prep')
      , copy_files
      , req_bco ],
      makefile_test,
      [])
