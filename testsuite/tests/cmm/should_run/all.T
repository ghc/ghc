test('HooplPostorder',
     extra_run_opts('"' + config.libdir + '"'),
     compile_and_run,
     ['-package ghc'])

test('cmp64',
     [    extra_run_opts('"' + config.libdir + '"')
     ,    req_cmm
     ],
     multi_compile_and_run,
     ['cmp64', [('cmp64_cmm.cmm', '')], '-O'])


# test('T17516',
#       [ collect_compiler_stats('bytes allocated', 5),
#       ],
#       multimod_compile,
#       ['T17516', '-O -v0'])

test('ByteSwitch',
     [    extra_run_opts('"' + config.libdir + '"')
     ,    req_cmm
     ],
     multi_compile_and_run,
     ['ByteSwitch', [('ByteSwitch_cmm.cmm', '')], ''])

test('T22871',
     [    extra_run_opts('"' + config.libdir + '"')
     ,    req_cmm
     ],
     multi_compile_and_run,
     ['T22871', [('T22871_cmm.cmm', '')], ''])

test('JumpTableNoStackDealloc',
     [    extra_run_opts('"' + config.libdir + '"')
     ,    req_cmm
     ],
     multi_compile_and_run,
     ['JumpTableNoStackDealloc', [('JumpTableNoStackDealloc_cmm.cmm', '')], ''])

test('AtomicFetch',
     [    extra_run_opts('"' + config.libdir + '"')
     ,    req_cmm
     ,    when(arch('i386'), skip) # https://gitlab.haskell.org/ghc/ghc/-/issues/23217
     ],
     multi_compile_and_run,
     ['AtomicFetch', [('AtomicFetch_cmm.cmm', '')], ''])
