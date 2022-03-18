test('HooplPostorder',
     extra_run_opts('"' + config.libdir + '"'),
     compile_and_run,
     ['-package ghc'])

test('cmp64',
     [    extra_run_opts('"' + config.libdir + '"')
     ,    omit_ways(['ghci'])
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
     ,    omit_ways(['ghci'])
     ],
     multi_compile_and_run,
     ['ByteSwitch', [('ByteSwitch_cmm.cmm', '')], ''])
