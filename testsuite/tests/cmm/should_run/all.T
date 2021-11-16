test('HooplPostorder',
     extra_run_opts('"' + config.libdir + '"'),
     compile_and_run,
     ['-package ghc'])

test('cmp64',
     [    extra_run_opts('"' + config.libdir + '"')
     ,    omit_ways(['ghci'])
     ,    javascript_failures
     ,    extra_clean('cmp64_cmm.o')
     ],
     multi_compile_and_run,
     ['cmp64', [('cmp64_cmm.cmm', '')], '-O'])


# test('T17516',
#       [ collect_compiler_stats('bytes allocated', 5),
#         extra_clean(['T17516A.hi', 'T17516A.o'])
#       ],
#       multimod_compile,
#       ['T17516', '-O -v0'])