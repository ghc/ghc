test('UnliftedDataTypeInterp',
     [ extra_files(['Obj.hs', 'ByteCode.hs', 'Types.hs', 'Common.hs-incl']),
       req_interp,
       req_bco,
       extra_ways(['ghci']),
       when(config.have_ext_interp, extra_ways(['ghci', 'ghci-ext'])),
       when(config.have_ext_interp and config.have_profiling, extra_ways(['ghci', 'ghci-ext', 'ghci-ext-prof']))
     ],
     compile_and_run,
     ['']
    )
