test('UnboxedTuples', [extra_files(['Obj.hs', 'ByteCode.hs', 'Common.hs-incl']),
     req_interp,
     when(config.have_ext_interp, extra_ways(['ghci-ext', 'ghci-ext-prof']))
     ], compile_and_run, [''])
