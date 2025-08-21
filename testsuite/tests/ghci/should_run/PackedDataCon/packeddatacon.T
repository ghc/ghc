test('PackedDataCon',
     [ extra_files(['Obj.hs', 'ByteCode.hs', 'Types.hs', 'Common.hs-incl']),
       req_interp,
       req_bco,
       extra_ways(ghci_ways),
       only_ways(ghci_ways),
     ],
     compile_and_run,
     ['']
    )
