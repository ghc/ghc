print(ghci_ways)

test('UnboxedTuples',
     [ extra_files(['Obj.hs', 'ByteCode.hs', 'Common.hs-incl']),
       req_interp,
       req_bco,
       only_ways(ghci_ways),
       extra_ways(ghci_ways),
     ],
     compile_and_run,
     ['']
    )
