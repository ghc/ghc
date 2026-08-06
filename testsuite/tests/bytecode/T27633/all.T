test('T27633a',
     [ extra_files(['Obj.hs']),
       req_interp,
       req_bco,
       only_ways(ghci_ways),
       extra_ways(ghci_ways),
     ],
     compile_and_run,
     ['']
    )

test('T27633b',
     [ extra_files(['Obj.hs']),
       req_interp,
       req_bco,
       only_ways(ghci_ways),
       extra_ways(ghci_ways),
     ],
     compile_and_run,
     ['']
    )
