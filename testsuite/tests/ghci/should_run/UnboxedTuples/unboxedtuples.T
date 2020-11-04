test('UnboxedTuples', [extra_files(['Obj.hs', 'Common.hs-incl']),
     only_ways(['ghci'])], compile_and_run, [''])
