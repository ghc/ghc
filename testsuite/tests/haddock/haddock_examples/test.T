test('haddock.Test',
     [omit_ways(prof_ways),
      extra_clean(['Test.hi', 'Test.o', 'Hidden.hi', 'Hidden.o',
                   'Visible.hi', 'Visible.o'])],
     multimod_compile,
     ['Test Hidden Visible', '-XRankNTypes -XExistentialQuantification -haddock -ddump-parsed'])
