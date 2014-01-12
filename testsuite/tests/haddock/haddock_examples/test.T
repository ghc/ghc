test('haddock.Test',
     [omit_ways(['optasm', 'profasm']),
      extra_clean(['Test.hi', 'Test.o', 'Hidden.hi', 'Hidden.o',
                   'Visible.hi', 'Visible.o'])],
     multimod_compile,
     ['Test Hidden Visible', '-XRankNTypes -XExistentialQuantification -haddock -ddump-parsed'])
