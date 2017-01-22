test('haddock.Test', [omit_ways(prof_ways)], multimod_compile,
     ['Test Hidden Visible',
      '-XRankNTypes -XExistentialQuantification -haddock -ddump-parsed'])
