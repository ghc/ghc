test('haddock.Test', [extra_files(['Hidden.hs', 'Test.hs', 'Visible.hs']),
                      omit_ways(prof_ways)], multimod_compile,
     ['Test Hidden Visible',
      '-XRankNTypes -XExistentialQuantification -haddock -ddump-parsed'])
