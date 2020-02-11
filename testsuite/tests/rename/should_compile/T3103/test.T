# Args to vtc are: extra compile flags

test('T3103', [extra_files(['Foreign/', 'GHC/'])], multimod_compile,
     ['Foreign.Ptr',
      '-v0 -hide-all-packages -package ghc-prim -package ghc-bignum -this-unit-id base'])
