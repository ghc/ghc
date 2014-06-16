# Args to vtc are: extra compile flags

test('T3103',
     [only_compiler_types(['ghc']),
      normal,
      extra_clean(['Foreign/Ptr.hi', 'Foreign/Ptr.o',
                   'GHC/Base.hi',    'GHC/Base.o',
                   'GHC/Num.hi',     'GHC/Num.o',
                   'GHC/Show.hi',    'GHC/Show.o',
                   'GHC/Unicode.hi', 'GHC/Unicode.hi-boot',
                   'GHC/Unicode.o',  'GHC/Unicode.o-boot',
                   'GHC/Word.hi',    'GHC/Word.o'])],
     multimod_compile,
     ['Foreign.Ptr', '-v0 -hide-all-packages -package ghc-prim -package integer-gmp -package-name base'])

