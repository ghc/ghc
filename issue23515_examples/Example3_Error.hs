/home/user/haskell/ghc3/ghc/issue23515_examples/Example3.hs:7:15: error: [GHC-25897]
    • Expected a type, but ‘a’ has kind ‘k1’
      ‘k1’ is a rigid type variable bound by
        a family instance declaration
        at /home/user/haskell/ghc3/ghc/issue23515_examples/Example3.hs:7:3-16
    • In the type ‘[a]’
      In the type family declaration for ‘N'’
  |
7 |   N' (t a) = [a]
  |               ^

