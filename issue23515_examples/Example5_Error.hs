/home/user/haskell/ghc3/ghc/issue23515_examples/Example5.hs:7:22: error: [GHC-25897]
    • Couldn't match kind ‘t1’ with ‘GHC.Types.LiftedRep’
      Expected kind ‘*’, but ‘a’ has kind ‘TYPE t1’
      ‘t1’ is a rigid type variable bound by
        a family instance declaration
        at /home/user/haskell/ghc3/ghc/issue23515_examples/Example5.hs:7:3-22
    • In the first argument of ‘Maybe’, namely ‘a’
      In the type ‘Maybe a’
      In the type family declaration for ‘F’
  |
7 |   F (a -> _) = Maybe a
  |                      ^

