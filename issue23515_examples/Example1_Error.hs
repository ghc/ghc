/home/user/haskell/ghc3/ghc/issue23515_examples/Example1.hs:7:23: error: [GHC-25897]
    • Expected kind ‘k’, but ‘Char’ has kind ‘*’
      ‘k’ is a rigid type variable bound by
        a family instance declaration
        at /home/user/haskell/ghc3/ghc/issue23515_examples/Example1.hs:7:15
    • In the type ‘Char’
      In the type instance declaration for ‘F’
  |
7 | type instance F Int = Char
  |                       ^^^^

/home/user/haskell/ghc3/ghc/issue23515_examples/Example1.hs:8:23: error: [GHC-25897]
    • Expecting one more argument to ‘Maybe’
      Expected kind ‘k’, but ‘Maybe’ has kind ‘* -> *’
      ‘k’ is a rigid type variable bound by
        a family instance declaration
        at /home/user/haskell/ghc3/ghc/issue23515_examples/Example1.hs:8:15
    • In the type ‘Maybe’
      In the type instance declaration for ‘F’
  |
8 | type instance F Int = Maybe
  |                       ^^^^^

