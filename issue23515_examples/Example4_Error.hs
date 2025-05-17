/home/user/haskell/ghc3/ghc/issue23515_examples/Example4.hs:7:25: error: [GHC-25897]
    • Expected kind ‘k’, but ‘Bool’ has kind ‘*’
      ‘k’ is a rigid type variable bound by
        a family instance declaration
        at /home/user/haskell/ghc3/ghc/issue23515_examples/Example4.hs:7:15-17
    • In the type ‘Bool’
      In the type instance declaration for ‘Foo’
  |
7 | type instance Foo Int = Bool
  |                         ^^^^

