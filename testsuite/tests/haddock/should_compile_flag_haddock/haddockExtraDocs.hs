module HaddockExtraDocs where

data SomeField = SomeField

data T1 =
  MkT1
    -- | Comment on SomeField
    SomeField
    -- ^ Another comment on SomeField? (rejected)

data T2 =
  MkT2 {
    -- | Comment on SomeField
    someField :: SomeField
  } -- ^ Another comment on SomeField? (rejected)

data T3 =
  -- | Comment on MkT3
  MkT3
  -- ^ Another comment on MkT3? (rejected)

data T4 =
  -- | Comment on MkT4
  MkT4 {}
  -- ^ Another comment on MkT4? (rejected)
