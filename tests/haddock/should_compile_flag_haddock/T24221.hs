module T24221 where

data Foo0
  -- | Docs for Foo1
  = Foo1 Int
  -- | Docs for Foo2
  | Foo2 Int
  -- | Docs for infix constructor
  | Int :* Bool

data Foo3 =
  Int :+ Bool -- ^ Docs for infix constructor

data Foo4 =
  Int    -- ^ Docs for Int
  :%     -- ^ Docs for infix constructor
  Bool   -- ^ Docs for Bool

data Foo5 =
  -- | Docs for Int
  Int
  -- | Docs for infix constructor
  :@
  -- | Docs for Bool
  Bool

data Foo6 =
  MkFoo6 { a6 :: Int   -- ^ Docs for a6
         , b6 :: Int } -- ^ Docs for b6

data Foo7 =
  MkFoo7 -- ^ Docs for MkFoo7
    { a7 :: Int    -- ^ Docs for a7
    , b7 :: Int }  -- ^ Docs for b7

data Foo8 =
  -- | Docs for MkFoo8
  MkFoo8 {
    -- | Docs for a8
    a8 :: Int,
    -- | Docs for b8
    b8 :: Int
  }