{-# LANGUAGE Rank2Types #-}

-- Another test for the new feature that contexts in a mutually
-- recursive group do not need to be of the same length
-- I'm putting it here mainly to document the need

-- GHC used to say:
-- 
-- Bug.lhs:23:10:
--     Inferred type is less polymorphic than expected
--      Quantified type variable `c' is mentioned in the environment:
--        printCatalog :: c -> IO () (bound at Bug.lhs:28:0)
--    In the `viewCatalog' field of a record

-- Bug.lhs:27:0:
--    Contexts differ in length
--    When matching the contexts of the signatures for
--      printer :: Viewer
--      printCatalog :: forall c. (Catalog c) => View c
--    The signature contexts in a mutually recursive group should all be identical

module Main where

class Catalog c where
  traverse :: c -> Viewer -> IO ()

instance Catalog Int where
  traverse i v = viewShowable v i

type View a = a -> IO ()

data Viewer = Viewer {
    viewShowable :: forall s. Show s => View s,
    viewCatalog :: forall c. Catalog c => View c
  }

printer :: Viewer
--printer = Viewer {
--  viewCatalog = \x -> traverse x printer,
--  viewShowable = putStrLn . show }
printer = Viewer {
  viewCatalog = printCatalog,
  viewShowable = putStrLn . show }

printCatalog :: forall c. Catalog c => View c
printCatalog x = traverse x printer

data X = X {
    cat :: Int
  }

instance Catalog X where
  traverse x v = do
    viewCatalog v (cat x)

main = do
  let x = X { cat = 20 }
  traverse x printer
