{-# LANGUAGE MagicHash #-}
module Main where

import Data.Dynamic
import Data.Typeable
import TestUtils
import GHC.Base

data Thing = Thing {field1 :: Char, field2 :: Bool}
  deriving (Show, Eq)

-- castFromDynamic :: Dynamic -> Maybe Thing
-- castFromDynamic d = fromDynamic d
--                  -- ^ this is the point

rep :: Thing -> TypeRep
rep d = typeOf d
      -- ^ this is the point

data Foo = Bar Int | Baz Double

myDataToTag :: Foo -> Int
myDataToTag f = I# (dataToTag# f)
                 -- ^ this is the point

points =
  [ (13,21)
  , (17,10)
  , (23,21)
  ]

main = do
  (df, hf) <- readTestHie "HieBuiltinInst.hie"
  let refmap = generateReferencesMap $ getAsts $ hie_asts hf

  traverse (explainEv df hf refmap) points
  return ()
