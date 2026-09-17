module Main where

import Data.Dynamic
import Data.Typeable
import TestUtils

data Thing = Thing {field1 :: Char, field2 :: Bool}
  deriving (Show, Eq)

castFromDynamic :: Dynamic -> Maybe Thing
castFromDynamic d = fromDynamic d
                 -- ^ this is the point

rep :: Thing -> TypeRep
rep d = typeOf d
      -- ^ this is the point

points =
  [ (11,21)
  , (15,10)
  ]

main = do
  (df, hf) <- readTestHie "HieTypeable.hie"
  let refmap = generateReferencesMap $ getAsts $ hie_asts hf

  traverse (explainEv df hf refmap) points
  return ()
