{-# LANGUAGE TypeApplications, PatternSynonyms, GADTs, ViewPatterns, TypeAbstractions #-}

module T22328 where

import Data.Typeable

data Gadt x y where
  ExistentialInGadt :: Typeable a => a -> Gadt x x

pattern CastGadt :: Typeable a => x ~ y => a -> Gadt x y
pattern CastGadt a <- ExistentialInGadt (cast -> Just a)

test :: Gadt i o -> Bool
test gadt = case gadt of
  CastGadt @Bool a -> a
  _ -> False
