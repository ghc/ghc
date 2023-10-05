{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoPolyKinds #-}

module T20584 where

data Decision_Wrap
data Decision_Map

type family DecideFn p where
  DecideFn (r -> p) = Decision_Map
  DecideFn p = Decision_Wrap
