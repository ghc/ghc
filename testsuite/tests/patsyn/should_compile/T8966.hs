{-# LANGUAGE PolyKinds, KindSignatures, PatternSynonyms, DataKinds, GADTs  #-}

module T8966 where

data NQ :: [k] -> * where
   D :: NQ '[a]

pattern Q = D
