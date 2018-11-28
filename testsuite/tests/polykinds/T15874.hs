{-# Language RankNTypes   #-}
{-# Language DataKinds    #-}
{-# Language PolyKinds    #-}
{-# Language GADTs        #-}
{-# Language TypeFamilies #-}

module T15874 where

import Data.Kind

data Var where
  Op :: Var
  Id :: Var

type Varianced = (forall (var :: Var). Type)

data family   Parser :: Varianced
data instance Parser = P
