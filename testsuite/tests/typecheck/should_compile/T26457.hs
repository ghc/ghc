{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module T26457 where

import Data.Kind
import Data.Proxy

type family FC (be :: Type) (entity :: Type) :: Constraint

class Database be where
    fun         :: Proxy be -> (forall tbl. FC be tbl => Proxy tbl -> ()) -> ()
    default fun :: Proxy be -> (forall tbl. FC be tbl => Proxy tbl -> ()) -> ()
    fun _ _ = undefined
