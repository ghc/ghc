{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}

module UnliftedNewtypesLevityBinder where

import GHC.Types (RuntimeRep,TYPE,Coercible)

newtype Ident :: forall (r :: RuntimeRep). TYPE r -> TYPE r where
  IdentC :: forall (r :: RuntimeRep) (a :: TYPE r). a -> Ident a

bad :: forall (r :: RuntimeRep) (a :: TYPE r). a -> Ident a
bad = IdentC
