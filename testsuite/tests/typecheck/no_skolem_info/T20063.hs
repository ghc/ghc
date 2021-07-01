{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}

module Bug where

data Context where
  Extend :: forall k. Context -> Context

type (:*&) :: Context -> forall k -> Context
type ctx :*& k = Extend @k ctx

data Idx ctx where
  T :: Idx ctx -> Idx (ctx :*& l)

data Rn ctx1 ctx2 where
  U :: Rn ctx1 ctx2 -> Rn (ctx1 :*& l) (ctx2 :*& l)

rnRename :: Rn ctx1 ctx2 -> Idx ctx3 -> Idx ctx4
rnRename (U _ ) _ = T _
rnRename _      T = undefined
