{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module T18023 where

import Data.Kind
import Data.Proxy

newtype N :: Type -> Type -> Type where
  MkN :: forall b a. { unN :: Either a b } -> N a b

toN :: Either Int Bool -> N Int Bool
toN = MkN @Bool @Int

fromN :: N Int Bool -> Either Int Bool
fromN = unN @Bool @Int

newtype P a = MkP { unP :: Proxy a }

toPTrue :: Proxy True -> P True
toPTrue = MkP @True

fromPTrue :: P True -> Proxy True
fromPTrue = unP @True

newtype P2 a b = MkP2 { unP2 :: (Proxy a, Proxy b) }

toP2True :: (Proxy True, Proxy True) -> P2 True True
toP2True = MkP2 @True @True

fromP2True :: P2 True True -> (Proxy True, Proxy True)
fromP2True = unP2 @True @True

type    P3 :: forall {k}. k -> Type
newtype P3 a = MkP3 { unP3 :: Proxy a }

toP3True :: Proxy True -> P3 True
toP3True = MkP3 @True

fromP3True :: P3 True -> Proxy True
fromP3True = unP3 @True
