{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
module Bug where

import Data.Kind
import Unsafe.Coerce

type family Sing :: k -> Type

class SingI a where
  sing :: Sing a

data SingInstance :: forall k. k -> Type where
  SingInstance :: SingI a => SingInstance a

newtype DI (a :: k) = Don'tInstantiate (SingI a => SingInstance a)

singInstance :: forall k (a :: k). Sing a -> SingInstance a
singInstance s = with_sing_i SingInstance
  where
    with_sing_i :: (SingI a => SingInstance a) -> SingInstance a
    with_sing_i si = unsafeCoerce (Don'tInstantiate si) s

{-# COMPLETE Sing #-}
pattern Sing :: forall k (a :: k). () => SingI a => Sing a
pattern Sing <- (singInstance -> SingInstance)
  where Sing = sing

-----

data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue  :: SBool True
type instance Sing @Bool = SBool

f :: SBool b -> ()
f Sing = ()

g :: Sing (b :: Bool) -> ()
g Sing = ()
