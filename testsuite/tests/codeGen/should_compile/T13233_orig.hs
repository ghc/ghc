{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module T13233_orig where

import Control.Monad ( ap, liftM )

newtype FCode a = FCode (CgInfoDownwards -> CgState -> (# a, CgState #))

data CgInfoDownwards
  = MkCgInfoDown { cgd_dflags :: DynFlags }

data CgState = MkCgState

returnFC :: a -> FCode a
returnFC val = FCode (\_info_down state -> (# val, state #))

thenC :: FCode () -> FCode a -> FCode a
thenC (FCode m) (FCode k) =
  FCode $ \ info_down state ->
    case m info_down state of
      (# _, new_state #) -> k info_down new_state

thenFC :: FCode a -> (a -> FCode c) -> FCode c
thenFC (FCode m) k =
  FCode $ \ info_down state ->
    case m info_down state of
      (# m_result, new_state #) ->
        case k m_result of
          FCode kcode -> kcode info_down new_state

infixr 9 `thenC`
infixr 9 `thenFC`
{-# INLINE thenC #-}
{-# INLINE thenFC #-}
{-# INLINE returnFC #-}

instance Functor FCode where
  fmap f (FCode g) =
    FCode $ \ i s -> case g i s of (# a, s' #) -> (# f a, s' #)

instance Applicative FCode where
  pure = returnFC
  (<*>) = ap

instance Monad FCode where
  (>>=) = thenFC

instance HasDynFlags FCode where
  getDynFlags = liftM cgd_dflags getInfoDown

getInfoDown :: FCode CgInfoDownwards
getInfoDown = FCode $ \ info_down state -> (# info_down, state #)

class HasDynFlags m where
  getDynFlags :: m DynFlags

data DynFlags = DynFlags
