{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module T13233_orig where

import Control.Monad ( ap, liftM )

newtype FCode a = FCode (StgToCmmConfig -> CgState -> (# a, CgState #))

data StgToCmmConfig
  = StgToCmmConfig { }

data CgState = MkCgState

returnFC :: a -> FCode a
returnFC val = FCode (\_cfg state -> (# val, state #))

thenC :: FCode () -> FCode a -> FCode a
thenC (FCode m) (FCode k) =
  FCode $ \ cfg state ->
    case m cfg state of
      (# _, new_state #) -> k cfg new_state

thenFC :: FCode a -> (a -> FCode c) -> FCode c
thenFC (FCode m) k =
  FCode $ \ cfg state ->
    case m cfg state of
      (# m_result, new_state #) ->
        case k m_result of
          FCode kcode -> kcode cfg new_state

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

getStgToCmmConfig :: FCode StgToCmmConfig
getStgToCmmConfig = FCode $ \ cfg state -> (# cfg, state #)
