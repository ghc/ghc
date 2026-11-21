{-# LANGUAGE CPP, DataKinds, ExperimentalDependentTypes, GADTs, Modifiers, PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables, StandaloneKindSignatures, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeOpenListFoldrConcrete where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import qualified NativeOpenListFoldrProducer as F

#ifdef WRONG_CALLBACK
newtype WrappedBool = WrappedBool Bool

type FoldBits :: (WrappedBool %U -> Bool %U -> Bool) -> Bool -> [Bool] -> Bool
#else
type FoldBits :: (Bool %U -> Bool %U -> Bool) -> Bool -> [Bool] -> Bool
#endif
#if defined(FORCE_BIT_STEP)
type FoldBits k z xs = F.forceBitStep k z xs
#elif defined(FORCE_BIT_SEED)
type FoldBits k z xs = F.forceBitSeed k z xs
#else
type FoldBits k z xs = F.foldBits k z xs
#endif

type FoldChars :: (Char %U -> Maybe Bool %U -> Maybe Bool) -> Maybe Bool -> [Char] -> Maybe Bool
type FoldChars k z xs = F.foldChars k z xs

-- | An empty list returns the supplied Boolean seed.
bitsNil :: forall k z. Proxy k -> Proxy z -> FoldBits k z '[] :~: z
bitsNil _ _ = Refl

-- | The direct fold passes the original head and the fold of its tail.
bitsCons :: forall k z x xs. Proxy k -> Proxy z -> Proxy x -> Proxy xs
         -> FoldBits k z (x ': xs) :~: k x (FoldBits k z xs)
bitsCons _ _ _ _ = Refl

-- | The local worker preserves the same call at a different element type.
charsCons :: forall k z x xs. Proxy k -> Proxy z -> Proxy x -> Proxy xs
          -> FoldChars k z (x ': xs) :~: k x (FoldChars k z xs)
charsCons _ _ _ _ = Refl

-- | Both folds can skip their undefined seeds and tails.
showConcreteFold :: String
showConcreteFold =
    show ( bitsNil Proxy Proxy
         , F.foldBits (\x _ -> x) undefined (True : undefined)
         , F.foldChars (\_ _ -> Just True) undefined (undefined : undefined) )
