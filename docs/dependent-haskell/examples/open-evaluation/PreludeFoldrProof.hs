{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers #-}
{-# LANGUAGE PolyKinds, ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules -fno-ignore-interface-pragmas #-}
module PreludeFoldrProof (foldNil, foldCons, foldAppend, example) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import ListFoldrSource (HeadStep)
import PreludeMapLengthProof (Spine(..))
import Prelude (Bool(..))
import qualified Prelude as P

-- | The list instance returns the seed on an empty list.
foldNil :: forall a b (k :: a %U -> b %U -> b) (z :: b).
           Proxy k -> Proxy z -> P.foldr k z ('[] :: [a]) :~: z
foldNil _ _ = Refl

-- | The list instance passes the head and the fold of the tail to the combiner.
foldCons :: forall a b (k :: a %U -> b %U -> b) (z :: b) (x :: a) (xs :: [a]).
            Proxy k -> Proxy z -> Proxy x -> Proxy xs
         -> P.foldr k z (x ': xs) :~: k x (P.foldr k z xs)
foldCons _ _ _ _ = Refl

-- | Prove the append law using the list instance selected by the operands.
foldAppend :: forall a b (k :: a %U -> b %U -> b) (z :: b) (xs :: [a]) (ys :: [a]).
              Proxy k -> Proxy z -> Spine xs -> Proxy ys
           -> P.foldr k z (xs P.++ ys) :~: P.foldr k (P.foldr k z ys) xs
foldAppend _ _ Empty _ = Refl
foldAppend k z (Cons xs) ys = case foldAppend k z xs ys of
    Refl -> Refl

-- | Apply the law to a one-element left list. Both folds return its head.
example :: 'True :~: 'True
example = foldAppend (Proxy @HeadStep) (Proxy @'False)
                     (Cons Empty :: Spine '[ 'True]) (Proxy @'[ 'False])
