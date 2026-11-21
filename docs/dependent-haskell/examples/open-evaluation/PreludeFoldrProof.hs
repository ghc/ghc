{-# LANGUAGE DataKinds, ExperimentalDependentTypes, GADTs, Modifiers #-}
{-# LANGUAGE PolyKinds, ScopedTypeVariables, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module PreludeFoldrProof (foldNil, foldCons, foldAppend, foldMap, example, showFoldMap) where

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

-- | Fold a mapped list by composing the step with the installed operator.
foldMap :: forall a b c (k :: b %U -> c %U -> c) (f :: a %U -> b) (z :: c) (xs :: [a]).
           Proxy k -> Proxy f -> Proxy z -> Spine xs
        -> P.foldr k z (P.map f xs) :~: P.foldr (k P.. f) z xs
foldMap _ _ _ Empty = Refl
foldMap k f z (Cons xs) = case foldMap k f z xs of
    Refl -> Refl

-- | Apply the law to a one-element left list. Both folds return its head.
example :: 'True :~: 'True
example = foldAppend (Proxy @HeadStep) (Proxy @'False)
                     (Cons Empty :: Spine '[ 'True]) (Proxy @'[ 'False])

-- | Fold the result of a separate traversal.
{-# NOINLINE foldMapped #-}
foldMapped :: (b -> c -> c) -> (a -> b) -> c -> [a] -> c
foldMapped k f z xs = P.foldr k z (P.map f xs)

-- | Fold the input with a composed step.
{-# NOINLINE foldComposed #-}
foldComposed :: (b -> c -> c) -> (a -> b) -> c -> [a] -> c
foldComposed k f z xs = P.foldr (k P.. f) z xs

-- | Observe the proof and the first result from both folds.
showFoldMap :: P.String
showFoldMap =
    P.show ( foldMap Proxy Proxy Proxy (Cons Empty :: Spine '[ 'False])
           , run (\x _ -> x) P.not False
           , run (\_ _ -> True) (P.error "forced mapper") (P.error "forced head") )
  where
    run k f x =
        let z = P.error "forced seed"
            xs = [x] P.++ P.error "forced tail"
        in (foldMapped k f z xs, foldComposed k f z xs)
