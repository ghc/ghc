{-# LANGUAGE ScopedTypeVariables, ImpredicativeTypes, TemplateHaskell, LambdaCase, TypeAbstractions, BlockArguments #-}
module T17594a where

import qualified Language.Haskell.TH as TH

id1 :: forall a. a -> a
id1 @t x = x

id2 :: forall a. Bool -> a -> a
id2 @t False x = x :: t
id2 True x = x

id3 :: a -> a
id3 @t x = x :: t

id4 :: forall a b c. a -> b -> c -> forall d e f. d -> e -> f -> (a, b, c, d, e, f)
id4 @t1 @t2 @t3 x1 x2 x3 @t4 @t5 @t6 x4 x5 x6 = (x1 :: t1, x2 :: t2, x3 :: t3, x4 :: t4, x5 :: t5, x6 :: t6)

id5 :: (forall a. a -> a, forall a. a -> a)
id5 = (\ @t x -> x, id1)

id6 :: [forall a. a -> a]
id6 = [\ @t x -> x, id1, id3, fst id5, snd id5, id8]

id7 :: a -> a
id7 @($(TH.varT (TH.mkName "t"))) x = x :: t

id_r_N_t :: (forall a. Bool -> a -> a) -> a -> a
id_r_N_t @t f = f @t False

id8 = id_r_N_t (\ @t _ x -> x :: t)

id9 :: forall a. a -> a
id9 = id_r_N_t \cases
  @t True x -> x :: t
  False x -> x
