{-# LANGUAGE ScopedTypeVariables, ImpredicativeTypes, NoTypeAbstractions #-}
module T17594b where

id1 :: forall a. a -> a
id1 @t x = x

id2 :: forall a. Bool -> a -> a
id2 @t False x = x :: t
id2 True x = x

id3 :: a -> a
id3 @t x = x :: t

id4 :: forall a b c. a -> b -> c -> forall d e f. d -> e -> f -> (a, b, c, d, e, f)
id4 @t1 @t2 @t3 x1 x2 x3 @t4 @t5 @t6 x4 x5 x6 = (x1 :: t1, x2 :: t2, x3 :: t3, x4 :: t4, x5 :: t5, x6 :: t6)

id_r_N_t :: (forall a. a -> a) -> a -> a
id_r_N_t @t f = f @t

id5 = id_r_N_t (\ @t x -> x :: t)

id6 :: forall a. a -> a
id6 = \ @t x -> x :: t

id7 :: forall a b c d e f. Int
id7 @t1 @t2 = let _ = () in \ @t3 -> case () of () -> \ @t4 @t5 -> \ @t6 -> 42

id8 :: (forall a. a -> a, forall a. a -> a)
id8 = (\ @t x -> x, id1)

id9 :: [forall a. a -> a]
id9 = [\ @t x -> x, id1, id3, id5, id6, fst id8, snd id8]
