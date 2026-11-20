{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
module NativeOpenInstalledMapProducer (mapper, example) where

{-# NOINLINE [0] mapper #-}
mapper :: (a -> b) -> [a] -> [b]
mapper _ [] = []
mapper f (x:xs) = f x : mapper f xs

#ifdef CHANGE_MAPPER
{-# RULES "drop mapper traversal" [~1] forall f xs. mapper f xs = [] #-}
#elif defined(COMPOSE_RULE)
{-# RULES "mapper/compose" [~1] forall f g xs.
    mapper f (mapper g xs) =
#ifdef FORCE_INTERMEDIATE
      mapper (\x -> let y = g x in y `seq` f y) xs
#else
      mapper (\x -> f (g x)) xs
#endif
  #-}
#elif defined(CONS_RULE)
{-# RULES "mapper/cons" [~1] forall f x xs.
    mapper f (x : xs) =
#if defined(FORCE_HEAD)
      x `seq` (f x : mapper f xs)
#elif defined(DROP_TAIL)
      f x : []
#else
      f x : mapper f xs
#endif
  #-}
#else
{-# RULES "mapper/id" [~1] forall xs. mapper (\x -> x) xs = xs #-}
#endif

example :: [Bool] -> [Bool]
example = mapper not
