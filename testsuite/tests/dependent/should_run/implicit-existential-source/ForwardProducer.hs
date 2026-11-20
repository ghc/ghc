module ForwardProducer (failAny, choose, keepAfter) where

{-# NOINLINE failAny #-}
failAny :: String -> a
failAny msg = error msg

{-# NOINLINE choose #-}
choose :: Bool -> a -> a -> a
choose b x y = if b then x else y

{-# NOINLINE keepAfter #-}
keepAfter :: b -> a -> a
keepAfter ignored value = ignored `seq` value
