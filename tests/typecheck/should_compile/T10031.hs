{-# LANGUAGE ScopedTypeVariables #-}
module T10031 where
import Data.Coerce
coerce' :: Coercible b a => a -> b
coerce' = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b
