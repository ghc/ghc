{-# LANGUAGE Haskell2010 #-}
module Bug873 (($), ($$)) where
infixr 0 $$

($$) :: (a -> b) -> a -> b
f $$ x = f x
