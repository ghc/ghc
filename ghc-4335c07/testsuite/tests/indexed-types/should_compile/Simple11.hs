{-# LANGUAGE TypeFamilies #-}

module Simple11 where

type family F a

same :: a -> a -> a
same = undefined

mkf :: a -> F a
mkf p = undefined

-- Works with explicit signature
-- foo :: a -> a -> (F a, a)
foo p q = same (mkf p, p) (mkf q, q)

