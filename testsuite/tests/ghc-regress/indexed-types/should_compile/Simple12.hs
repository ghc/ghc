{-# LANGUAGE TypeFamilies #-}

module Simple12 where

type family F a

same :: a -> a -> a
same = undefined

mkf :: a -> F a
mkf p = undefined

-- works with either of these signatures
-- foo :: a ~ F a => a -> a
-- foo :: a ~ F a => a -> F a
foo p = same p (mkf p)

