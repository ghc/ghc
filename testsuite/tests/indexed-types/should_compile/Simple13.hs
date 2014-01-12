{-# LANGUAGE TypeFamilies #-}

-- This should fail, I think, because of the loopy equality,
-- but the error message is hopeless

module Simple13 where

type family F a

same :: a -> a -> a
same = undefined

mkf :: a -> [F a]
mkf p = undefined

foo :: a ~ [F a] => a -> a
foo p = same p (mkf p)

