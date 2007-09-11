{-# LANGUAGE TypeFamilies #-}

module Simple13 where

type family F a

same :: a -> a -> a
same = undefined

mkf :: a -> [F a]
mkf p = undefined

foo :: a ~ [F a] => a -> a
foo p = same p (mkf p)

