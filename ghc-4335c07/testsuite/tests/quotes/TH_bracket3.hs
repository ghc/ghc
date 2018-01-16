{-# LANGUAGE MultiParamTypeClasses #-}

module TH_bracket3 where

d_class = [d| class Classy a b where
                 f :: a -> b

              instance Classy Int Bool where
                 f x = if x == 0 then True else False
           |]
