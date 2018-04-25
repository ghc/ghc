{-# LANGUAGE ScopedTypeVariables #-}
module T12069 where

foo (_ :: p a) = [] :: [a]
