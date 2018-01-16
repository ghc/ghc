{-# LANGUAGE ScopedTypeVariables #-}
module T7827 where

bug :: a -> ()
bug ((_ :: a) :: a) = ()
