{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Bug(P) where

newtype P a = P (IO a) deriving (Functor, Applicative, Monad)

