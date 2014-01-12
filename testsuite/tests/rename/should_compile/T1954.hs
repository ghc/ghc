{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Bug(P) where

import Control.Applicative (Applicative)

newtype P a = P (IO a) deriving (Functor, Applicative, Monad)

