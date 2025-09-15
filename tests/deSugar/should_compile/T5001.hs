{-# LANGUAGE MultiParamTypeClasses #-}

module T5001 where

import T5001a

data T = T

instance Comorphism Float Int Char T

