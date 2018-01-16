{-# LANGUAGE TypeInType #-}

module KindLevels where

import Data.Kind

data A
data B :: A -> *
data C :: B a -> *
data D :: C b -> *
data E :: D c -> *
