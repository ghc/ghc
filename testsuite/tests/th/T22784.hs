{-# LANGUAGE TemplateHaskell #-}
module T22784 where

import Data.Kind

$([d| f :: (Bool :: Type)
      f = True |])
