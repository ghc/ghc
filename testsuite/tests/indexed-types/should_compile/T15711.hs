{-# LANGUAGE TypeFamilies, DataKinds #-}

module Bug where

class C a where
  type F (x :: Maybe a)
