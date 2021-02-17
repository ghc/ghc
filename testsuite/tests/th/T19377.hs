{-# LANGUAGE TemplateHaskell #-}
module T19377 where

$([d| x :: Int
      x = 42
      {-# ANN x "blah" #-}

      data Y
      {-# ANN type Y "yargh" #-}
    |])
