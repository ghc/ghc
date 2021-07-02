{-# LANGUAGE TemplateHaskell #-}

module Bug where

m :: a -> a
m x = $$([||_||])
