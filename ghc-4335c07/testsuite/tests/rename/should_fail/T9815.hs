{-# LANGUAGE RecordWildCards #-}
module T9815 where

newtype N = N Int deriving (Show)

foo = print N{..}
