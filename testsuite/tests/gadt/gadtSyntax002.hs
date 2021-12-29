{-# LANGUAGE GADTSyntax #-}

module GADTSyntax002 where

newtype Down a where
  Down :: { getDown :: a } -> Down a
