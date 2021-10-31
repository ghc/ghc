{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module T20590 where

$([d| data T where
        MkT :: forall a. a -> T
    |])
