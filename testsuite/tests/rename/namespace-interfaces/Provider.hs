{-# LANGUAGE ExplicitNamespaces #-}
module Provider (type Key, data Key) where

data Key = Key

{-# ANN type Key ("type" :: String) #-}
{-# ANN Key ("constructor" :: String) #-}
