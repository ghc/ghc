{-# OPTIONS_GHC -fplugin=TcPlugin_InitStop_Plugin #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module TcPlugin_InitStop_Ghci where

f :: Bool -> Int
f True = 1

g :: Bool -> Int
g False = 2
