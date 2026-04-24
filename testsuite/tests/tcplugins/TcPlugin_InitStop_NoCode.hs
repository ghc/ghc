{-# OPTIONS_GHC -fplugin=TcPlugin_InitStop_Plugin #-}

module TcPlugin_InitStop_NoCode where

import TcPlugin_InitStop_NoCode_aux ( h )

f :: Bool -> Int
f True = 1

g :: Bool -> Int
g False = 2

k :: Bool -> Int
k True  = h False
k False = h True
