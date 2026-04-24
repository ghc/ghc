{-# OPTIONS_GHC -fplugin=TcPlugin_InitStop_Plugin #-}

module TcPlugin_InitStop_NoCode_aux where

import {-# SOURCE #-} TcPlugin_InitStop_NoCode ( f, g )

h x = f x + g x
