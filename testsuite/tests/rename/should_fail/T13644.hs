{-# LANGUAGE GHC2021 #-}
module T13644 where

import T13644A

baseENDECfuncs :: FuncId -> ()
baseENDECfuncs FuncId{ name = nm } = undefined
