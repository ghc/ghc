
{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE TemplateHaskell, UndecidableInstances #-}

module TH_spliceDecl4 where

import TH_spliceDecl4_Lib

instance IncrSelf String where
    incrSelf x = x ++ "x"

$(instanceIncrSelfTuple 2)


