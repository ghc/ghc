{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Array.Byte
import Language.Haskell.TH.Syntax


main = print $(
    let liftBA :: ByteArray -> Q Exp
        liftBA = lift
    in liftBA [0..128])
