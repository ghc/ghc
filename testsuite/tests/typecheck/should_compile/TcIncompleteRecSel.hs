{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
module TcIncompleteRecSel where

import GHC.Records

data T where
    T1 :: { x :: Bool } -> T
    T2 :: T

f :: HasField "x" t Bool => t -> Bool
f = getField @"x"

g :: T -> Bool
g = f