{-# LANGUAGE SpliceImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI09 where

import splice InstanceA ()
import splice ClassA

e :: IO ()
-- Uses a non-splice imported instance
e = $(const [| pure () |] (x vx))
