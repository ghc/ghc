{-# LANGUAGE SpliceImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI09 where

import InstanceA ()
import splice ClassA

e :: X
-- Uses a non-splice imported instance
e = $(const [| x vx |] ())
