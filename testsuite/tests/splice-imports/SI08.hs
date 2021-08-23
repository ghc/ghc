{-# LANGUAGE SpliceImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI08 where

import InstanceA ()
import splice ClassA

e :: X
-- Uses a non-splice imported instance
e = $(const [| x vx |] (x vx))
