{-# LANGUAGE ExplicitStageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI09 where

import InstanceA ()
import splice ClassA
import ClassA
import splice Prelude

e :: X
-- Uses a non-splice imported instance
e = $(const [| x vx |] ())
