{-# LANGUAGE ExplicitStageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI08 where

import InstanceA ()
import splice ClassA
import ClassA
import splice Prelude (const)

e :: X
-- Uses a non-splice imported instance
e = $(const [| x vx |] (x vx))
