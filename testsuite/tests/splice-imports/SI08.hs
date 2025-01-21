{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI08 where

-- Instance is only available at level 0
import InstanceA ()
import splice ClassA
import ClassA
import splice Prelude (const)

e :: X
-- Uses a non-splice imported instance
-- Used at levels 1 and 0
e = $(const [| x vx |] (x vx))
