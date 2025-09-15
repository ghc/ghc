{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI09 where

import InstanceA ()
import splice ClassA
import ClassA
import splice Prelude

e :: X
-- Using the instance only at the normal level should work
e = $(const [| x vx |] ())
