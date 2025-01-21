{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI09 where

import splice InstanceA ()
import splice ClassA
import splice Prelude

e :: IO ()
-- Using the instance only in a splice, and it's splice imported should work
e = $(const [| pure () |] (x vx))
