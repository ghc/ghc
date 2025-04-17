{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExplicitLevelImports #-}
module SI25 where

-- Import specifically splice
import splice SI25Helper
import Language.Haskell.TH

-- Test simple explicit level import with a splice
test1 :: Q Exp
test1 = $(nestedCode "hello")

-- Test nested splices with explicit level imports
-- This should fail, as nestedCode is only available at level -1
test2 :: String
test2 = $($(nestedCode "nested"))
