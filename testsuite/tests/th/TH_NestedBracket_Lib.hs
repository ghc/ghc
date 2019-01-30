{-# LANGUAGE TemplateHaskell #-}
module TH_NestedBracket_Lib where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

-- Test the normal lifting
foo :: Lift a => a -> Q Exp
foo x = [| x |]

-- Test living two levels
foo2 :: Lift a => a -> Q Exp
foo2 x = [| [| x |] |]

-- Test lifting with additional syntax
foo3 :: Bool -> Q Exp
foo3 x = [| [| not x |] |]

-- Test lifting from level 1 to level 2
foo4 :: Q Exp
foo4 = [| \x -> [| x |] |]

-- Test lifting three levels
foo5 :: Lift a => a -> Q Exp
foo5 x = [| [| [| x |] |] |]

normalLift :: Bool -> Q Exp
normalLift = lift

liftTwice :: Bool -> Q Exp
liftTwice b = lift b >>= \b' -> ((varE 'return) `appE` (lift b'))

--foo3 :: Q Exp
--foo3 = [| \x -> [| x |] |]
