{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module TH_overloaded_constraints_fail where
-- Test the error message when there are conflicting nested splices

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Functor.Identity

instance Quote Identity where
  -- Not the correct implementation, just for testing
  newName s = Identity (Name (mkOccName s) NameS)

idQ :: Identity Exp
idQ = [| 5 |]

qq :: Q Exp
qq = [| 5 |]

quote = [| $(idQ) $(qq) |]
