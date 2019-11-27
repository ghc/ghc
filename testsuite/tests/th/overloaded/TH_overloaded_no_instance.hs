{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TemplateHaskell #-}
module TH_overloaded_constraints_no_instance where
-- Test the error message when there is no instance

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

data NewType a

-- No instance for Quote NewType
quote2 :: NewType Exp
quote2 = [| 5 |]

