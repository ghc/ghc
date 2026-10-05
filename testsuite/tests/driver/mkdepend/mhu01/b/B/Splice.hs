{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE TemplateHaskell #-}
module B.Splice where

import splice A.TH
import quote A.Base
-- Resolves to the 'Util' of the home unit 'b', not the one from 'a'
import Util
import Language.Haskell.TH (Q, Exp)

spliced :: Int
spliced = $(liftBase) + utilB

quoted :: Q Exp
quoted = [| base |]
