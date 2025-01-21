{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI13 where

import Language.Haskell.TH
import quote Prelude

x :: Q Exp
x = [| id |]




