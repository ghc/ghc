{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI13 where

import Language.Haskell.TH
import quote Prelude

-- A quote import allows usage of id inside the quote

x :: Q Exp
x = [| id |]




