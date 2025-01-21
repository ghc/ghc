{-# LANGUAGE ExplicitStageImports #-}
{-# LANGUAGE TemplateHaskell #-}
module SI11 where

import Language.Haskell.TH

-- Is path-based CSP banned?
data X = X

x :: X -> Q Exp
x X = [| X |]




