{-# LANGUAGE TemplateHaskell #-}
module QQ where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH

pq = QuasiQuoter { quoteDec = \_ -> return [sig],
                   quoteType = \_ -> undefined,
                   quoteExp = \_ -> undefined,
                   quotePat = \_ -> undefined }

sig = SigD (mkName "f") (ArrowT `AppT` int `AppT` int)
int = ConT (mkName "Int")
