{-# LANGUAGE TemplateHaskell #-}

module TestModuleTH where

import Language.Haskell.TH

$(do
     modAnn <- pragAnnD ModuleAnnotation
                        (stringE "TH module annotation")
     [typ] <- [d| data TestTypeTH = TestTypeTH |]
     conAnn <- pragAnnD (ValueAnnotation $ mkName "TestTypeTH")
                        (stringE "TH Constructor annotation")
     typAnn <- pragAnnD (TypeAnnotation $ mkName "TestTypeTH")
                        (stringE "TH Type annotation")
     valAnn <- pragAnnD (ValueAnnotation $ mkName "testValueTH")
                        (stringE "TH Value annotation")
     [val] <- [d| testValueTH = (42 :: Int) |]
     return [modAnn, conAnn, typAnn, typ, valAnn, val] )
