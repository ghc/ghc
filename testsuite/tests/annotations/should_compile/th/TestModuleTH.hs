{-# LANGUAGE TemplateHaskell #-}

module TestModuleTH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addTopDecls)

$(do
     modAnn <- pragAnnD ModuleAnnotation
                        (stringE "TH module annotation")
     modAnn' <- pragAnnD ModuleAnnotation
                         (stringE "addTopDecls module annotation")
     [typ] <- [d| data TestTypeTH = TestTypeTH |]
     conAnn <- pragAnnD (ValueAnnotation $ mkName "TestTypeTH")
                        (stringE "TH Constructor annotation")
     conAnn' <- pragAnnD (ValueAnnotation $ mkName "TestTypeTH")
                         (stringE "addTopDecls Constructor annotation")
     typAnn <- pragAnnD (TypeAnnotation $ mkName "TestTypeTH")
                        (stringE "TH Type annotation")
     typAnn' <- pragAnnD (TypeAnnotation $ mkName "TestTypeTH")
                         (stringE "addTopDecls Type annotation")
     valAnn <- pragAnnD (ValueAnnotation $ mkName "testValueTH")
                        (stringE "TH Value annotation")
     valAnn' <- pragAnnD (ValueAnnotation $ mkName "testValueTH")
                         (stringE "addTopDecls value annotation")
     [val] <- [d| testValueTH = (42 :: Int) |]
     addTopDecls [modAnn', conAnn', typAnn', valAnn']
     return [modAnn, conAnn, typAnn, typ, valAnn, val] )
