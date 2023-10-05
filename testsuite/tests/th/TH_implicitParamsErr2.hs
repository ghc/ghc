{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH

main = $(letE [ implicitParamBindD "x" [e| 1 |]
              , funD (mkName "y") [clause [] (normalB [e| 2 |]) []]
              ]
              (varE (mkName "y")))
