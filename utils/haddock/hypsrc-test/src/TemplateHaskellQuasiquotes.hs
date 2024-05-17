{-# LANGUAGE Haskell2010 #-}
{-# LANGUAGE TemplateHaskell #-}

module TemplateHaskellQuasiquotes where

import Language.Haskell.TH

aDecl :: DecsQ
aDecl = [d|
    bar :: $aType -> [ (Int, String) ]
    bar $aPattern = $anExpression
  |]

aPattern :: PatQ
aPattern = [p|
    [ aCrazyLongVariableName
    , _unused
    , (y, z)
    , ( $aNumberPattern, "hello")
    ]
  |]

aNumberPattern :: PatQ
aNumberPattern = [p|
    w@v@4.5
  |]

anExpression, anExpression2 :: ExpQ
anExpression = [e|
    [ (1 + $anExpression2, "world") ]
  |]
anExpression2 = [| (1 + round pi) |]

aType :: TypeQ
aType = [t|
    [ (Double, String) ]
  |]

typedExpr1 :: Code Q ()
typedExpr1 = [|| () ||]

typedExpr :: Code Q ()
typedExpr = [|| const $$(typedExpr1) () ||]



