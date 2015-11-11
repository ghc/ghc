module T10819_Lib where

import Language.Haskell.TH.Syntax

doSomeTH s tp drv = return [NewtypeD [] n [] Nothing
                            (NormalC n [(NotStrict, ConT tp)]) drv]
  where n = mkName s
