module T5665a where

import Language.Haskell.TH

doSomeTH s tp = return [NewtypeD [] n [] Nothing
                        (NormalC n [(NotStrict, ConT tp)]) []]
   where n = mkName s
