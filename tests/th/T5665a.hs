module T5665a where

import Language.Haskell.TH

doSomeTH s tp = return [NewtypeD [] n [] Nothing (NormalC n
        [(Bang NoSourceUnpackedness NoSourceStrictness, ConT tp)]) []]
   where n = mkName s
