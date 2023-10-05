{-# LANGUAGE Haskell2010, TemplateHaskell, TypeFamilies, TypeOperators #-}

import Language.Haskell.TH (runQ)
import Language.Haskell.TH.Ppr (pprint)

main =
  runQ [d|data family (a + b) c d|]
  >>= putStrLn . pprint
