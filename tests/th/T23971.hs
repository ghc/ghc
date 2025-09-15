{-# LANGUAGE Haskell2010, MultiParamTypeClasses, TypeOperators, TemplateHaskell #-}

import Language.Haskell.TH (runQ)
import Language.Haskell.TH.Ppr (pprint)

main =
  runQ [d|class a ## b|]
  >>= putStrLn . pprint
