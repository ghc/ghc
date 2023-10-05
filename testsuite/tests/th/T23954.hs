{-# LANGUAGE Haskell2010, RoleAnnotations, TemplateHaskell, TypeOperators #-}

import Language.Haskell.TH (runQ)
import Language.Haskell.TH.Ppr (pprint)

main =
  runQ [d|
         data a ## b
         type role (##) nominal nominal
       |]
  >>= putStrLn . pprint
