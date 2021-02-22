module Main (main) where

import Language.Haskell.TH.Ppr    (ppr)
import Language.Haskell.TH.Syntax (Dec (PragmaD), Pragma (CompleteP), mkName, Type(ConT))

main :: IO ()
main = print $ ppr $ PragmaD $ CompleteP [mkName "Foo", mkName "Bar"] $ Just (ConT (mkName "Bar"))
