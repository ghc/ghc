module Main (main) where

import Language.Haskell.TH.Ppr    (ppr)
import Language.Haskell.TH.Syntax (Dec (PragmaD), Pragma (CompleteP), mkName)

main :: IO ()
main = print $ ppr $ PragmaD $ CompleteP [mkName "Foo", mkName "Bar"] $ Just $ mkName "Bar"
