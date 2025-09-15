{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH.Syntax

[d| f = Bool |] >>= addTopDecls >> return []

main = return ()
