module T10279 where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- NB: rts-1.0.2 is used here because it doesn't change.
-- You do need to pick the right version number, otherwise the
-- error message doesn't recognize it as a source package ID,
-- (This is OK,  since it will look obviously wrong when they
-- try to find the package in their package database.)
blah = $(conE (Name (mkOccName "Foo") (NameG VarName (mkPkgName "rts-1.0.3") (mkModName "A"))))
