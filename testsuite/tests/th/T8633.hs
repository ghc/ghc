module Main where
import Language.Haskell.TH.Syntax

t1 = case mkName "^.." of
    Name (OccName ".")  (NameQ (ModName "^")) -> error "bug0"
    Name (OccName "^..") NameS                -> return ()

t2 = case mkName "Control.Lens.^.." of
    Name (OccName ".")  (NameQ (ModName "Control.Lens.^")) -> error "bug1"
    Name (OccName "^..") (NameQ (ModName "Control.Lens")) -> return ()

t3 = case mkName "Data.Bits..&." of
    Name (OccName ".&.") (NameQ (ModName "Data.Bits")) -> return ()

t4 = case mkName "abcde" of
    Name (OccName "abcde") NameS -> return ()

main :: IO ()
main = do t1; t2; t3; t4