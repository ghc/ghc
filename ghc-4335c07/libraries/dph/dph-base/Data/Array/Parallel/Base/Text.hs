-- | Utilities for defining Read\/Show instances.
module Data.Array.Parallel.Base.Text 
        ( showsApp
        , readApp
        , readsApp
        , Read(..))
where
import Text.Read


showsApp :: Show a => Int -> String -> a -> ShowS
showsApp k fn arg 
        = showParen (k>10) 
          (showString fn . showChar ' ' . showsPrec 11 arg)

readApp :: Read a => String -> ReadPrec a
readApp fn 
 = parens $ prec 10 
 $ do   Ident ide <- lexP
        if ide /= fn then pfail else step readPrec


readsApp :: Read a => Int -> String -> ReadS a
readsApp k fn = readPrec_to_S (readApp fn) k

