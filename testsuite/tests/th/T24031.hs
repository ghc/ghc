{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LinearTypes #-}
-- | This test shows that we can reify a bunch of primitive types.
-- Additionally it acts as a golden test to ensure that we don't
-- accidentally change our output for these types.

import Control.Monad
import Data.Char
import GHC.Exts
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import GHC.Builtin.Types.Prim (primTyCons)
import GHC.Tc.Gen.Splice (reifyName)

main :: IO ()
main = $(do let types = map reifyName primTyCons
            reified <- mapM reify types
            let stripInt [] = []
                stripInt (x:xs)
                  | isDigit x = stripInt xs
                  | otherwise = x:xs
            -- remove _[0-9]* sequences
            let stripUniques ('_':xs) = stripUniques $ stripInt xs
                stripUniques (x:xs)= x:stripUniques xs
                stripUniques [] = []
            let output = lift $ map (stripUniques . show) reified
            [| mapM_ putStrLn $output |])
