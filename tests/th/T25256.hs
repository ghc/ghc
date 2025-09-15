{-# LANGUAGE Haskell2010, ScopedTypeVariables, TemplateHaskell #-}

import Language.Haskell.TH (runQ, Type (UnboxedSumT, UnboxedTupleT))
import Language.Haskell.TH.Ppr (pprint)

main = runQ [d| f ((a :: [Char]) :: String) = (a :: [Char]) :: String |] >>= putStrLn . pprint
