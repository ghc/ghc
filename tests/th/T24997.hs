{-# LANGUAGE Haskell2010, TemplateHaskell, UnboxedSums #-}

import Language.Haskell.TH (runQ, Type (UnboxedSumT, UnboxedTupleT))
import Language.Haskell.TH.Ppr (pprint)

main = do
  runQ [t| (# Int | Char | Bool #) |] >>= putStrLn . pprint
  runQ [t| (# Int, Char, Bool #) |] >>= putStrLn . pprint
  runQ [t| $(pure (UnboxedTupleT 3)) Int Char |] >>= putStrLn . pprint
  runQ [t| $(pure (UnboxedSumT 3)) Int Char |] >>= putStrLn . pprint
