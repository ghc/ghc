{-# LANGUAGE Haskell2010, PatternSynonyms, TemplateHaskell, ViewPatterns #-}

import Language.Haskell.TH (runQ)
import Language.Haskell.TH.Ppr (pprint)

main = do
  runQ [d|pattern (:+) :: Int -> Int -> (Int, Int);
          pattern x :+ y = (x, y)|] >>= putStrLn . pprint
  runQ [d|pattern A :: Int -> String;
          pattern A n <- (read -> n) where {
            A 0 = "hi";
            A 1 = "bye"}|] >>= putStrLn . pprint
