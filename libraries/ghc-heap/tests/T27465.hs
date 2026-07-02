{-# LANGUAGE MagicHash #-}

import GHC.Exts
import GHC.Exts.Heap

data T = A | B | C

main :: IO ()
main = do
  clos <- getClosureData C
  let expected = I# (dataToTag# C)
  case clos of
    ConstrClosure {info = itbl, name = con} -> do
      putStrLn $ "constructor:  " ++ con
      putStrLn $ "expected tag: " ++ show expected
      putStrLn $ "srtlen field: " ++ show (srtlen itbl)
      if fromIntegral (srtlen itbl) == expected
        then putStrLn "OK"
        else fail "peekItbl returned wrong srtlen"
    _ -> fail $ "unexpected closure: " ++ show clos
