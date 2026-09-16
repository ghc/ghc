-- As LitRubbishStatic, but with one field of MkT still live, so the
-- constructor application stays inside the worker and is built at run time by
-- a PACK instruction rather than at link time. Its three fields are the three
-- cases of Note [Absent fillers] side by side: a real value, an 'absentError'
-- thunk for the absent lazy field, and a rubbish literal for the absent
-- strict one. The rubbish field must still hold a heap pointer, because the
-- collector traces it; see Note [Rubbish literals of boxed type] in
-- GHC.StgToByteCode.
--
-- The collection has to happen while the constructor is still alive, and it
-- is alive only for the duration of the call it was built for -- 'useps'
-- ignores it, which is what makes its fields absent in the first place, so it
-- is garbage the moment that call returns. Hence 'useps' collects itself,
-- while the argument is still live in the frame that passed it.
module Main where

import System.Mem (performMajorGC)

data T = MkT [Int] [Int] ![Int]

{-# OPAQUE useps #-}
useps :: T -> IO Int
useps _ = do { performMajorGC; return 0 }

f :: T -> IO Int
f ps = case ps of
  MkT xs _ _ -> do
    n <- useps ps
    return (length xs + n)
{-# NOINLINE f #-}

main :: IO ()
main = do
  n <- f (MkT [1,2] [3,4] [5,6])
  print n
  putStrLn "collected"
