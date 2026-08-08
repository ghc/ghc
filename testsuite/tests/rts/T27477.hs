{-# LANGUAGE BangPatterns #-}

-- Stress test for #27477: missing ACQUIRE in eval_thunk_selector
-- (rts/sm/Evac.c).
--
-- With the bug present this test crashes or reports value mismatches
-- within a few seconds on AArch64; on strongly-ordered targets
-- it just passes.

module Main (main) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.IORef
import System.Environment
import System.Exit
import System.IO
import System.Mem (performMinorGC)

data T = T { nxt :: T, val :: !Int }

-- expected values and the cells that should evaluate to them
type Batch = ([Int], [T])

tailD, branchD, branchesPerCell :: Int
tailD = 12          -- shared tail length per cluster
branchD = 3         -- private branch length
branchesPerCell = 2
-- tailD + branchD must stay below MAX_THUNK_SELECTOR_DEPTH (16)

{-# NOINLINE spineEnd #-}
spineEnd :: T
spineEnd = errorWithoutStackTrace "spine end reached"

-- Fully evaluated spine of constructors; cell i from the head has val i+1.
buildSpine :: Int -> IO T
buildSpine len = evaluate (go len (T spineEnd 0))
  where
    go :: Int -> T -> T
    go 0 acc = acc
    go k acc = go (k - 1) (T acc k)

-- k selector thunks, shallow-first: cell i selects from cell i-1,
-- cell 0 selects from t.  If t evaluates to spine cell m (val m+1),
-- cell i evaluates to spine cell m+1+i (val m+2+i).
{-# NOINLINE selChain #-}
selChain :: Int -> T -> [T]
selChain 0 _ = []
selChain k t = let t' = nxt t in t' : selChain (k - 1) t'

lenL :: [a] -> Int
lenL = go 0
  where
    go !n [] = n
    go !n (_ : xs) = go (n + 1) xs

everyN :: Int -> [a] -> [a]
everyN _ [] = []
everyN n (x : xs) = x : everyN n (drop (n - 1) xs)

-- One branch hanging off tail cell tj (which evaluates to val jval):
-- branch cell i evaluates to jval+1+i.  Returned deepest-first.
{-# NOINLINE mkBranch #-}
mkBranch :: Int -> T -> IO Batch
mkBranch jval tj = do
  let cells = selChain branchD tj
  _ <- evaluate (lenL cells)
  let expected = [ jval + 1 + i | i <- [0 .. branchD - 1] ]
  pure (reverse expected, reverse cells)

-- A cluster: one private spine, one shared tail of selectors over it,
-- branchesPerCell branches of selectors converging on every tail cell.
-- Neither the spine nor the tail is returned: they must stay reachable
-- only through the branch selectors.
buildCluster :: IO [Batch]
buildCluster = do
  sp <- buildSpine (tailD + branchD + 4)
  let tails = selChain tailD sp
  _ <- evaluate (lenL tails)
  bss <- forM (zip [0 ..] tails) $ \(j, tj) ->
    replicateM branchesPerCell (mkBranch (j + 2) tj)
  pure (concat bss)

verifyBatch :: IORef Int -> Int -> Batch -> IO ()
verifyBatch errs wid (es0, ts0) = go es0 ts0
  where
    go (e : es) (t : ts) = do
      v <- evaluate (val t)
      when (v /= e) $ do
        hPutStrLn stderr $
          "MISMATCH worker=" ++ show wid
            ++ " expected=" ++ show e ++ " got=" ++ show v
        modifyIORef' errs (+ 1)
      go es ts
    go _ _ = pure ()

-- simple cyclic barrier
newtype Barrier = Barrier (MVar (Int, [MVar ()]))

newBarrier :: IO Barrier
newBarrier = Barrier <$> newMVar (0, [])

awaitBarrier :: Barrier -> Int -> IO ()
awaitBarrier (Barrier st) total = do
  w <- newEmptyMVar
  join $ modifyMVar st $ \(k, ws) ->
    if k + 1 == total
      then pure ((0, []), mapM_ (\m -> putMVar m ()) ws)
      else pure ((k + 1, w : ws), takeMVar w)

worker :: Int -> Int -> Int -> Barrier -> [IORef [Batch]] -> IORef Int -> Int -> IO ()
worker rounds clusters nWorkers bar slots errs wid =
  forM_ [1 .. rounds] $ \(_r :: Int) -> do
    -- build fresh selector clusters, publish them
    batches <- concat <$> replicateM clusters buildCluster
    writeIORef (slots !! wid) batches
    awaitBarrier bar nWorkers
    -- root each branch's deepest cell via fresh cons cells in our own
    -- nursery (so our GC thread evacuates the shared cells itself), in
    -- a per-worker interleaved order so that different GC threads
    -- sweep different branches of the same shared tails concurrently
    published <- mapM readIORef slots
    let allBatches = concat published
        ordered
          | nWorkers > 1 =
              concat [ everyN nWorkers (drop ((wid + k) `mod` nWorkers) allBatches)
                     | k <- [0 .. nWorkers - 1] ]
          | otherwise = allBatches
        deepTs = [ t | (_, t : _) <- ordered ]
        deepEs = [ e | (e : _, _) <- ordered ]
    _ <- evaluate (lenL deepTs)
    _ <- evaluate (lenL deepEs)
    awaitBarrier bar nWorkers
    -- run a parallel GC with everything live
    when (wid == 0) performMinorGC
    awaitBarrier bar nWorkers
    -- force everything and check the values
    verifyBatch errs wid (deepEs, deepTs)
    awaitBarrier bar nWorkers

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  args <- getArgs
  let atDef d i = if length args > i then read (args !! i) else d
      rounds = atDef 600 0
      clusters = atDef 48 1
  n <- getNumCapabilities
  bar <- newBarrier
  slots <- replicateM n (newIORef [])
  errs <- newIORef 0
  dones <- forM [0 .. n - 1] $ \i -> do
    done <- newEmptyMVar
    _ <- forkOn i $
      (worker rounds clusters n bar slots errs i >> putMVar done Nothing)
        `catch` \e -> putMVar done (Just (e :: SomeException))
    pure done
  results <- mapM takeMVar dones
  forM_ results $ \r -> case r of
    Just e -> do
      hPutStrLn stderr ("worker exception: " ++ show e)
      exitWith (ExitFailure 2)
    Nothing -> pure ()
  ec <- readIORef errs
  when (ec > 0) $ do
    hPutStrLn stderr ("FAILED: " ++ show ec ++ " mismatches")
    exitWith (ExitFailure 2)
  putStrLn "OK"
