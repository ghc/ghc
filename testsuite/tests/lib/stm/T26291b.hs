-- Test for the findAtomicallyFrameHelper crash when running_alt_code=1.
--
-- findAtomicallyFrameHelper is called by stg_abort, which fires when a nested
-- transaction's stmCommitNestedTransaction fails (due to a concurrent TVar
-- write conflicting with the nested trec's read set).  If the walk encounters
-- a CATCH_RETRY_FRAME with running_alt_code=1, the old code unconditionally
-- called stmAbortTransaction on tso->trec, which is the *parent* transaction
-- (no nested trec exists for the RHS).  That freed the parent trec, leaving
-- tso->trec as garbage; stg_abort then dereferenced it and crashed.
--
-- The structure that exercises this:
--   outer orElse: LHS retries → RHS runs (outer CATCH_RETRY_FRAME has running_alt_code=1)
--   inner orElse: LHS reads a TVar in a nested trec and tries to commit
--     → if a concurrent writer invalidates the read, stmCommitNestedTransaction fails
--     → stg_abort → findAtomicallyFrameHelper encounters the outer CATCH_RETRY_FRAME
--       (running_alt_code=1) → crash without the fix.
module Main where

import Control.Concurrent
import Control.Concurrent.STM

main :: IO ()
main = do
  tv <- newTVarIO (0 :: Int)

  -- Continuously modify tv to provoke nested-commit failures.
  _ <- forkIO $ let loop = atomically (modifyTVar' tv (+1)) >> loop in loop

  -- Run the critical orElse pattern many times.  Each iteration the inner LHS
  -- reads tv (nested trec) and tries to commit; concurrent writes will
  -- occasionally cause the commit to fail and trigger stg_abort.
  let loop 0 = return ()
      loop n = do
        _ <- atomically $
          orElse
            retry                             -- outer LHS: always retries
            (orElse (readTVar tv) (return 0)) -- outer RHS (running_alt_code=1):
                                              --   inner LHS reads tv (nested trec)
        loop (n - 1)
  loop (100000 :: Int)

  putStrLn "OK"
