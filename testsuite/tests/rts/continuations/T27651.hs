-- When capturing or resuming a continuation adjusts the async exception
-- masking state, the RTS trampolines through a mask/unmask frame, and the
-- stack must be well-formed at that point: with a blocked exception
-- pending, the eager raise in stg_unmaskAsyncExceptionszh_ret walks the
-- whole stack. 
--
-- Phase 1 exercises the capture side (stg_control0zh_ll): control0# runs
-- inside uninterruptibleMask_ while another thread has queued an
-- exception via throwTo, so the capture unmasks with the exception
-- pending. The frame evaluated between the unmask frame and the prompt
-- keeps raw Int# payload live so that a stale word on the stack cannot
-- masquerade as a valid frame by accident.
--
-- Phase 2 exercises the resume side (stg_CONTINUATION_apply): the
-- continuation is captured while unmasked (inside mask/restore), so
-- resuming it unmasks, and it is applied from a thread that is masked
-- with an exception pending.
import Control.Concurrent
import Control.Exception
import Control.Monad

import ContIO

data Boom = Boom deriving Show
instance Exception Boom

{-# NOINLINE useInts #-}
useInts :: Int -> Int -> Int -> Int -> Int -> Int
useInts a b c d e = a + b * c + d * e

rounds :: Int
rounds = 150

phase1 :: Int -> IO ()
phase1 i = do
  mv   <- newEmptyMVar
  done <- newEmptyMVar
  let !p = i * 7919 + 3    -- raw ints to live in the continuation frame
      !q = i * 104729 + 7
      !u = i * 1299709 + 11
      !v = i * 15485863 + 13
  a <- forkIO $
    handle (\Boom -> void (tryPutMVar done (Left Boom))) $ do
      tag <- newPromptTag
      r <- prompt tag $ do
             x <- uninterruptibleMask_ $ do
                    putMVar mv ()
                    threadDelay 2000  -- let the thrower queue its exception
                    control0 tag (\_k -> pure (42 :: Int))
             -- continuation frame between the unmask frame and the
             -- prompt frame, carrying raw Int# payload:
             pure (useInts x p q u v)
      void (tryPutMVar done (Right r))
  takeMVar mv
  _ <- forkIO $ throwTo a Boom
  void (takeMVar done)

phase2 :: Int -> IO ()
phase2 i = do
  mv   <- newEmptyMVar
  done <- newEmptyMVar
  kvar <- newEmptyMVar
  let !p = i * 7919 + 3
      !q = i * 104729 + 7
      !u = i * 1299709 + 11
      !v = i * 15485863 + 13
  -- Capture a continuation whose resumption unmasks: the capture happens
  -- inside restore, so its apply_mask_frame is the unmask frame.
  _ <- forkIO $ do
    tag <- newPromptTag
    _ <- prompt tag $ mask $ \restore -> do
           x <- restore (control0 tag (\k -> putMVar kvar k >> pure 0))
           pure (useInts x p q u v)
    pure ()
  k <- takeMVar kvar
  a <- forkIO $
    handle (\Boom -> void (tryPutMVar done (Left Boom))) $ do
      r <- uninterruptibleMask_ $ do
             putMVar mv ()
             threadDelay 2000  -- let the thrower queue its exception
             k (pure 42)  -- resuming unmasks with the exception pending
      void (tryPutMVar done (Right r))
  takeMVar mv
  _ <- forkIO $ throwTo a Boom
  void (takeMVar done)

main :: IO ()
main = do
  forM_ [1 .. rounds] phase1
  forM_ [1 .. rounds] phase2
  putStrLn "ok"
