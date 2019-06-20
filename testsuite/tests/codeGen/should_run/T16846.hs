{-# LANGUAGE CPP  #-}
{-# LANGUAGE ExistentialQuantification  #-}
module Main (main) where

import Control.Concurrent.STM

data Free f a = Pure a | Free (f (Free f a))

data SuspendF a
  = forall r. StepSTM (STM r)
  | forall r. StepIO (IO r)

effect :: STM a -> Free SuspendF a
effect a = Free $ StepSTM a

io :: IO a -> Free SuspendF a
io a = Free $ StepIO a

comb :: [Free SuspendF a] -> Free SuspendF a
comb vs = io $ do
  _ <- mapM go vs
  undefined

go :: Free SuspendF a -> IO (STM ())
go (Free (StepIO a))  = a >>= \_ -> go $ Pure undefined
go (Free (StepSTM a)) = pure $ a >>= \_ -> pure ()
go (Pure _)           = pure $ pure ()

runWidget :: Free SuspendF a -> IO a
runWidget w = case w of
  Free (StepIO io) -> do
    _ <- io
    undefined

-- Uncommenting this hid the original bug.
--main :: IO ()
main = runWidget $ comb $ replicate 10000000 (effect retry)
