-- This test checks that restoring a continuation that captures a CATCH frame
-- properly adjusts the async exception masking state.

import Control.Exception
import Data.IORef

import ContIO

data E = E deriving (Show)
instance Exception E

printMaskingState :: IO ()
printMaskingState = print =<< getMaskingState

main :: IO ()
main = do
  tag <- newPromptTag
  ref <- newIORef Nothing
  mask_ $ prompt tag $
    catch (control0 tag $ \k ->
             writeIORef ref (Just k))
          (\E -> printMaskingState)
  Just k <- readIORef ref

  let execute_test = do
        k (printMaskingState *> throwIO E)
        printMaskingState

  putStrLn "initially unmasked:"
  execute_test

  putStrLn "\ninitially interruptibly masked:"
  mask_ execute_test

  putStrLn "\ninitially uninterruptibly masked:"
  uninterruptibleMask_ execute_test
