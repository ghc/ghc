-- This test verifies that the async exception masking state is captured and
-- restored appropriately during continuation capture and restore.

import Control.Exception
import ContIO

main :: IO ()
main = do
  tag <- newPromptTag
  uninterruptibleMask $ \unmaskUninterruptible ->
    prompt tag $ unmaskUninterruptible $
    mask $ \unmaskInterruptible ->
    control0 tag $ \k -> do
      print =<< getMaskingState -- should be MaskedUninterruptible
      unmaskInterruptible $ do
        k (print =<< getMaskingState) -- should be MaskedInterruptible
        print =<< getMaskingState -- should be Unmasked
