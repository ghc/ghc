import Control.Exception
import Text.Printf

-- Test all the various combinations of nesting mask/uninterruptibleMask

main = do
  stat 1 Unmasked
  mask_ $ stat 2 MaskedInterruptible
  mask $ \restore -> do
    stat 3 MaskedInterruptible
    restore $ stat 4 Unmasked
    restore $ restore $ stat 5 Unmasked
    stat 6 MaskedInterruptible
    uninterruptibleMask $ \restore -> do
      stat 7 MaskedUninterruptible
      restore $ stat 8 MaskedInterruptible
      restore $ restore $ stat 9 MaskedInterruptible
      stat 10 MaskedUninterruptible
      mask $ \restore -> do
        stat 11 MaskedUninterruptible
        restore $ stat 12 MaskedUninterruptible
        restore $ restore $ stat 13 MaskedUninterruptible
        stat 14 MaskedUninterruptible
      stat 15 MaskedUninterruptible
    stat 16 MaskedInterruptible
  stat 17 Unmasked

  uninterruptibleMask $ \restore -> do
    stat 20 MaskedUninterruptible
    restore $ stat 21 Unmasked
    restore $ restore $ stat 22 Unmasked
    stat 23 MaskedUninterruptible
    mask $ \restore -> do
      stat 24 MaskedUninterruptible
      restore $ stat 25 MaskedUninterruptible
      restore $ restore $ stat 26 MaskedUninterruptible
      stat 27 MaskedUninterruptible
      uninterruptibleMask $ \restore -> do
        stat 28 MaskedUninterruptible
        restore $ stat 29 MaskedUninterruptible
        restore $ restore $ stat 30 MaskedUninterruptible
        stat 31 MaskedUninterruptible
      stat 32 MaskedUninterruptible
    stat 33 MaskedUninterruptible
  stat 34 Unmasked

  -- it is possible to call a restore from a mask that is not the
  -- innermost enclosing one, although this is not a recommended use
  -- of the API.
  mask $ \restore0 -> do
    stat 41 MaskedInterruptible 
    -- it is possible to call a restore from a mask that is not the
    uninterruptibleMask $ \restore1 -> do
      stat 42 MaskedUninterruptible
      restore0 $ stat 43 Unmasked
      restore0 $ restore0 $ stat 44 Unmasked
      restore1 $ stat 45 MaskedInterruptible
      restore1 $ restore1 $ stat 46 MaskedInterruptible
      restore0 $ restore1 $ stat 47 MaskedInterruptible
      restore1 $ restore0 $ stat 48 Unmasked
      stat 49 MaskedUninterruptible
    stat 50 MaskedInterruptible
  stat 51 Unmasked

stat :: Int -> MaskingState -> IO ()
stat n m = do 
 s <- getMaskingState
 if (s /= m) 
    then error (printf "%2d: %s\n" n (show s))
    else return ()
