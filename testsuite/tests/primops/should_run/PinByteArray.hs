{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedDatatypes #-}

module Main where

import Data.Kind
import System.Mem (performGC)
import GHC.Exts
import GHC.IO
import GHC.Prim
-- import GHC.ForeignPtr
import GHC.Compact
import Control.Exception

data U (a :: UnliftedType) = U { unu :: a }

primIO :: (State# RealWorld -> (# State# RealWorld, (a :: UnliftedType) #)) -> IO (U a)
primIO act = IO $ \s -> case act s of (# s, r #) -> (# s, U r #)

isPinned (U b) = isTrue# (isMutableByteArrayPinned# b)

sameArray (U a) (U b) = isTrue# (sameMutableByteArray# a b)

main :: IO ()
main = do
    unpinned <- primIO (newByteArray# 10#)
    large <- primIO (newByteArray# 10000#)
    pinned <- primIO (newPinnedByteArray# 1#)
    compact_region <- compact $ large
    let ar_compact = getCompact compact_region
    let arrs = [unpinned,large,pinned,ar_compact]

    putStr "Small:"
    print $ isPinned unpinned
    putStr "Large:"
    print $ isPinned large
    putStr "Compacted:"
    print $ isPinned ar_compact
    putStr "Pinned:"
    print $ isPinned pinned

    -- Try to compact the three types of arrays.
    !_ <- compact unpinned -- Expected to work
    !_ <- compact large -- Expected to work
    !_ <- compact ar_compact -- Expected to work
    -- This one should fail.
    catch (compact pinned >> return ()) (\(e :: CompactionFailed) -> print "Failed to compact pinned array as expected." >> return ())

    -- Call unsafePinMutableByteArray# on all arrays.
    [pinned_unpinned, pinned_large, pinned_pinned, pinned_compact] <- mapM (\(U arr) -> primIO (unsafePinMutableByteArray# arr)) arrs

    putStrLn "Pinnedness of original array references after unsafePinMutableByteArray#"
    -- The large one should be pinned now
    putStr "Small:"
    print $ isPinned unpinned
    putStr "Large:"
    print $ isPinned large
    putStrLn "Compacted:"
    print $ isPinned ar_compact
    putStrLn "Pinned:"
    print $ isPinned pinned

    putStrLn "Pinnedness of arrays returned from unsafePinMutableByteArray#"
    -- These should all be pinned now
    putStr "Small:"
    print $ isPinned pinned_unpinned
    putStr "Large:"
    print $ isPinned pinned_large
    putStr "Compacted:"
    print $ isPinned pinned_compact
    putStr "Pinned:"
    print $ isPinned pinned_pinned

    putStrLn "Have references been pinned in-place?"
    -- The large and pinned array should have been pinned in place.
    putStr "Small:"
    print $ sameArray unpinned pinned_unpinned
    putStr "Large:"
    print $ sameArray large pinned_large
    putStr "Compacted:"
    print $ sameArray ar_compact pinned_compact
    putStr "Pinned:"
    print $ sameArray pinned pinned_pinned

    -- The large array should have been pinned in place and therefore should fail to compact.
    catch (compact large >> return ()) (\(e :: CompactionFailed) -> print "Failed to compact large array post-pin(expected to fail)." >> return ())
    catch (compact ar_compact >> return ()) (\(e :: CompactionFailed) -> print "Failed to compact ar_compact array post-pin(expected to fail)." >> return ())

    return ()
