-- test for #3279

import System.IO.Unsafe
import GHC.Conc
import Control.Exception
import GHC.IO (unsafeUnmask)

f :: Int
f = (1 +) . unsafePerformIO $ do
        throwIO (ErrorCall "foo") `catch` \(SomeExceptionWithLocation e _) -> do
            myThreadId >>= flip throwTo e
            -- point X
            unsafeUnmask $ return 1

main :: IO ()
main = do
    evaluate f `catch` \(SomeExceptionWithLocation e _) -> return 0
    -- the evaluation of 'x' is now suspended at point X
    tid <- mask_ $ forkIO (evaluate f >> return ())
    killThread tid
    -- now execute the 'unblock' above with a pending exception
    yield
    -- should print 1 + 1 = 2
    print f
