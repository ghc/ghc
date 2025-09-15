-- test for #3279

import System.IO.Unsafe
import GHC.Conc
import Control.Exception
import GHC.IO (unsafeUnmask)

f :: Int
f = (1 +) . unsafePerformIO $ do
        -- NB: We have to use `catchNoPropagate` rather than `catch`.
        -- `catch` starting from !13302 turns all asynchronous exceptions into synchronous ones
        -- because it has to catch and rethrow the exception to annotate it
        -- with the `WhileHandling` backtrace. Using `catchNoPropagate`
        -- preserves the asynchronous exception stack unwinding behaviour which
        -- allows this test to work.
        -- See #25300 and !13302 for details.
        throwIO (ErrorCall "foo") `catchNoPropagate` \(ExceptionWithContext c (SomeException e)) -> do
            myThreadId >>= flip throwTo (ExceptionWithContext c (SomeException e))
            -- point X
            unsafeUnmask $ return 1

main :: IO ()
main = do
    evaluate f `catch` \(SomeException e) -> return 0
    -- the evaluation of 'x' is now suspended at point X
    tid <- mask_ $ forkIO (evaluate f >> return ())
    killThread tid
    -- now execute the 'unblock' above with a pending exception
    yield
    -- should print 1 + 1 = 2
    print f

