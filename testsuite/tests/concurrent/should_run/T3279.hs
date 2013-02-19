-- test for #3279

import Data.IORef
import System.IO.Unsafe
import GHC.Conc
import Control.Exception

main :: IO ()
main = do
    restoreRef <- newIORef id

    let f :: Int
        f = (1 +) . unsafePerformIO $ do
                error "foo" `catch` \(SomeException e) -> do
                    myThreadId >>= flip throwTo e
                    -- point X
                    restore <- readIORef restoreRef
                    restore $ return 1

    evaluate f `catch` \(SomeException e) -> return 0
    -- the evaluation of 'x' is now suspended at point X
    tid <- mask $ \restore -> do writeIORef restoreRef restore
                                 forkIO (evaluate f >> return ())
    killThread tid
    -- now execute the 'unblock' above with a pending exception
    yield
    writeIORef restoreRef id
    -- should print 1 + 1 = 2
    print f
    
