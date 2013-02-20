-- test for #3279

import System.IO.Unsafe
import GHC.Conc
import Control.Exception

f :: Int
f = (1 +) . unsafePerformIO $ do
        error "foo" `catch` \(SomeException e) -> do
            myThreadId >>= flip throwTo e
            -- point X
            unblock $ return 1

main :: IO ()
main = do
    evaluate f `catch` \(SomeException e) -> return 0
    -- the evaluation of 'x' is now suspended at point X
    tid <- block $ forkIO (evaluate f >> return ())
    killThread tid
    -- now execute the 'unblock' above with a pending exception
    yield
    -- should print 1 + 1 = 2
    print f
    
