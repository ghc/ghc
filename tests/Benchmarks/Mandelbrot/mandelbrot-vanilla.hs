import System.Environment
import System.IO
import Foreign
import Foreign.Marshal.Array
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad

main = do
    -- width in pixels
    w <- getArgs >>= readIO . head
        -- width in bytes
    let n      = w `div` 8
        -- width of a pixel in the complex plane
        m  = 2 / fromIntegral w
        coords = [T 1 0 y (fromIntegral y * m - 1) | y <- [0..w-1]]
    q <- newChan
    replies <- replicateM w newEmptyMVar
    mapM_ (writeChan q) $ zip coords replies
    numProcs <- getNumCapabilities
    replicateM_ numProcs . forkIO $ worker q w m n

    putStrLn ("P4\n"++show w++" "++show w)
    mapM_ (takeMVar >=> \b -> hPutBuf stdout b n) replies

-- Worker computes one line of the image and sends it to the master
-- q - work queue
-- w - width in pixels
-- m - width of a pixel in the complex plane
-- n - width in bytes
worker q w m n = forever (do
    (coord, reply) <- readChan q
    p <- mallocArray0 n
    unfold (next_x w m n) p coord
    putMVar reply p)

-- f - takes coordinates and returns Nothing if done
--     or the next byte of the bitmap otherwise.
-- ptr - buffer to write to
-- x0 - initial coordinates
unfold :: (T -> Maybe (Word8,T)) -> Ptr Word8 -> T -> IO (Ptr Word8)
unfold !f !ptr !x0 = go ptr x0
  where
    -- p - pointer into the buffer
    -- x - coordinates
    go !p !x = case f x of
        Just (w,y)          -> poke p w >> go (p `plusPtr` 1) y
        Nothing             -> return ptr

-- T bs x y ci
--    bx - x position in bytes
--    x  - x position in pixels
--    y  - y position in pixels
--    ci - y position in complex plane
data T = T !Int !Int !Int !Double

-- w - image width in pixels
-- iw - pixel width in the complex plane
-- bw - image width in bytes
next_x !w !iw !bw (T bx x y ci)
    | bx == bw  = Nothing
    | otherwise = Just (loop_x w x 8 iw ci 0, T (bx+1) (x+8) y ci)

-- w - image width in pixels
-- x - current x coordinate in pixels
-- n - bit positition from 8 to 0
-- iw - pixel width in the complex plane
-- ci - current y coordinate in complex plane
-- b - accumulated bit value.
loop_x !w !x !n !iw !ci !b
    | x < w = if n == 0
                    then b
                    else loop_x w (x+1) (n-1) iw ci (b+b+v)
    | otherwise = b `shiftL` n
  where
    v = fractal 0 0 (fromIntegral x * iw - 1.5) ci 50

-- julia function (r :+ i) (cr :+ ci) with max iterations k.
fractal :: Double -> Double -> Double -> Double -> Int -> Word8
fractal !r !i !cr !ci !k
    | r2 + i2 > 4 = 0
    | k == 0      = 1
    | otherwise   = fractal (r2-i2+cr) ((r+r)*i+ci) cr ci (k-1)
  where
    (!r2,!i2) = (r*r,i*i)
