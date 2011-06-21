-- !!! test for foreign import dynamic/wrapper, orignally by Alastair Reid,
-- with a few changes to get it to run on GHC by Simon Marlow.

import Foreign hiding ( unsafePerformIO )
import Foreign.C
import Control.Exception
import System.IO.Unsafe
import Prelude hiding (read)
import System.IO (hFlush, stdout)

main = do

  putStrLn "\nTesting sin==mysin (should return lots of Trues)"
  print (testSin sin mysin)

-- disabled because errno is not necessarily a label these days

--   putStrLn "\nTesting errno"
--   err <- peek errno
--  putStrLn $ "errno == " ++ show err

  putStrLn "\nTesting puts (and withString)"
  withCString "Test successful" puts

  putStrLn "\nTesting peekArray0"
  s <- withCString "Test successful" (peekArray0 (castCharToCChar '\0'))
  putStr (map castCCharToChar s)

-- disabled due to use of non-portable constants in arguments to open:

--  putStrLn "\nTesting open, read and close"
--  s <- testRead "ffi005.hs" 200
--  putStrLn (map castCCharToChar s)

--  putStrLn "\nTesting open, write and close"
--  testWrite "/tmp/test_write" "Test successful"

  putStrLn "\nTesting sin==dynamic_sin (should return lots of Trues)"
  print (testSin sin (dyn_sin sin_addr))

  putStrLn "\nTesting sin==IO wrapped_sin (should return lots of Trues)"
  sin_addr2 <- wrapIO (return . sin)
  print (testSin sin (unsafePerformIO . (dyn_sinIO sin_addr2)))
  freeHaskellFunPtr sin_addr2

  putStrLn "\nTesting sin==Id wrapped_sin (should return lots of Trues)"
  sin_addr3 <- wrapId sin
  print (testSin sin (dyn_sin sin_addr3))
  freeHaskellFunPtr sin_addr3

  putStrLn "\nTesting exit"
  hFlush stdout
  exit 3

testSin f g = [ (f x == g x) | x <- [0,0.01 .. 1] ]

foreign import ccall "sin" mysin :: CDouble -> CDouble
foreign import ccall "dynamic" dyn_sin :: FunPtr (CDouble -> CDouble) -> (CDouble -> CDouble)
foreign import ccall "dynamic" dyn_sinIO :: FunPtr (CDouble -> IO CDouble) -> (CDouble -> IO CDouble)
foreign import ccall "&sin" sin_addr :: FunPtr (CDouble -> CDouble)
foreign import ccall "wrapper" wrapId :: (CDouble -> CDouble) -> IO (FunPtr (CDouble -> CDouble))
foreign import ccall "wrapper" wrapIO :: (CDouble -> IO CDouble) -> IO (FunPtr (CDouble -> IO CDouble))

-- foreign import ccall safe "static stdlib.h &errno" errno :: Ptr CInt

withBuffer sz m = do
  b <- mallocArray sz
  sz' <- m b
  s <- peekArray sz' b
  free b
  return s

foreign import ccall puts :: CString -> IO CInt

-- foreign import ccall "open" open'  :: CString -> CInt -> IO CInt
-- foreign import ccall "open" open2' :: CString -> CInt -> CInt -> IO CInt
-- foreign import ccall "creat" creat' :: CString -> CInt -> IO CInt
-- foreign import ccall        close :: CInt -> IO CInt
-- foreign import ccall "read" read' :: CInt -> CString -> CInt -> IO CInt
-- foreign import ccall "write" write' :: CInt -> CString -> CInt -> IO CInt

-- creat s m   = withCString s $ \s' -> unix "creat" $ creat' s' m
-- open s m    = withCString s $ \s' -> unix "open"  $ open' s' m
-- open2 s m n = withCString s $ \s' -> unix "open2" $ open2' s' m n
-- write fd s  = withCString s $ \s' -> unix "write" $ write' fd s' (fromIntegral (length s))
-- read  fd sz = withBuffer sz $ \s' -> unix "read"  $ read' fd s' (fromIntegral sz)

-- unix s m = do
--   x <- m
--   if x < 0
--    then do
--      err <- peek errno
--      ioError $ userError $ s ++ ": " ++ show (x,err)
--    else return (fromIntegral x)

-- testRead fn sz = bracket (open fn 0) close (flip read sz)
-- testWrite fn s = bracket (open2 fn (512+64+1) 511) close (flip write s)

foreign import ccall exit :: Int -> IO ()

-- Various bits of rubbish.
-- foreign import ccall "static stdlib.h exit" (***) :: CString -> CString -> IO Int
--
-- foreign import ccall safe "static stdlib.h printf" (+++) :: CString -> CString -> IO Int
-- foreign import ccall safe "static stdlib.h &errno" illegal_foo :: Ptr Int
--
-- foreign import ccall safe "wrapper" illegal_bar :: Char -> IO (FunCString)
-- foreign import ccall safe "dynamic" illegal_baz :: FunCString -> Char

-- foreign export ccall "id_charstar" id :: CString -> CString
  
