{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.C
import Foreign

getProgName :: IO String
getProgName =
  alloca $ \ p_argc ->
  alloca $ \ p_argv -> do
     getProgArgv p_argc p_argv
     argv <- peek p_argv
     unpackProgName argv

unpackProgName  :: Ptr (Ptr CChar) -> IO String   -- argv[0]
unpackProgName argv = do
  s <- peekElemOff argv 0 >>= peekCString
  return (basename s)
  where
   basename :: String -> String
   basename f = go f f
    where
      go acc [] = acc
      go acc (x:xs)
        | isPathSeparator x = go xs xs
        | otherwise         = go acc xs

   isPathSeparator :: Char -> Bool
   isPathSeparator '/'  = True
   isPathSeparator '\\' = True
   isPathSeparator _    = False

foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()

main :: IO ()
main = print =<< getProgName
