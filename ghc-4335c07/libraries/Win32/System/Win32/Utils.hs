{- |
   Module      :  System.Win32.Utils
   Copyright   :  2009 Balazs Komuves, 2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Utilities for calling Win32 API
-}
module System.Win32.Utils
  ( try, tryWithoutNull, try'
  -- * Maybe values
  , maybePtr, ptrToMaybe, maybeNum, numToMaybe
  , peekMaybe, withMaybe
  ) where
import Control.Monad               ( unless )
import Foreign.Marshal.Array       ( allocaArray, peekArray )
import Foreign.Marshal.Utils       ( with )
import Foreign.Ptr                 ( Ptr, nullPtr )
import Foreign.Storable            ( Storable(..) )
import System.Win32.Types          ( failIfZero
                                   , failWith, getLastError, eRROR_INSUFFICIENT_BUFFER )
import qualified System.Win32.Types ( try )
import System.Win32.String         ( LPTSTR, peekTString )
import System.Win32.Types          ( BOOL, UINT, maybePtr, ptrToMaybe, maybeNum, numToMaybe )
import System.Win32.Word           ( DWORD, PDWORD )


-- | Support for API calls that are passed a fixed-size buffer and tell
-- you via the return value if the buffer was too small.  In that
-- case, we extend the buffer size and try again.
try :: String -> (LPTSTR -> UINT -> IO UINT) -> UINT -> IO String
try = System.Win32.Types.try
{-# INLINE try #-}

tryWithoutNull :: String -> (LPTSTR -> UINT -> IO UINT) -> UINT -> IO String
tryWithoutNull loc f n = do
   e <- allocaArray (fromIntegral n) $ \lptstr -> do
          r <- failIfZero loc $ f lptstr n
          if (r > n) then return (Left r) else do
            str <- peekTString lptstr
            return (Right str)
   case e of
        Left r'   -> tryWithoutNull loc f r'
        Right str -> return str

try' :: Storable a => String -> (Ptr a -> PDWORD -> IO BOOL) -> DWORD -> IO [a]
try' loc f n =
   with n $ \n' -> do
   e <- allocaArray (fromIntegral n) $ \lptstr -> do
          flg <- f lptstr n'
          unless flg $ do
            err_code <- getLastError
            unless (err_code == eRROR_INSUFFICIENT_BUFFER)
              $ failWith loc err_code
          r   <- peek n'
          if (r > n) then return (Left r) else do
            str <- peekArray (fromIntegral r) lptstr
            return (Right str)
   case e of
        Left r'   -> try' loc f r'
        Right str -> return str

-- | See also: 'Foreign.Marshal.Utils.maybePeek' function.
peekMaybe :: Storable a => Ptr a -> IO (Maybe a)
peekMaybe p = 
  if p == nullPtr
    then return Nothing
    else Just `fmap` peek p

-- | See also: 'Foreign.Marshal.Utils.maybeWith' function.
withMaybe :: Storable a => Maybe a -> (Ptr a -> IO b) -> IO b
withMaybe Nothing  action = action nullPtr
withMaybe (Just x) action = with x action
