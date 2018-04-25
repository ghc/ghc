{- |
   Module      :  System.Win32.Console.HWND
   Copyright   :  2009 Balazs Komuves, 2013 shelarcy
   License     :  BSD-style

   Maintainer  :  shelarcy@gmail.com
   Stability   :  Provisional
   Portability :  Non-portable (Win32 API)

   Get the handle of the current console window.
-}
module System.Win32.Console.HWND where
import Control.Concurrent           ( threadDelay )
import Control.Exception            ( bracket )
import Foreign.Ptr                  ( nullPtr )
import Graphics.Win32.Window        ( c_FindWindow )
import Graphics.Win32.GDI.Types     ( HWND )
import System.Win32.Console.Title   ( getConsoleTitle, setConsoleTitle )
import System.Win32.Process ( getCurrentProcessId )
import System.Win32.String          ( withTString )
import System.Win32.Time            ( getTickCount )

-- | Get the handle of the current console window by using window's title.
-- See: <http://support.microsoft.com/kb/124103>
getConsoleHWND :: IO HWND
getConsoleHWND
  = bracket getConsoleTitle setConsoleTitle $ \_ -> do
        time   <- getTickCount
        pid    <- getCurrentProcessId
        let unique = show time ++ show pid
        setConsoleTitle unique
        threadDelay (42*1000)
        withTString unique $ \punique ->
            c_FindWindow nullPtr punique
