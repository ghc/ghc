{-# LANGUAGE CApiFFI #-}
module Basement.Terminal.Size 
    ( getDimensions
    ) where
        
import           Foreign
import           Foreign.C
import           Basement.Compat.Base
import           Basement.Types.OffsetSize
import           Basement.Numerical.Subtractive
import           Basement.Numerical.Additive
import           Prelude (fromIntegral)

#include "foundation_system.h"
#ifdef FOUNDATION_SYSTEM_WINDOWS

import           System.Win32.Types (HANDLE, BOOL)
import           Graphics.Win32.Misc (getStdHandle, sTD_OUTPUT_HANDLE, StdHandleId)

#include <windows.h>
#elif defined FOUNDATION_SYSTEM_UNIX
#include <sys/ioctl.h>
#ifdef __sun
#include <sys/termios.h>
#endif
#endif 

#include <stdio.h>

#ifdef FOUNDATION_SYSTEM_UNIX
data Winsize = Winsize
    { ws_row    :: !Word16
    , ws_col    :: !Word16
    , ws_xpixel :: !Word16
    , ws_ypixel :: !Word16
    }

instance Storable Winsize where
    sizeOf _ = #{size struct winsize}
    alignment _ = #{alignment struct winsize}
    peek ptr = do
        r <- #{peek struct winsize, ws_row} ptr
        c <- #{peek struct winsize, ws_col} ptr
        x <- #{peek struct winsize, ws_xpixel} ptr
        y <- #{peek struct winsize, ws_ypixel} ptr
        return (Winsize r c x y)
    poke ptr (Winsize r c x y) = do
        #{poke struct winsize, ws_row} ptr r
        #{poke struct winsize, ws_col} ptr c
        #{poke struct winsize, ws_xpixel} ptr x
        #{poke struct winsize, ws_ypixel} ptr y
        
#elif defined FOUNDATION_SYSTEM_WINDOWS
type Handle = Ptr CChar  -- void *

data SmallRect = SmallRect 
    { left   :: !Int16
    , top    :: !Int16
    , right  :: !Int16
    , bottom :: !Int16
    } deriving (Show)

instance Storable SmallRect where
    sizeOf _ = #{size SMALL_RECT}
    alignment _ = #{alignment SMALL_RECT}
    peek ptr = do
        l <- #{peek SMALL_RECT, Left} ptr
        r <- #{peek SMALL_RECT, Right} ptr
        t <- #{peek SMALL_RECT, Top} ptr
        b <- #{peek SMALL_RECT, Bottom} ptr
        return (SmallRect l t r b)
    poke ptr (SmallRect l t r b) = do
        #{poke SMALL_RECT, Left} ptr l
        #{poke SMALL_RECT, Top} ptr t
        #{poke SMALL_RECT, Right} ptr r
        #{poke SMALL_RECT, Bottom} ptr b
        
data Coord = Coord 
    { x :: !Int16
    , y :: !Int16
    } deriving (Show)

instance Storable Coord where
    sizeOf _ = #{size COORD}
    alignment _ = #{alignment COORD}
    peek ptr = do
        x <- #{peek COORD, X} ptr
        y <- #{peek COORD, Y} ptr
        return (Coord x y)
    poke ptr (Coord x y) = do
        #{poke COORD, X} ptr x
        #{poke COORD, Y} ptr y

data ConsoleScreenBufferInfo = ConsoleScreenBufferInfo 
    { dwSize              :: !Coord
    , dwCursorPosition    :: !Coord
    , wAttributes         :: !Word16
    , srWindow            :: !SmallRect
    , dwMaximumWindowSize :: !Coord
    } deriving (Show)

instance Storable ConsoleScreenBufferInfo where
    sizeOf _ = #{size CONSOLE_SCREEN_BUFFER_INFO}
    alignment _ = #{alignment CONSOLE_SCREEN_BUFFER_INFO}
    peek ptr = do
        s <- #{peek CONSOLE_SCREEN_BUFFER_INFO, dwSize} ptr
        c <- #{peek CONSOLE_SCREEN_BUFFER_INFO, dwCursorPosition} ptr
        a <- #{peek CONSOLE_SCREEN_BUFFER_INFO, wAttributes} ptr
        w <- #{peek CONSOLE_SCREEN_BUFFER_INFO, srWindow} ptr
        m <- #{peek CONSOLE_SCREEN_BUFFER_INFO, dwMaximumWindowSize} ptr
        return (ConsoleScreenBufferInfo s c a w m)
    poke ptr (ConsoleScreenBufferInfo s c a w m) = do
        #{poke CONSOLE_SCREEN_BUFFER_INFO, dwSize} ptr s
        #{poke CONSOLE_SCREEN_BUFFER_INFO, dwCursorPosition} ptr c
        #{poke CONSOLE_SCREEN_BUFFER_INFO, wAttributes} ptr a
        #{poke CONSOLE_SCREEN_BUFFER_INFO, srWindow} ptr w
        #{poke CONSOLE_SCREEN_BUFFER_INFO, dwMaximumWindowSize} ptr m
    
invalidHandleValue :: IntPtr
invalidHandleValue = #{const INVALID_HANDLE_VALUE}

stdOutputHandle :: CULong
stdOutputHandle = #{const STD_OUTPUT_HANDLE}
#endif
-- defined FOUNDATION_SYSTEM_WINDOWS

#ifdef FOUNDATION_SYSTEM_UNIX

foreign import capi "sys/ioctl.h ioctl" c_ioctl :: CInt -> CULong -> Ptr a -> IO CInt

-- | Get the terminal windows size
tiocgwinsz :: CULong
tiocgwinsz = Prelude.fromIntegral (#{const TIOCGWINSZ} :: Word)

#elif defined FOUNDATION_SYSTEM_WINDOWS
foreign import ccall "GetConsoleScreenBufferInfo" c_get_console_screen_buffer_info 
  :: HANDLE -> Ptr ConsoleScreenBufferInfo -> IO BOOL
#endif

#ifdef FOUNDATION_SYSTEM_UNIX
ioctlWinsize :: CInt -> IO (Maybe (CountOf Char, CountOf Char))
ioctlWinsize fd = alloca $ \winsizePtr -> do
    status <- c_ioctl fd tiocgwinsz winsizePtr
    if status == (-1 :: CInt)
        then pure Nothing
        else Just . toDimensions <$> peek winsizePtr
  where
    toDimensions winsize =
        ( CountOf . Prelude.fromIntegral . ws_col $ winsize
        , CountOf . Prelude.fromIntegral . ws_row $ winsize)
       
#elif defined FOUNDATION_SYSTEM_WINDOWS
getConsoleScreenBufferInfo :: HANDLE -> IO (Maybe ConsoleScreenBufferInfo)
getConsoleScreenBufferInfo handle = alloca $ \infoPtr -> do
    status <- c_get_console_screen_buffer_info handle infoPtr
    if status
        then Just <$> peek infoPtr
        else pure Nothing
       
winWinsize :: StdHandleId -> IO (Maybe (CountOf Char, CountOf Char))
winWinsize handleRef = (infoToDimensions <$>) <$>
    (getStdHandle handleRef >>= getConsoleScreenBufferInfo)
  where
    infoToDimensions info =
        let window = srWindow info
            width = Prelude.fromIntegral (right window - left window + 1)
            height = Prelude.fromIntegral (bottom window - top window + 1)
         in (CountOf width, CountOf height)
#endif
-- defined FOUNDATION_SYSTEM_WINDOWS

-- | Return the size of the current terminal
--
-- If the system is not supported or that querying the system result in an error
-- then a default size of (80, 24) will be given back.
getDimensions :: IO (CountOf Char, CountOf Char)
getDimensions =
#if defined FOUNDATION_SYSTEM_WINDOWS
    maybe defaultSize id <$> winWinsize sTD_OUTPUT_HANDLE
#elif defined FOUNDATION_SYSTEM_UNIX
    maybe defaultSize id <$> ioctlWinsize 0
#else
    pure defaultSize
#endif
  where
    defaultSize = (80, 24)
