#include "Common-Safe-Haskell.hs"
{-# OPTIONS_HADDOCK hide        #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE DeriveDataTypeable #-}

{-| "System.Win32.Console" is really very impoverished, so I have had to do all
the FFI myself.
-}
module System.Console.ANSI.Windows.Foreign
  (
    -- Re-exports from Win32.Types
    BOOL, WORD, DWORD, WCHAR, HANDLE, iNVALID_HANDLE_VALUE, nullHANDLE, SHORT,

    -- 'Re-exports from System.Win32.Console.Extra'
    INPUT_RECORD (..), INPUT_RECORD_EVENT (..), kEY_EVENT,
    KEY_EVENT_RECORD (..), UNICODE_ASCII_CHAR (..), writeConsoleInput,
    getNumberOfConsoleInputEvents, readConsoleInput,

    charToWCHAR, cWcharsToChars,

    COORD(..), SMALL_RECT(..), rect_top, rect_bottom, rect_left, rect_right,
    rect_width, rect_height, CONSOLE_CURSOR_INFO(..),
    CONSOLE_SCREEN_BUFFER_INFO(..), CHAR_INFO(..),

    sTD_INPUT_HANDLE, sTD_OUTPUT_HANDLE, sTD_ERROR_HANDLE,

    eNABLE_VIRTUAL_TERMINAL_INPUT, eNABLE_VIRTUAL_TERMINAL_PROCESSING,

    fOREGROUND_BLUE, fOREGROUND_GREEN, fOREGROUND_RED, fOREGROUND_INTENSITY,
    fOREGROUND_WHITE, fOREGROUND_INTENSE_WHITE,
    bACKGROUND_BLUE, bACKGROUND_GREEN, bACKGROUND_RED, bACKGROUND_INTENSITY,
    bACKGROUND_WHITE, bACKGROUND_INTENSE_WHITE,
    cOMMON_LVB_REVERSE_VIDEO, cOMMON_LVB_UNDERSCORE,

    getStdHandle,
    getConsoleScreenBufferInfo,
    getConsoleCursorInfo,
    getConsoleMode,

    setConsoleTextAttribute,
    setConsoleCursorPosition,
    setConsoleCursorInfo,
    setConsoleTitle,
    setConsoleMode,

    fillConsoleOutputAttribute,
    fillConsoleOutputCharacter,
    scrollConsoleScreenBuffer,

    withTString, withHandleToHANDLE,

    ConsoleException (..)
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
import Control.Exception (Exception, throw)
import Data.Bits ((.|.), shiftL)
import Data.Char (chr, ord)
import Data.Typeable (Typeable)
import Foreign.C.Types (CInt (..), CWchar (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray, withArrayLen)
import Foreign.Marshal.Utils (maybeWith, with)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable (..))
-- `SHORT` and `withHandleToHANDLE` are not both available before Win32-2.5.1.0
import System.Win32.Compat (BOOL, DWORD, ErrCode, HANDLE, LPCTSTR, LPDWORD,
  SHORT, TCHAR, UINT, WORD, failIfFalse_, getLastError, iNVALID_HANDLE_VALUE,
  nullHANDLE, withHandleToHANDLE, withTString)

#if defined(i386_HOST_ARCH)
#define WINDOWS_CCONV stdcall
#elif defined(x86_64_HOST_ARCH)
#define WINDOWS_CCONV ccall
#else
#error Unknown mingw32 arch
#endif

type WCHAR = CWchar

charToWCHAR :: Char -> WCHAR
charToWCHAR char = fromIntegral (ord char)

-- This is a FFI hack. Some of the API calls take a Coord, but that isn't a
-- built-in FFI type so I can't use it directly. Instead, I use UNPACKED_COORD
-- and marshal COORDs into this manually. Note that we CAN'T just use two SHORTs
-- directly because they get expanded to 4 bytes each instead of just boing 2
-- lots of 2 bytes by the stdcall convention, so linking fails.
type UNPACKED_COORD = CInt

-- Field packing order determined experimentally: I couldn't immediately find a
-- specification for Windows struct layout anywhere.
unpackCOORD :: COORD -> UNPACKED_COORD
unpackCOORD (COORD x y)
  = (fromIntegral y) `shiftL` (sizeOf x * 8) .|. (fromIntegral x)


peekAndOffset :: Storable a => Ptr a -> IO (a, Ptr b)
peekAndOffset ptr = do
  item <- peek ptr
  return (item, ptr `plusPtr` sizeOf item)

pokeAndOffset :: Storable a => Ptr a -> a -> IO (Ptr b)
pokeAndOffset ptr item = do
  poke ptr item
  return (ptr `plusPtr` sizeOf item)

data COORD = COORD
  { coord_x :: SHORT
  , coord_y :: SHORT
  } deriving (Read, Eq)

instance Show COORD where
  show (COORD x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

instance Storable COORD where
  sizeOf ~(COORD x y) = sizeOf x + sizeOf y
  alignment ~(COORD x _) = alignment x
  peek ptr = do
    let ptr' = castPtr ptr :: Ptr SHORT
    x <- peekElemOff ptr' 0
    y <- peekElemOff ptr' 1
    return (COORD x y)
  poke ptr (COORD x y) = do
    let ptr' = castPtr ptr :: Ptr SHORT
    pokeElemOff ptr' 0 x
    pokeElemOff ptr' 1 y

data SMALL_RECT = SMALL_RECT
  { rect_top_left     :: COORD
  , rect_bottom_right :: COORD
  }

rect_top, rect_left, rect_bottom, rect_right :: SMALL_RECT -> SHORT
rect_top = coord_y . rect_top_left
rect_left = coord_x . rect_top_left
rect_bottom = coord_y . rect_bottom_right
rect_right = coord_x . rect_bottom_right

rect_width, rect_height :: SMALL_RECT -> SHORT
rect_width rect = rect_right rect - rect_left rect + 1
rect_height rect = rect_bottom rect - rect_top rect + 1

instance Show SMALL_RECT where
  show (SMALL_RECT tl br) = show tl ++ "-" ++ show br

instance Storable SMALL_RECT where
  sizeOf ~(SMALL_RECT tl br) = sizeOf tl + sizeOf br
  alignment ~(SMALL_RECT tl _) = alignment tl
  peek ptr = do
    let ptr' = castPtr ptr :: Ptr COORD
    tl <- peekElemOff ptr' 0
    br <- peekElemOff ptr' 1
    return (SMALL_RECT tl br)
  poke ptr (SMALL_RECT tl br) = do
    let ptr' = castPtr ptr :: Ptr COORD
    pokeElemOff ptr' 0 tl
    pokeElemOff ptr' 1 br

data CONSOLE_CURSOR_INFO = CONSOLE_CURSOR_INFO
  { cci_cursor_size    :: DWORD
  , cci_cursor_visible :: BOOL
  } deriving (Show)

instance Storable CONSOLE_CURSOR_INFO where
  sizeOf ~(CONSOLE_CURSOR_INFO size visible) = sizeOf size + sizeOf visible
  alignment ~(CONSOLE_CURSOR_INFO size _) = alignment size
  peek ptr = do
    (size, ptr') <- peekAndOffset (castPtr ptr)
    visible <- peek ptr'
    return (CONSOLE_CURSOR_INFO size visible)
  poke ptr (CONSOLE_CURSOR_INFO size visible) = do
    ptr' <- pokeAndOffset (castPtr ptr) size
    poke ptr' visible

data CONSOLE_SCREEN_BUFFER_INFO = CONSOLE_SCREEN_BUFFER_INFO
  { csbi_size                :: COORD
  , csbi_cursor_position     :: COORD
  , csbi_attributes          :: WORD
  , csbi_window              :: SMALL_RECT
  , csbi_maximum_window_size :: COORD
  } deriving (Show)

instance Storable CONSOLE_SCREEN_BUFFER_INFO where
  sizeOf ~(CONSOLE_SCREEN_BUFFER_INFO
    size cursor_position attributes window maximum_window_size)
    = sizeOf size + sizeOf cursor_position + sizeOf attributes + sizeOf window
        + sizeOf maximum_window_size
  alignment ~(CONSOLE_SCREEN_BUFFER_INFO size _ _ _ _) = alignment size
  peek ptr = do
    (size, ptr1) <- peekAndOffset (castPtr ptr)
    (cursor_position, ptr2) <- peekAndOffset ptr1
    (attributes, ptr3) <- peekAndOffset ptr2
    (window, ptr4) <- peekAndOffset ptr3
    maximum_window_size <- peek ptr4
    return (CONSOLE_SCREEN_BUFFER_INFO
      size cursor_position attributes window maximum_window_size)
  poke ptr (CONSOLE_SCREEN_BUFFER_INFO
    size cursor_position attributes window maximum_window_size)
    = do
      ptr1 <- pokeAndOffset (castPtr ptr) size
      ptr2 <- pokeAndOffset ptr1 cursor_position
      ptr3 <- pokeAndOffset ptr2 attributes
      ptr4 <- pokeAndOffset ptr3 window
      poke ptr4 maximum_window_size

data CHAR_INFO = CHAR_INFO
  { ci_char       :: WCHAR
  , ci_attributes :: WORD
  } deriving (Show)

instance Storable CHAR_INFO where
  sizeOf ~(CHAR_INFO char attributes) = sizeOf char + sizeOf attributes
  alignment ~(CHAR_INFO char _) = alignment char
  peek ptr = do
    (char, ptr') <- peekAndOffset (castPtr ptr)
    attributes <- peek ptr'
    return (CHAR_INFO char attributes)
  poke ptr (CHAR_INFO char attributes) = do
    ptr' <- pokeAndOffset (castPtr ptr) char
    poke ptr' attributes

eNABLE_VIRTUAL_TERMINAL_INPUT, eNABLE_VIRTUAL_TERMINAL_PROCESSING :: DWORD
sTD_INPUT_HANDLE, sTD_OUTPUT_HANDLE, sTD_ERROR_HANDLE :: DWORD
eNABLE_VIRTUAL_TERMINAL_INPUT      = 512
eNABLE_VIRTUAL_TERMINAL_PROCESSING =   4
sTD_INPUT_HANDLE  = 0xFFFFFFF6 -- minus 10
sTD_OUTPUT_HANDLE = 0xFFFFFFF5 -- minus 11
sTD_ERROR_HANDLE  = 0xFFFFFFF4 -- minus 12

fOREGROUND_BLUE, fOREGROUND_GREEN, fOREGROUND_RED, fOREGROUND_INTENSITY,
  bACKGROUND_BLUE, bACKGROUND_GREEN, bACKGROUND_RED, bACKGROUND_INTENSITY,
  cOMMON_LVB_REVERSE_VIDEO, cOMMON_LVB_UNDERSCORE :: WORD
fOREGROUND_BLUE          =    0x1
fOREGROUND_GREEN         =    0x2
fOREGROUND_RED           =    0x4
fOREGROUND_INTENSITY     =    0x8
bACKGROUND_BLUE          =   0x10
bACKGROUND_GREEN         =   0x20
bACKGROUND_RED           =   0x40
bACKGROUND_INTENSITY     =   0x80
cOMMON_LVB_REVERSE_VIDEO = 0x4000
cOMMON_LVB_UNDERSCORE    = 0x8000

fOREGROUND_WHITE, bACKGROUND_WHITE, fOREGROUND_INTENSE_WHITE,
  bACKGROUND_INTENSE_WHITE :: WORD
fOREGROUND_WHITE = fOREGROUND_RED .|. fOREGROUND_GREEN .|. fOREGROUND_BLUE
bACKGROUND_WHITE = bACKGROUND_RED .|. bACKGROUND_GREEN .|. bACKGROUND_BLUE
fOREGROUND_INTENSE_WHITE = fOREGROUND_WHITE .|. fOREGROUND_INTENSITY
bACKGROUND_INTENSE_WHITE = bACKGROUND_WHITE .|. bACKGROUND_INTENSITY

kEY_EVENT, mOUSE_EVENT, wINDOW_BUFFER_SIZE_EVENT, mENU_EVENT,
  fOCUS_EVENT :: WORD
kEY_EVENT                =  1
mOUSE_EVENT              =  2
wINDOW_BUFFER_SIZE_EVENT =  4
mENU_EVENT               =  8
fOCUS_EVENT              = 16

foreign import WINDOWS_CCONV unsafe "windows.h GetStdHandle"
  getStdHandle :: DWORD -> IO HANDLE
foreign import WINDOWS_CCONV unsafe "windows.h GetConsoleScreenBufferInfo"
  cGetConsoleScreenBufferInfo :: HANDLE
                              -> Ptr CONSOLE_SCREEN_BUFFER_INFO
                              -> IO BOOL
foreign import WINDOWS_CCONV unsafe "windows.h GetConsoleCursorInfo"
  cGetConsoleCursorInfo :: HANDLE -> Ptr CONSOLE_CURSOR_INFO -> IO BOOL
foreign import WINDOWS_CCONV unsafe "windows.h GetConsoleMode"
  cGetConsoleMode :: HANDLE -> Ptr DWORD -> IO BOOL
foreign import WINDOWS_CCONV unsafe "windows.h SetConsoleTextAttribute"
  cSetConsoleTextAttribute :: HANDLE -> WORD -> IO BOOL
foreign import WINDOWS_CCONV unsafe "windows.h SetConsoleCursorPosition"
  cSetConsoleCursorPosition :: HANDLE -> UNPACKED_COORD -> IO BOOL
foreign import WINDOWS_CCONV unsafe "windows.h SetConsoleCursorInfo"
  cSetConsoleCursorInfo :: HANDLE -> Ptr CONSOLE_CURSOR_INFO -> IO BOOL
foreign import WINDOWS_CCONV unsafe "windows.h SetConsoleTitleW"
  cSetConsoleTitle :: LPCTSTR -> IO BOOL
foreign import WINDOWS_CCONV unsafe "windows.h SetConsoleMode"
  cSetConsoleMode :: HANDLE -> DWORD -> IO BOOL
foreign import WINDOWS_CCONV unsafe "windows.h FillConsoleOutputAttribute"
  cFillConsoleOutputAttribute :: HANDLE
                              -> WORD
                              -> DWORD
                              -> UNPACKED_COORD
                              -> Ptr DWORD
                              -> IO BOOL
foreign import WINDOWS_CCONV unsafe "windows.h FillConsoleOutputCharacterW"
  cFillConsoleOutputCharacter :: HANDLE
                              -> TCHAR
                              -> DWORD
                              -> UNPACKED_COORD
                              -> Ptr DWORD
                              -> IO BOOL
foreign import WINDOWS_CCONV unsafe "windows.h ScrollConsoleScreenBufferW"
  cScrollConsoleScreenBuffer :: HANDLE
                             -> Ptr SMALL_RECT
                             -> Ptr SMALL_RECT
                             -> UNPACKED_COORD
                             -> Ptr CHAR_INFO
                             -> IO BOOL
foreign import WINDOWS_CCONV unsafe "windows.h WriteConsoleInputW"
  cWriteConsoleInput :: HANDLE
                     -> Ptr INPUT_RECORD
                     -> DWORD
                     -> LPDWORD
                     -> IO BOOL
foreign import WINDOWS_CCONV unsafe "windows.h GetNumberOfConsoleInputEvents"
  cGetNumberOfConsoleInputEvents :: HANDLE -> Ptr DWORD -> IO BOOL
foreign import WINDOWS_CCONV unsafe "windows.h ReadConsoleInputW"
  cReadConsoleInput :: HANDLE
                    -> Ptr INPUT_RECORD
                    -> DWORD
                    -> LPDWORD
                    -> IO BOOL

data ConsoleException = ConsoleException !ErrCode deriving (Eq, Typeable)

instance Show ConsoleException where
  show (ConsoleException 6) =
    "A fatal error has occurred.\n\n" ++
    "An attempt has been made to send console virtual terminal sequences\n" ++
    "(ANSI codes) to an output that has not been recognised as an\n" ++
    "ANSI-capable terminal and also cannot be emulated as an ANSI-enabled\n" ++
    "terminal (emulation needs a ConHost-based terminal, such as Command\n" ++
    "Prompt or PowerShell). That may occur, for example, if output has\n" ++
    "been redirected to a file.\n\n" ++
    "If that is unexpected, please post an issue at:\n" ++
    "https://github.com/feuerbach/ansi-terminal/issues\n"
  show (ConsoleException errCode) = "ConsoleException " ++ show errCode

instance Exception ConsoleException

throwIfFalse :: IO Bool -> IO ()
throwIfFalse action = do
  succeeded <- action
  if not succeeded
    then getLastError >>= throw . ConsoleException -- TODO: Check if last error
    -- is zero for some instructable reason (?)
    else return ()

getConsoleScreenBufferInfo :: HANDLE -> IO CONSOLE_SCREEN_BUFFER_INFO
getConsoleScreenBufferInfo handle
  = alloca $ \ptr_console_screen_buffer_info -> do
      throwIfFalse $
        cGetConsoleScreenBufferInfo handle ptr_console_screen_buffer_info
      peek ptr_console_screen_buffer_info

getConsoleCursorInfo :: HANDLE -> IO CONSOLE_CURSOR_INFO
getConsoleCursorInfo handle = alloca $ \ptr_console_cursor_info -> do
  throwIfFalse $ cGetConsoleCursorInfo handle ptr_console_cursor_info
  peek ptr_console_cursor_info

getConsoleMode :: HANDLE -> IO DWORD
getConsoleMode handle = alloca $ \ptr_mode -> do
  throwIfFalse $ cGetConsoleMode handle ptr_mode
  peek ptr_mode

setConsoleTextAttribute :: HANDLE -> WORD -> IO ()
setConsoleTextAttribute handle attributes
  = throwIfFalse $ cSetConsoleTextAttribute handle attributes

setConsoleCursorPosition :: HANDLE -> COORD -> IO ()
setConsoleCursorPosition handle cursor_position
  = throwIfFalse $ cSetConsoleCursorPosition handle
      (unpackCOORD cursor_position)

setConsoleCursorInfo :: HANDLE -> CONSOLE_CURSOR_INFO -> IO ()
setConsoleCursorInfo handle console_cursor_info
  = with console_cursor_info $ \ptr_console_cursor_info -> do
      throwIfFalse $ cSetConsoleCursorInfo handle ptr_console_cursor_info

setConsoleTitle :: LPCTSTR -> IO ()
setConsoleTitle title = throwIfFalse $ cSetConsoleTitle title

setConsoleMode :: HANDLE -> DWORD -> IO ()
setConsoleMode handle attributes
  = throwIfFalse $ cSetConsoleMode handle attributes

fillConsoleOutputAttribute :: HANDLE -> WORD -> DWORD -> COORD -> IO DWORD
fillConsoleOutputAttribute handle attribute fill_length write_origin
  = alloca $ \ptr_chars_written -> do
      throwIfFalse $ cFillConsoleOutputAttribute handle attribute
        fill_length (unpackCOORD write_origin) ptr_chars_written
      peek ptr_chars_written

fillConsoleOutputCharacter :: HANDLE -> TCHAR -> DWORD -> COORD -> IO DWORD
fillConsoleOutputCharacter handle char fill_length write_origin
  = alloca $ \ptr_chars_written -> do
      throwIfFalse $ cFillConsoleOutputCharacter handle char fill_length
        (unpackCOORD write_origin) ptr_chars_written
      peek ptr_chars_written

scrollConsoleScreenBuffer :: HANDLE
                          -> SMALL_RECT
                          -> Maybe SMALL_RECT
                          -> COORD
                          -> CHAR_INFO
                          -> IO ()
scrollConsoleScreenBuffer
  handle scroll_rectangle mb_clip_rectangle destination_origin fill
  = with scroll_rectangle $ \ptr_scroll_rectangle ->
    maybeWith with mb_clip_rectangle $ \ptr_clip_rectangle ->
    with fill $ \ptr_fill ->
    throwIfFalse $ cScrollConsoleScreenBuffer handle ptr_scroll_rectangle
      ptr_clip_rectangle (unpackCOORD destination_origin) ptr_fill

-- The following is based on module System.Win32.Console.Extra from package
-- Win32-console, cut down for the WCHAR version of writeConsoleInput.

writeConsoleInput :: HANDLE -> [INPUT_RECORD] -> IO DWORD
writeConsoleInput hdl evs
  = writeConsoleInputWith hdl $ \act ->
    withArrayLen evs $ \len ptr ->
    act (ptr, toEnum len)

writeConsoleInputWith :: HANDLE
                      -> InputHandler (Ptr INPUT_RECORD, DWORD)
                      -> IO DWORD
writeConsoleInputWith hdl withBuffer
  = returnWith_ $ \ptrN ->
    withBuffer $ \(ptrBuf, len) ->
    failIfFalse_ "WriteConsoleInputW" $ cWriteConsoleInput hdl ptrBuf len ptrN

returnWith_ :: Storable a => (Ptr a -> IO b) -> IO a
returnWith_ act = alloca $ \ptr -> act ptr >> peek ptr

type InputHandler i = forall a. (i -> IO a) -> IO a

{-
typedef union _UNICODE_ASCII_CHAR {
    WCHAR UnicodeChar;
    CHAR  AsciiChar;
} UNICODE_ASCII_CHAR;
-}
newtype UNICODE_ASCII_CHAR = UnicodeAsciiChar
  { unicodeAsciiChar :: WCHAR
  } deriving (Show, Read, Eq)

instance Storable UNICODE_ASCII_CHAR where
  sizeOf _    = 2
  alignment _ = 2
  peek ptr = UnicodeAsciiChar <$> (`peekByteOff` 0) ptr
  poke ptr val = case val of
    UnicodeAsciiChar c -> (`pokeByteOff` 0) ptr c

{-
typedef struct _KEY_EVENT_RECORD {
	BOOL bKeyDown;
	WORD wRepeatCount;
	WORD wVirtualKeyCode;
	WORD wVirtualScanCode;
	union {
		WCHAR UnicodeChar;
		CHAR AsciiChar;
	} uChar;
	DWORD dwControlKeyState;
}
#ifdef __GNUC__
/* gcc's alignment is not what win32 expects */
 PACKED
#endif
KEY_EVENT_RECORD;
-}
data KEY_EVENT_RECORD = KEY_EVENT_RECORD
  { keyEventKeyDown         :: BOOL
  , keyEventRepeatCount     :: WORD
  , keyEventVirtualKeyCode  :: WORD
  , keyEventVirtualScanCode :: WORD
  , keyEventChar            :: UNICODE_ASCII_CHAR
  , keyEventControlKeystate :: DWORD
  } deriving (Show, Read, Eq)

instance Storable KEY_EVENT_RECORD where
  sizeOf _    = 16
  alignment _ =  4
  peek ptr = KEY_EVENT_RECORD <$> (`peekByteOff`  0) ptr
                              <*> (`peekByteOff`  4) ptr
                              <*> (`peekByteOff`  6) ptr
                              <*> (`peekByteOff`  8) ptr
                              <*> (`peekByteOff` 10) ptr
                              <*> (`peekByteOff` 12) ptr
  poke ptr val = do
    (`pokeByteOff`  0) ptr $ keyEventKeyDown val
    (`pokeByteOff`  4) ptr $ keyEventRepeatCount val
    (`pokeByteOff`  6) ptr $ keyEventVirtualKeyCode val
    (`pokeByteOff`  8) ptr $ keyEventVirtualScanCode val
    (`pokeByteOff` 10) ptr $ keyEventChar val
    (`pokeByteOff` 12) ptr $ keyEventControlKeystate val

{-
typedef struct _MOUSE_EVENT_RECORD {
	COORD dwMousePosition;
	DWORD dwButtonState;
	DWORD dwControlKeyState;
	DWORD dwEventFlags;
} MOUSE_EVENT_RECORD;
-}
data MOUSE_EVENT_RECORD = MOUSE_EVENT_RECORD
  { mousePosition        :: COORD
  , mouseButtonState     :: DWORD
  , mouseControlKeyState :: DWORD
  , mouseEventFlags      :: DWORD
  } deriving (Show, Read, Eq)

instance Storable MOUSE_EVENT_RECORD where
  sizeOf _    = 16
  alignment _ =  4
  peek ptr = MOUSE_EVENT_RECORD <$> (`peekByteOff`  0) ptr
                                <*> (`peekByteOff`  4) ptr
                                <*> (`peekByteOff`  8) ptr
                                <*> (`peekByteOff` 12) ptr
  poke ptr val = do
    (`pokeByteOff`  0) ptr $ mousePosition val
    (`pokeByteOff`  4) ptr $ mouseButtonState val
    (`pokeByteOff`  8) ptr $ mouseControlKeyState val
    (`pokeByteOff` 12) ptr $ mouseEventFlags val

{-
typedef struct _WINDOW_BUFFER_SIZE_RECORD {
    COORD dwSize;
} WINDOW_BUFFER_SIZE_RECORD;
-}
data WINDOW_BUFFER_SIZE_RECORD = WINDOW_BUFFER_SIZE_RECORD
  { bufSizeNew :: COORD
  } deriving (Show, Read, Eq)

instance Storable WINDOW_BUFFER_SIZE_RECORD where
  sizeOf _    = 4
  alignment _ = 4
  peek ptr = WINDOW_BUFFER_SIZE_RECORD <$> (`peekByteOff` 0) ptr
  poke ptr val = (`pokeByteOff` 0) ptr $ bufSizeNew val

{-
typedef struct _MENU_EVENT_RECORD {
    UINT dwCommandId;
} MENU_EVENT_RECORD,*PMENU_EVENT_RECORD;
-}
data MENU_EVENT_RECORD = MENU_EVENT_RECORD
  { menuCommandId :: UINT
  } deriving (Show, Read, Eq)

instance Storable MENU_EVENT_RECORD where
  sizeOf _    = 4
  alignment _ = 4
  peek ptr = MENU_EVENT_RECORD <$> (`peekByteOff` 0) ptr
  poke ptr val = (`pokeByteOff` 0) ptr $ menuCommandId val

{-
typedef struct _FOCUS_EVENT_RECORD { BOOL bSetFocus; } FOCUS_EVENT_RECORD;
-}
data FOCUS_EVENT_RECORD = FOCUS_EVENT_RECORD
  { focusSetFocus :: BOOL
  } deriving (Show, Read, Eq)

instance Storable FOCUS_EVENT_RECORD where
  sizeOf _    = 4
  alignment _ = 4
  peek ptr = FOCUS_EVENT_RECORD <$> (`peekByteOff` 0) ptr
  poke ptr val = (`pokeByteOff` 0) ptr $ focusSetFocus val

data INPUT_RECORD_EVENT
  = InputKeyEvent KEY_EVENT_RECORD
  | InputMouseEvent MOUSE_EVENT_RECORD
  | InputWindowBufferSizeEvent WINDOW_BUFFER_SIZE_RECORD
  | InputMenuEvent MENU_EVENT_RECORD
  | InputFocusEvent FOCUS_EVENT_RECORD
  deriving (Show, Read, Eq)

{-
typedef struct _INPUT_RECORD {
	WORD EventType;
	union {
		KEY_EVENT_RECORD KeyEvent;
		MOUSE_EVENT_RECORD MouseEvent;
		WINDOW_BUFFER_SIZE_RECORD WindowBufferSizeEvent;
		MENU_EVENT_RECORD MenuEvent;
		FOCUS_EVENT_RECORD FocusEvent;
	} Event;
} INPUT_RECORD,*PINPUT_RECORD;
-}
data INPUT_RECORD = INPUT_RECORD
  { inputEventType :: WORD
  , inputEvent     :: INPUT_RECORD_EVENT
  } deriving (Show, Read, Eq)

instance Storable INPUT_RECORD where
  sizeOf _    = 20
  alignment _ =  4
  peek ptr = do
    evType <- (`peekByteOff` 0) ptr
    event <- case evType of
      _ | evType == kEY_EVENT
          -> InputKeyEvent              <$> (`peekByteOff` 4) ptr
      _ | evType == mOUSE_EVENT
          -> InputMouseEvent            <$> (`peekByteOff` 4) ptr
      _ | evType == wINDOW_BUFFER_SIZE_EVENT
          -> InputWindowBufferSizeEvent <$> (`peekByteOff` 4) ptr
      _ | evType == mENU_EVENT
          -> InputMenuEvent             <$> (`peekByteOff` 4) ptr
      _ | evType == fOCUS_EVENT
          -> InputFocusEvent            <$> (`peekByteOff` 4) ptr
      _ -> error $ "peek (INPUT_RECORD): Unknown event type " ++
             show evType
    return $ INPUT_RECORD evType event
  poke ptr val = do
    (`pokeByteOff` 0) ptr $ inputEventType val
    case inputEvent val of
      InputKeyEvent              ev -> (`pokeByteOff` 4) ptr ev
      InputMouseEvent            ev -> (`pokeByteOff` 4) ptr ev
      InputWindowBufferSizeEvent ev -> (`pokeByteOff` 4) ptr ev
      InputMenuEvent             ev -> (`pokeByteOff` 4) ptr ev
      InputFocusEvent            ev -> (`pokeByteOff` 4) ptr ev

-- The following is based on module System.Win32.Console.Extra from package
-- Win32-console.

getNumberOfConsoleInputEvents :: HANDLE -> IO DWORD
getNumberOfConsoleInputEvents hdl =
  returnWith_ $ \ptrN ->
    failIfFalse_ "GetNumberOfConsoleInputEvents" $
      cGetNumberOfConsoleInputEvents hdl ptrN

-- The following is based on module System.Win32.Console.Extra from package
-- Win32-console, cut down for the WCHAR version of readConsoleInput.

readConsoleInput :: HANDLE -> DWORD -> IO [INPUT_RECORD]
readConsoleInput hdl len
  = readConsoleInputWith hdl len $ \(ptr, n) -> peekArray (fromEnum n) ptr

readConsoleInputWith :: HANDLE
                     -> DWORD
                     -> OutputHandler (Ptr INPUT_RECORD, DWORD)
readConsoleInputWith hdl len handler =
  allocaArray (fromEnum len) $ \ptrBuf ->
    alloca $ \ptrN -> do
      failIfFalse_ "ReadConsoleInputW" $
        cReadConsoleInput hdl ptrBuf len ptrN
      n <- peek ptrN
      handler (ptrBuf, n)

type OutputHandler o = forall a. (o -> IO a) -> IO a

-- Replicated from module Foreign.C.String in package base because that module
-- does not export the function.
cWcharsToChars :: [CWchar] -> [Char]
cWcharsToChars = map chr . fromUTF16 . map fromIntegral
 where
  fromUTF16 (c1:c2:wcs)
    | 0xd800 <= c1 && c1 <= 0xdbff && 0xdc00 <= c2 && c2 <= 0xdfff =
      ((c1 - 0xd800)*0x400 + (c2 - 0xdc00) + 0x10000) : fromUTF16 wcs
  fromUTF16 (c:wcs) = c : fromUTF16 wcs
  fromUTF16 [] = []
