module System.Console.Haskeline.Backend.Win32(
                win32Term,
                win32TermStdin,
                fileRunTerm
                )where


import System.IO
import Foreign
import Foreign.C
import System.Win32 hiding (multiByteToWideChar)
import Graphics.Win32.Misc(getStdHandle, sTD_OUTPUT_HANDLE)
import Data.List(intercalate)
import Control.Concurrent.STM
import Control.Concurrent hiding (throwTo)
import Data.Char(isPrint)
import Data.Maybe(mapMaybe)
import Control.Applicative
import Control.Monad

import System.Console.Haskeline.Key
import System.Console.Haskeline.Monads hiding (Handler)
import System.Console.Haskeline.LineState
import System.Console.Haskeline.Term
import System.Console.Haskeline.Backend.WCWidth
import System.Console.Haskeline.Backend.Win32.Echo (hWithoutInputEcho)

import Data.ByteString.Internal (createAndTrim)
import qualified Data.ByteString as B

#include "win_console.h"
##include "windows_cconv.h"

foreign import WINDOWS_CCONV "windows.h ReadConsoleInputW" c_ReadConsoleInput
    :: HANDLE -> Ptr () -> DWORD -> Ptr DWORD -> IO Bool

foreign import WINDOWS_CCONV "windows.h WaitForSingleObject" c_WaitForSingleObject
    :: HANDLE -> DWORD -> IO DWORD

foreign import WINDOWS_CCONV "windows.h GetNumberOfConsoleInputEvents"
    c_GetNumberOfConsoleInputEvents :: HANDLE -> Ptr DWORD -> IO Bool

getNumberOfEvents :: HANDLE -> IO Int
getNumberOfEvents h = alloca $ \numEventsPtr -> do
    failIfFalse_ "GetNumberOfConsoleInputEvents"
        $ c_GetNumberOfConsoleInputEvents h numEventsPtr
    fmap fromEnum $ peek numEventsPtr

getEvent :: HANDLE -> TChan Event -> IO Event
getEvent h = keyEventLoop (eventReader h)

eventReader :: HANDLE -> IO [Event]
eventReader h = do
    let waitTime = 500 -- milliseconds
    ret <- c_WaitForSingleObject h waitTime
    yield -- otherwise, the above foreign call causes the loop to never
          -- respond to the killThread
    if ret /= (#const WAIT_OBJECT_0)
        then eventReader h
        else do
            es <- readEvents h
            return $ mapMaybe processEvent es

consoleHandles :: MaybeT IO Handles
consoleHandles = do
    h_in <- open "CONIN$"
    h_out <- open "CONOUT$"
    return Handles { hIn = h_in, hOut = h_out }
  where
   open file = handle (\(_::IOException) -> mzero) $ liftIO
                $ createFile file (gENERIC_READ .|. gENERIC_WRITE)
                        (fILE_SHARE_READ .|. fILE_SHARE_WRITE) Nothing
                        oPEN_EXISTING 0 Nothing


processEvent :: InputEvent -> Maybe Event
processEvent KeyEvent {keyDown = kd, unicodeChar = c, virtualKeyCode = vc,
                    controlKeyState = cstate}
    | kd || ((testMod (#const LEFT_ALT_PRESSED) || vc == (#const VK_MENU))
             && c /= '\NUL')
      -- Make sure not to ignore Unicode key events! The Unicode character might
      -- only be emitted on a keyup event. See also GH issue #54.
    = fmap (\e -> KeyInput [Key modifier' e]) $ keyFromCode vc `mplus` simpleKeyChar
  where
    simpleKeyChar = guard (c /= '\NUL') >> return (KeyChar c)
    testMod ck = (cstate .&. ck) /= 0
    modifier' = if hasMeta modifier && hasControl modifier
                    then noModifier {hasShift = hasShift modifier}
                    else modifier
    modifier = Modifier {hasMeta = testMod ((#const RIGHT_ALT_PRESSED)
                                        .|. (#const LEFT_ALT_PRESSED))
                        ,hasControl = testMod ((#const RIGHT_CTRL_PRESSED)
                                        .|. (#const LEFT_CTRL_PRESSED))
                                    && not (c > '\NUL' && c <= '\031')
                        ,hasShift = testMod (#const SHIFT_PRESSED)
                                    && not (isPrint c)
                        }

processEvent WindowEvent = Just WindowResize
processEvent _ = Nothing

keyFromCode :: WORD -> Maybe BaseKey
keyFromCode (#const VK_BACK) = Just Backspace
keyFromCode (#const VK_LEFT) = Just LeftKey
keyFromCode (#const VK_RIGHT) = Just RightKey
keyFromCode (#const VK_UP) = Just UpKey
keyFromCode (#const VK_DOWN) = Just DownKey
keyFromCode (#const VK_DELETE) = Just Delete
keyFromCode (#const VK_HOME) = Just Home
keyFromCode (#const VK_END) = Just End
keyFromCode (#const VK_PRIOR) = Just PageUp
keyFromCode (#const VK_NEXT) = Just PageDown
-- The Windows console will return '\r' when return is pressed.
keyFromCode (#const VK_RETURN) = Just (KeyChar '\n')
-- TODO: KillLine?
-- TODO: function keys.
keyFromCode _ = Nothing

data InputEvent = KeyEvent {keyDown :: BOOL,
                          repeatCount :: WORD,
                          virtualKeyCode :: WORD,
                          virtualScanCode :: WORD,
                          unicodeChar :: Char,
                          controlKeyState :: DWORD}
            -- TODO: WINDOW_BUFFER_SIZE_RECORD
            -- I cant figure out how the user generates them.
           | WindowEvent
           | OtherEvent
                        deriving Show

peekEvent :: Ptr () -> IO InputEvent
peekEvent pRecord = do
    eventType :: WORD <- (#peek INPUT_RECORD, EventType) pRecord
    let eventPtr = (#ptr INPUT_RECORD, Event) pRecord
    case eventType of
        (#const KEY_EVENT) -> getKeyEvent eventPtr
        (#const WINDOW_BUFFER_SIZE_EVENT) -> return WindowEvent
        _ -> return OtherEvent

readEvents :: HANDLE -> IO [InputEvent]
readEvents h = do
    n <- getNumberOfEvents h
    alloca $ \numEventsPtr ->
        allocaBytes (n * #size INPUT_RECORD) $ \pRecord -> do
            failIfFalse_ "ReadConsoleInput"
                $ c_ReadConsoleInput h pRecord (toEnum n) numEventsPtr
            numRead <- fmap fromEnum $ peek numEventsPtr
            forM [0..toEnum numRead-1] $ \i -> peekEvent
                $ pRecord `plusPtr` (i * #size INPUT_RECORD)

getKeyEvent :: Ptr () -> IO InputEvent
getKeyEvent p = do
    kDown' <- (#peek KEY_EVENT_RECORD, bKeyDown) p
    repeat' <- (#peek KEY_EVENT_RECORD, wRepeatCount) p
    keyCode <- (#peek KEY_EVENT_RECORD, wVirtualKeyCode) p
    scanCode <- (#peek KEY_EVENT_RECORD, wVirtualScanCode) p
    char :: CWchar <- (#peek KEY_EVENT_RECORD, uChar) p
    state <- (#peek KEY_EVENT_RECORD, dwControlKeyState) p
    return KeyEvent {keyDown = kDown',
                            repeatCount = repeat',
                            virtualKeyCode = keyCode,
                            virtualScanCode = scanCode,
                            unicodeChar = toEnum (fromEnum char),
                            controlKeyState = state}

data Coord = Coord {coordX, coordY :: Int}
                deriving Show

#if __GLASGOW_HASKELL__ < 711
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif
instance Storable Coord where
    sizeOf _ = (#size COORD)
    alignment _ = (#alignment COORD)
    peek p = do
        x :: CShort <- (#peek COORD, X) p
        y :: CShort <- (#peek COORD, Y) p
        return Coord {coordX = fromEnum x, coordY = fromEnum y}
    poke p c = do
        (#poke COORD, X) p (toEnum (coordX c) :: CShort)
        (#poke COORD, Y) p (toEnum (coordY c) :: CShort)


foreign import ccall "haskeline_SetPosition"
    c_SetPosition :: HANDLE -> Ptr Coord -> IO Bool

setPosition :: HANDLE -> Coord -> IO ()
setPosition h c = with c $ failIfFalse_ "SetConsoleCursorPosition"
                    . c_SetPosition h

foreign import WINDOWS_CCONV "windows.h GetConsoleScreenBufferInfo"
    c_GetScreenBufferInfo :: HANDLE -> Ptr () -> IO Bool

getPosition :: HANDLE -> IO Coord
getPosition = withScreenBufferInfo $
    (#peek CONSOLE_SCREEN_BUFFER_INFO, dwCursorPosition)

withScreenBufferInfo :: (Ptr () -> IO a) -> HANDLE -> IO a
withScreenBufferInfo f h = allocaBytes (#size CONSOLE_SCREEN_BUFFER_INFO)
                                $ \infoPtr -> do
        failIfFalse_ "GetConsoleScreenBufferInfo"
            $ c_GetScreenBufferInfo h infoPtr
        f infoPtr

getBufferSize :: HANDLE -> IO Layout
getBufferSize = withScreenBufferInfo $ \p -> do
    c <- (#peek CONSOLE_SCREEN_BUFFER_INFO, dwSize) p
    return Layout {width = coordX c, height = coordY c}

foreign import WINDOWS_CCONV "windows.h WriteConsoleW" c_WriteConsoleW
    :: HANDLE -> Ptr TCHAR -> DWORD -> Ptr DWORD -> Ptr () -> IO Bool

writeConsole :: HANDLE -> String -> IO ()
-- For some reason, Wine returns False when WriteConsoleW is called on an empty
-- string.  Easiest fix: just don't call that function.
writeConsole _ "" = return ()
writeConsole h str = writeConsole' >> writeConsole h ys
  where
    (xs,ys) = splitAt limit str
    -- WriteConsoleW has a buffer limit which is documented as 32768 word8's,
    -- but bug reports from online suggest that the limit may be lower (~25000).
    -- To be safe, we pick a round number we know to be less than the limit.
    limit = 20000 -- known to be less than WriteConsoleW's buffer limit
    writeConsole'
        = withArray (map (toEnum . fromEnum) xs)
            $ \t_arr -> alloca $ \numWritten -> do
                    failIfFalse_ "WriteConsoleW"
                        $ c_WriteConsoleW h t_arr (toEnum $ length xs)
                                numWritten nullPtr

foreign import WINDOWS_CCONV "windows.h MessageBeep" c_messageBeep :: UINT -> IO Bool

messageBeep :: IO ()
messageBeep = c_messageBeep simpleBeep >> return ()-- intentionally ignore failures.
  where simpleBeep = 0xffffffff


----------
-- Console mode
foreign import WINDOWS_CCONV "windows.h GetConsoleMode" c_GetConsoleMode
    :: HANDLE -> Ptr DWORD -> IO Bool

foreign import WINDOWS_CCONV "windows.h SetConsoleMode" c_SetConsoleMode
    :: HANDLE -> DWORD -> IO Bool

withWindowMode :: MonadException m => Handles -> m a -> m a
withWindowMode hs f = do
    let h = hIn hs
    bracket (getConsoleMode h) (setConsoleMode h)
            $ \m -> setConsoleMode h (m .|. (#const ENABLE_WINDOW_INPUT)) >> f
  where
    getConsoleMode h = liftIO $ alloca $ \p -> do
            failIfFalse_ "GetConsoleMode" $ c_GetConsoleMode h p
            peek p
    setConsoleMode h m = liftIO $ failIfFalse_ "SetConsoleMode" $ c_SetConsoleMode h m

----------------------------
-- Drawing

data Handles = Handles { hIn, hOut :: HANDLE }

closeHandles :: Handles -> IO ()
closeHandles hs = closeHandle (hIn hs) >> closeHandle (hOut hs)

newtype Draw m a = Draw {runDraw :: ReaderT Handles m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadException, MonadReader Handles)

type DrawM a = forall m . (MonadIO m, MonadReader Layout m) => Draw m a

instance MonadTrans Draw where
    lift = Draw . lift

getPos :: MonadIO m => Draw m Coord
getPos = asks hOut >>= liftIO . getPosition

setPos :: Coord -> DrawM ()
setPos c = do
    h <- asks hOut
    -- SetPosition will fail if you give it something out of bounds of
    -- the window buffer (i.e., the input line doesn't fit in the window).
    -- So we do a simple guard against that uncommon case.
    -- However, we don't throw away the x coord since it produces sensible
    -- results for some cases.
    maxY <- liftM (subtract 1) $ asks height
    liftIO $ setPosition h c { coordY = max 0 $ min maxY $ coordY c }

printText :: MonadIO m => String -> Draw m ()
printText txt = do
    h <- asks hOut
    liftIO (writeConsole h txt)

printAfter :: [Grapheme] -> DrawM ()
printAfter gs = do
    -- NOTE: you may be tempted to write
    -- do {p <- getPos; printText (...); setPos p}
    -- Unfortunately, that would be WRONG, because if printText wraps
    -- a line at the bottom of the window, causing the window to scroll,
    -- then the old value of p will be incorrect.
    printText (graphemesToString gs)
    movePosLeft gs

drawLineDiffWin :: LineChars -> LineChars -> DrawM ()
drawLineDiffWin (xs1,ys1) (xs2,ys2) = case matchInit xs1 xs2 of
    ([],[])     | ys1 == ys2            -> return ()
    (xs1',[])   | xs1' ++ ys1 == ys2    -> movePosLeft xs1'
    ([],xs2')   | ys1 == xs2' ++ ys2    -> movePosRight xs2'
    (xs1',xs2')                         -> do
        movePosLeft xs1'
        let m = gsWidth xs1' + gsWidth ys1 - (gsWidth xs2' + gsWidth ys2)
        let deadText = stringToGraphemes $ replicate m ' '
        printText (graphemesToString xs2')
        printAfter (ys2 ++ deadText)

movePosRight, movePosLeft :: [Grapheme] -> DrawM ()
movePosRight str = do
    p <- getPos
    w <- asks width
    setPos $ moveCoord w p str
  where
    moveCoord _ p [] = p
    moveCoord w p cs = case splitAtWidth (w - coordX p) cs of
                        (_,[],len) | len < w - coordX p -- stayed on same line
                            -> Coord { coordY = coordY p,
                                       coordX = coordX p + len
                                     }
                        (_,cs',_) -- moved to next line
                            -> moveCoord w Coord {
                                            coordY = coordY p + 1,
                                            coordX = 0
                                           } cs'

movePosLeft str = do
    p <- getPos
    w <- asks width
    setPos $ moveCoord w p str
  where
    moveCoord _ p [] = p
    moveCoord w p cs = case splitAtWidth (coordX p) cs of
                        (_,[],len) -- stayed on same line
                            -> Coord { coordY = coordY p,
                                       coordX = coordX p - len
                                     }
                        (_,_:cs',_) -- moved to previous line
                            -> moveCoord w Coord {
                                            coordY = coordY p - 1,
                                            coordX = w-1
                                           } cs'

crlf :: String
crlf = "\r\n"

instance (MonadException m, MonadReader Layout m) => Term (Draw m) where
    drawLineDiff (xs1,ys1) (xs2,ys2) = let
        fixEsc = filter ((/= '\ESC') . baseChar)
        in drawLineDiffWin (fixEsc xs1, fixEsc ys1) (fixEsc xs2, fixEsc ys2)
    -- TODO now that we capture resize events.
    -- first, looks like the cursor stays on the same line but jumps
    -- to the beginning if cut off.
    reposition _ _ = return ()

    printLines [] = return ()
    printLines ls = printText $ intercalate crlf ls ++ crlf

    clearLayout = clearScreen

    moveToNextLine s = do
        movePosRight (snd s)
        printText "\r\n" -- make the console take care of creating a new line

    ringBell True = liftIO messageBeep
    ringBell False = return () -- TODO

win32TermStdin :: MaybeT IO RunTerm
win32TermStdin = do
    liftIO (hIsTerminalDevice stdin) >>= guard
    win32Term

win32Term :: MaybeT IO RunTerm
win32Term = do
    hs <- consoleHandles
    ch <- liftIO newTChanIO
    fileRT <- liftIO $ fileRunTerm stdin
    return fileRT
      { termOps = Left TermOps {
          getLayout = getBufferSize (hOut hs)
          , withGetEvent = withWindowMode hs
                              . win32WithEvent hs ch
          , saveUnusedKeys = saveKeys ch
          , evalTerm = EvalTerm (runReaderT' hs . runDraw)
                              (Draw . lift)
          , externalPrint = atomically . writeTChan ch . ExternalPrint
          }
      , closeTerm = do
          flushEventQueue (putStrOut fileRT) ch
          closeHandles hs
      }

win32WithEvent :: MonadException m => Handles -> TChan Event
                                        -> (m Event -> m a) -> m a
win32WithEvent h eventChan f = f $ liftIO $ getEvent (hIn h) eventChan

-- stdin is not a terminal, but we still need to check the right way to output unicode to stdout.
fileRunTerm :: Handle -> IO RunTerm
fileRunTerm h_in = do
    putter <- putOut
    cp <- getCodePage
    return RunTerm {
                    closeTerm = return (),
                    putStrOut = putter,
                    wrapInterrupt = withCtrlCHandler,
                    termOps = Right FileOps
                                { withoutInputEcho = hWithoutInputEcho h_in
                                , wrapFileInput = hWithBinaryMode h_in
                                , getLocaleChar = getMultiByteChar cp h_in
                                , maybeReadNewline = hMaybeReadNewline h_in
                                , getLocaleLine = hGetLocaleLine h_in
                                            >>= liftIO . codePageToUnicode cp
                                }

                    }

-- On Windows, Unicode written to the console must be written with the WriteConsole API call.
-- And to make the API cross-platform consistent, Unicode to a file should be UTF-8.
putOut :: IO (String -> IO ())
putOut = do
    outIsTerm <- hIsTerminalDevice stdout
    if outIsTerm
        then do
            h <- getStdHandle sTD_OUTPUT_HANDLE
            return (writeConsole h)
        else do
            cp <- getCodePage
            return $ \str -> unicodeToCodePage cp str >>= B.putStr >> hFlush stdout


type Handler = DWORD -> IO BOOL

foreign import ccall "wrapper" wrapHandler :: Handler -> IO (FunPtr Handler)

foreign import WINDOWS_CCONV "windows.h SetConsoleCtrlHandler" c_SetConsoleCtrlHandler
    :: FunPtr Handler -> BOOL -> IO BOOL

-- sets the tv to True when ctrl-c is pressed.
withCtrlCHandler :: MonadException m => m a -> m a
withCtrlCHandler f = bracket (liftIO $ do
                                    tid <- myThreadId
                                    fp <- wrapHandler (handler tid)
                                -- don't fail if we can't set the ctrl-c handler
                                -- for example, we might not be attached to a console?
                                    _ <- c_SetConsoleCtrlHandler fp True
                                    return fp)
                                (\fp -> liftIO $ c_SetConsoleCtrlHandler fp False)
                                (const f)
  where
    handler tid (#const CTRL_C_EVENT) = do
        throwTo tid Interrupt
        return True
    handler _ _ = return False



------------------------
-- Multi-byte conversion

foreign import WINDOWS_CCONV "WideCharToMultiByte" wideCharToMultiByte
        :: CodePage -> DWORD -> LPCWSTR -> CInt -> LPCSTR -> CInt
                -> LPCSTR -> LPBOOL -> IO CInt

unicodeToCodePage :: CodePage -> String -> IO B.ByteString
unicodeToCodePage cp wideStr = withCWStringLen wideStr $ \(wideBuff, wideLen) -> do
    -- first, ask for the length without filling the buffer.
    outSize <- wideCharToMultiByte cp 0 wideBuff (toEnum wideLen)
                    nullPtr 0 nullPtr nullPtr
    -- then, actually perform the encoding.
    createAndTrim (fromEnum outSize) $ \outBuff ->
        fmap fromEnum $ wideCharToMultiByte cp 0 wideBuff (toEnum wideLen)
                    (castPtr outBuff) outSize nullPtr nullPtr

foreign import WINDOWS_CCONV "MultiByteToWideChar" multiByteToWideChar
        :: CodePage -> DWORD -> LPCSTR -> CInt -> LPWSTR -> CInt -> IO CInt

codePageToUnicode :: CodePage -> B.ByteString -> IO String
codePageToUnicode cp bs = B.useAsCStringLen bs $ \(inBuff, inLen) -> do
    -- first ask for the size without filling the buffer.
    outSize <- multiByteToWideChar cp 0 inBuff (toEnum inLen) nullPtr 0
    -- then, actually perform the decoding.
    allocaArray0 (fromEnum outSize) $ \outBuff -> do
    outSize' <- multiByteToWideChar cp 0 inBuff (toEnum inLen) outBuff outSize
    peekCWStringLen (outBuff, fromEnum outSize')


getCodePage :: IO CodePage
getCodePage = do
    conCP <- getConsoleCP
    if conCP > 0
        then return conCP
        else getACP

foreign import WINDOWS_CCONV "IsDBCSLeadByteEx" c_IsDBCSLeadByteEx
        :: CodePage -> BYTE -> BOOL

getMultiByteChar :: CodePage -> Handle -> MaybeT IO Char
getMultiByteChar cp h = do
        b1 <- hGetByte h
        bs <- if c_IsDBCSLeadByteEx cp b1
                then hGetByte h >>= \b2 -> return [b1,b2]
                else return [b1]
        cs <- liftIO $ codePageToUnicode cp (B.pack bs)
        case cs of
            [] -> getMultiByteChar cp h
            (c:_) -> return c

----------------------------------
-- Clearing screen
-- WriteConsole has a limit of ~20,000-30000 characters, which is
-- less than a 200x200 window, for example.
-- So we'll use other Win32 functions to clear the screen.

getAttribute :: HANDLE -> IO WORD
getAttribute = withScreenBufferInfo $
    (#peek CONSOLE_SCREEN_BUFFER_INFO, wAttributes)

fillConsoleChar :: HANDLE -> Char -> Int -> Coord -> IO ()
fillConsoleChar h c n start = with start $ \startPtr -> alloca $ \numWritten -> do
    failIfFalse_ "FillConsoleOutputCharacter"
        $ c_FillConsoleCharacter h (toEnum $ fromEnum c)
            (toEnum n) startPtr numWritten

foreign import ccall "haskeline_FillConsoleCharacter" c_FillConsoleCharacter
    :: HANDLE -> TCHAR -> DWORD -> Ptr Coord -> Ptr DWORD -> IO BOOL

fillConsoleAttribute :: HANDLE -> WORD -> Int -> Coord -> IO ()
fillConsoleAttribute h a n start = with start $ \startPtr -> alloca $ \numWritten -> do
    failIfFalse_ "FillConsoleOutputAttribute"
        $ c_FillConsoleAttribute h a
            (toEnum n) startPtr numWritten

foreign import ccall "haskeline_FillConsoleAttribute" c_FillConsoleAttribute
    :: HANDLE -> WORD -> DWORD -> Ptr Coord -> Ptr DWORD -> IO BOOL

clearScreen :: DrawM ()
clearScreen = do
    lay <- ask
    h <- asks hOut
    let windowSize = width lay * height lay
    let origin = Coord 0 0
    attr <- liftIO $ getAttribute h
    liftIO $ fillConsoleChar h ' ' windowSize origin
    liftIO $ fillConsoleAttribute h attr windowSize origin
    setPos origin
