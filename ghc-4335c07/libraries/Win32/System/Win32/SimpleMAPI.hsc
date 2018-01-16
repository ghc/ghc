#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Win32.SimpleMAPI
-- Copyright   :  (c) Esa Ilari Vuokko, 2006
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Esa Ilari Vuokko <ei@vuokko.info>
-- Stability   :  provisional
-- Portability :  portable
--
-- FFI-bindings to interact with SimpleMAPI
--
-----------------------------------------------------------------------------
module System.Win32.SimpleMAPI
where

-- I am not sure why exactly, but with mingw64 mapi.h does not define
-- some of the values we use, e.g. MAPI_LOGOFF_SHARED.
-- mapix.h does define MAPI_LOGOFF_SHARED, but the various flags
-- clash with each other.

import Control.Exception    ( bracket, handle, finally, onException
                            , IOException )
import Control.Monad        ( liftM5 )
import Foreign              ( FunPtr, newForeignPtr, pokeByteOff, maybeWith
                            , Ptr, castPtr, castPtrToFunPtr, nullPtr
                            , touchForeignPtr, alloca, peek, allocaBytes
                            , minusPtr, plusPtr, copyBytes, ForeignPtr )
import Foreign.C            ( withCAString, withCAStringLen )
  -- Apparently, simple MAPI does not support unicode and probably never will,
  -- so this module will just mangle any Unicode in your strings
import Graphics.Win32.GDI.Types     ( HWND)
import System.Win32.DLL     ( loadLibrary, c_GetProcAddress, freeLibrary
                            , c_FreeLibraryFinaliser )
import System.Win32.Types   ( DWORD, LPSTR, HMODULE, failIfNull )

##include "windows_cconv.h"

#include "windows.h"
#include "mapi.h"


type ULONG = DWORD
type LHANDLE = ULONG
newtype MapiRecipDesc = MapiRecipDesc ()
type MapiFlag = ULONG
#{enum MapiFlag,
    , mAPI_LOGON_UI         = MAPI_LOGON_UI
    , mAPI_NEW_SESSION      = MAPI_NEW_SESSION
    , mAPI_FORCE_DOWNLOAD   = MAPI_FORCE_DOWNLOAD
    , mAPI_DIALOG           = MAPI_DIALOG
    , mAPI_UNREAD_ONLY      = MAPI_UNREAD_ONLY
    , mAPI_LONG_MSGID       = MAPI_LONG_MSGID
    , mAPI_GUARANTEE_FIFO   = MAPI_GUARANTEE_FIFO
    , mAPI_ENVELOPE_ONLY    = MAPI_ENVELOPE_ONLY
    , mAPI_PEEK             = MAPI_PEEK
    , mAPI_BODY_AS_FILE     = MAPI_BODY_AS_FILE
    , mAPI_SUPPRESS_ATTACH  = MAPI_SUPPRESS_ATTACH
    , mAPI_AB_NOMODIFY      = MAPI_AB_NOMODIFY
    , mAPI_OLE              = MAPI_OLE
    , mAPI_OLE_STATIC       = MAPI_OLE_STATIC
    , mAPI_UNREAD           = MAPI_UNREAD
    , mAPI_RECEIPT_REQUESTED = MAPI_RECEIPT_REQUESTED
    , mAPI_SENT             = MAPI_SENT
    }
-- Have to define enum values outside previous declaration due to
-- hsc2hs bug in --cross-compile mode:
--    https://ghc.haskell.org/trac/ghc/ticket/13620
#ifdef MAPI_LOGOFF_SHARED
#{enum MapiFlag,
    , mAPI_LOGOFF_SHARED    = MAPI_LOGOFF_SHARED
}
#endif
#ifdef MAPI_LOGOFF_UI
#{enum MapiFlag,
    , mAPI_LOGOFF_UI        = MAPI_LOGOFF_UI
}
#endif

mapiErrors :: [(ULONG,String)]
mapiErrors =
    [ ((#const SUCCESS_SUCCESS)         , "Success")
    , ((#const MAPI_E_FAILURE)          , "Generic error or multiple errors")
    , ((#const MAPI_E_USER_ABORT)       , "User aborted")
    , ((#const MAPI_E_LOGIN_FAILURE)    , "Logoff failed")
    , ((#const MAPI_E_LOGON_FAILURE)    , "Logon failed")
    , ((#const MAPI_E_DISK_FULL)        , "Disk full")
    , ((#const MAPI_E_INSUFFICIENT_MEMORY)      , "Not enough memory")
    , ((#const MAPI_E_ACCESS_DENIED)    , "Access denied")
#ifdef MAPI_E_BLK_TOO_SMALL
    , ((#const MAPI_E_BLK_TOO_SMALL)    , "BLK_TOO_SMALL")
#endif
    , ((#const MAPI_E_TOO_MANY_SESSIONS), "Too many open sessions")
    , ((#const MAPI_E_TOO_MANY_FILES)   , "Too many open files")
    , ((#const MAPI_E_TOO_MANY_RECIPIENTS)      , "Too many recipients")
    , ((#const MAPI_E_ATTACHMENT_NOT_FOUND)     , "Attachemnt not found")
    , ((#const MAPI_E_ATTACHMENT_OPEN_FAILURE)  , "Couldn't open attachment")
    , ((#const MAPI_E_ATTACHMENT_WRITE_FAILURE) , "Couldn't write attachment")
    , ((#const MAPI_E_UNKNOWN_RECIPIENT)        , "Unknown recipient")
    , ((#const MAPI_E_BAD_RECIPTYPE)            , "Bad recipient type")
    , ((#const MAPI_E_NO_MESSAGES)              , "No messages")
    , ((#const MAPI_E_INVALID_MESSAGE)          , "Invalid message")
    , ((#const MAPI_E_TEXT_TOO_LARGE)           , "Text too large")
    , ((#const MAPI_E_INVALID_SESSION)          , "Invalid session")
    , ((#const MAPI_E_TYPE_NOT_SUPPORTED)       , "Type not supported")
    , ((#const MAPI_E_AMBIGUOUS_RECIPIENT)      , "Ambigious recipient")
#ifdef MAPI_E_AMBIGUOUS_RECIP
    , ((#const MAPI_E_AMBIGUOUS_RECIP)          , "Ambigious recipient")
#endif
    , ((#const MAPI_E_MESSAGE_IN_USE)           , "Message in use")
    , ((#const MAPI_E_NETWORK_FAILURE)          , "Network failure")
    , ((#const MAPI_E_INVALID_EDITFIELDS)       , "Invalid editfields")
    , ((#const MAPI_E_INVALID_RECIPS)           , "Invalid recipient(s)")
    , ((#const MAPI_E_NOT_SUPPORTED)            , "Not supported")
    ]

mapiErrorString :: ULONG -> String
mapiErrorString c = case lookup c mapiErrors of
    Nothing -> "Unkown error (" ++ show c ++ ")"
    Just x  -> x

mapiFail :: String -> IO ULONG -> IO ULONG
mapiFail name act = act >>= \err -> if err==(#const SUCCESS_SUCCESS)
    then return err
    else fail $ name ++ ": " ++ mapiErrorString err


mapiFail_ :: String -> IO ULONG -> IO ()
mapiFail_ n a = mapiFail n a >> return ()

type MapiLogonType = ULONG -> LPSTR -> LPSTR -> MapiFlag -> ULONG -> Ptr LHANDLE -> IO ULONG
foreign import WINDOWS_CCONV "dynamic" mkMapiLogon :: FunPtr MapiLogonType -> MapiLogonType

type MapiLogoffType = LHANDLE -> ULONG -> MapiFlag -> ULONG -> IO ULONG
foreign import WINDOWS_CCONV "dynamic" mkMapiLogoff :: FunPtr MapiLogoffType -> MapiLogoffType

type MapiResolveNameType =
    LHANDLE -> ULONG -> LPSTR -> MapiFlag -> ULONG
    -> Ptr (Ptr MapiRecipDesc) -> IO ULONG
foreign import WINDOWS_CCONV "dynamic" mkMapiResolveName :: FunPtr MapiResolveNameType -> MapiResolveNameType

type MapiFreeBufferType = Ptr () -> IO ULONG
foreign import WINDOWS_CCONV "dynamic" mkMapiFreeBuffer :: FunPtr MapiFreeBufferType -> MapiFreeBufferType

type MapiSendMailType = LHANDLE -> ULONG -> Ptr Message -> MapiFlag -> ULONG -> IO ULONG
foreign import WINDOWS_CCONV "dynamic" mkMapiSendMail :: FunPtr MapiSendMailType -> MapiSendMailType

data MapiFuncs = MapiFuncs
    { mapifLogon    :: MapiLogonType
    , mapifLogoff   :: MapiLogoffType
    , mapifResolveName  :: MapiResolveNameType
    , mapifFreeBuffer   :: MapiFreeBufferType
    , mapifSendMail :: MapiSendMailType
    }

type MapiLoaded = (MapiFuncs, ForeignPtr ())


-- |
loadMapiFuncs :: String -> HMODULE -> IO MapiFuncs
loadMapiFuncs dllname dll =  liftM5 MapiFuncs
    (loadProc "MAPILogon"       dll mkMapiLogon)
    (loadProc "MAPILogoff"      dll mkMapiLogoff)
    (loadProc "MAPIResolveName" dll mkMapiResolveName)
    (loadProc "MAPIFreeBuffer"  dll mkMapiFreeBuffer)
    (loadProc "MAPISendMail"    dll mkMapiSendMail)
    where
       loadProc :: String -> HMODULE -> (FunPtr a -> a) -> IO a
       loadProc name dll' conv = withCAString name $ \name' -> do
            proc <- failIfNull ("loadMapiDll: " ++ dllname ++ ": " ++ name)
                        $ c_GetProcAddress dll' name'
            return $ conv $ castPtrToFunPtr proc
-- |
loadMapiDll :: String -> IO (MapiFuncs, HMODULE)
loadMapiDll dllname = do
    dll <- loadLibrary dllname
    do funcs <- loadMapiFuncs dllname dll
       return (funcs, dll)
     `onException` freeLibrary dll

-- |
withMapiFuncs :: [String] -> (MapiFuncs -> IO a) -> IO a
withMapiFuncs dlls act = bracket load free (act . fst)
    where
        loadOne l = case l of
            []  -> fail $ "withMapiFuncs: Failed to load DLLs: " ++ show dlls
            x:y -> handleIOException (const $ loadOne y) (loadMapiDll x)
        load = loadOne dlls
        free = freeLibrary . snd

-- |
loadMapi :: [String] -> IO MapiLoaded
loadMapi dlls = do
    (f,m) <- loadOne dlls
    m' <- newForeignPtr c_FreeLibraryFinaliser m
    return (f,m')
    where
        loadOne l = case l of
            []  -> fail $ "loadMapi: Failed to load any of DLLs: " ++ show dlls
            x:y -> handleIOException (const $ loadOne y) (loadMapiDll x)

-- |
withMapiLoaded :: MapiLoaded -> (MapiFuncs -> IO a) -> IO a
withMapiLoaded (f,m) act = finally (act f) (touchForeignPtr m)

maybeHWND :: Maybe HWND -> ULONG
maybeHWND = maybe 0 (fromIntegral . flip minusPtr nullPtr)

-- | Create Simple MAPI-session by logon
mapiLogon
    :: MapiFuncs    -- ^ Functions loaded from MAPI DLL
    -> Maybe HWND   -- ^ Parent window, used for modal logon dialog
    -> Maybe String -- ^ Session
    -> Maybe String -- ^ Password
    -> MapiFlag     -- ^ None, one or many flags: FORCE_DOWNLOAD, NEW_SESSION, LOGON_UI, PASSWORD_UI
    -> IO LHANDLE
mapiLogon f hwnd ses pw flags =
    maybeWith withCAString ses  $ \c_ses ->
    maybeWith withCAString pw   $ \c_pw  ->
    alloca                      $ \out   -> do
        mapiFail_ "MAPILogon: " $ mapifLogon
            f (maybeHWND hwnd) 
            c_ses c_pw flags 0 out
        peek out

-- | End Simple MAPI-session
mapiLogoff
    :: MapiFuncs
    -> LHANDLE
    -> Maybe HWND
    -> IO ()
mapiLogoff f ses hwnd
    = mapiFail_ "MAPILogoff"
        $ mapifLogoff f ses (maybeHWND hwnd) 0 0


data RecipientClass = RcOriginal | RcTo | RcCc | RcBcc
    deriving (Show, Eq, Ord, Enum)

rcToULONG :: RecipientClass -> ULONG
rcToULONG = fromIntegral . fromEnum

uLONGToRc :: ULONG -> RecipientClass
uLONGToRc = toEnum . fromIntegral


data Recipient
    = RecipResolve (Maybe HWND) MapiFlag String (Maybe Recipient)
    | Recip String String
    deriving (Show)
type Recipients = [(RecipientClass, Recipient)]

simpleRecip :: String -> Recipient
simpleRecip s = RecipResolve Nothing 0 s $ Just $ Recip s s

withRecipient
    :: MapiFuncs
    -> LHANDLE
    -> RecipientClass
    -> Recipient
    -> (Ptr MapiRecipDesc -> IO a)
    -> IO a
withRecipient f ses rcls rec act = resolve "" rec
    where
        a buf = do
            (#poke MapiRecipDesc, ulRecipClass) buf (rcToULONG rcls)
            act buf
        resolve err rc = case rc of
            Recip name addr ->
                withCAString name $ \c_name ->
                withCAString addr $ \c_addr ->
                allocaBytes (#size MapiRecipDesc) $ \buf -> do
                    (#poke MapiRecipDesc, ulReserved)   buf (0::ULONG)
                    (#poke MapiRecipDesc, lpszName)     buf c_name
                    (#poke MapiRecipDesc, lpszAddress)  buf c_addr
                    (#poke MapiRecipDesc, ulEIDSize)    buf (0::ULONG)
                    (#poke MapiRecipDesc, lpEntryID)    buf nullPtr
                    a buf
            RecipResolve hwnd flag name fallback -> do
                res <-  alloca          $ \res ->
                        withCAString name $ \name' -> do
                            errn <- mapifResolveName
                                    f ses (maybeHWND hwnd) name' flag 0 res
                            if errn==(#const SUCCESS_SUCCESS)
                                then do
                                    buf <- peek res
                                    v <- a buf
                                    _ <- mapifFreeBuffer f $ castPtr buf
                                    return $ Right v
                                else return $ Left
                                    $ err ++ ", "
                                    ++ name ++ ":" ++ mapiErrorString errn
                case res of
                    Left e -> case fallback of
                        Nothing -> fail $ "Failed to resolve any of the recipients: " ++ e
                        Just x  -> resolve e x
                    Right x -> return x

withRecipients
    :: MapiFuncs
    -> LHANDLE
    -> Recipients
    -> (Int -> Ptr MapiRecipDesc -> IO a)
    -> IO a
withRecipients f ses rec act = w [] rec
    where
        w res [] = allocaBytes (length res*rs) $ \buf -> do
            mapM_ (write buf) $ zip [0..] $ reverse res
            act (length res) buf
        w res ((c,r):y) = withRecipient f ses c r $ \x -> w (x:res) y
        rs = (#size MapiRecipDesc)
        write buf (off,src) = do
            let buf' = plusPtr buf (off*rs)
            copyBytes buf' src rs

data FileTag = FileTag
    { ftTag         :: Maybe String -- ^ mime
    , ftEncoding    :: Maybe String
    } deriving (Show)

defFileTag :: FileTag
defFileTag = FileTag Nothing Nothing

withFileTag :: FileTag -> (Ptr FileTag -> IO a) -> IO a
withFileTag ft act =
    allocaBytes (#size MapiFileTagExt)  $ \buf ->
    w (ftTag ft)                        $ \(tbuf,tsiz) ->
    w (ftEncoding ft)                   $ \(ebuf,esiz) -> do
        (#poke MapiFileTagExt, ulReserved)  buf (0::ULONG)
        (#poke MapiFileTagExt, cbTag)       buf tsiz
        (#poke MapiFileTagExt, lpTag)       buf tbuf
        (#poke MapiFileTagExt, cbEncoding)  buf esiz
        (#poke MapiFileTagExt, lpEncoding)  buf ebuf
        act buf
    where
        w v a = case v of
            Nothing -> a (nullPtr, 0)
            Just x  -> withCAStringLen x a

data Attachment = Attachment
    { attFlag       :: MapiFlag
    , attPosition   :: Maybe ULONG
    , attPath       :: String
    , attName       :: Maybe String
    , attTag        :: Maybe FileTag
    } deriving (Show)
defAttachment :: Attachment
defAttachment = Attachment 0 Nothing "" Nothing Nothing
type Attachments = [Attachment]

withAttachments :: Attachments -> (Int -> Ptr Attachment -> IO a) -> IO a
withAttachments att act = allocaBytes (len*as) $ \buf -> write (act len buf) buf att
    where
        as = (#size MapiFileDesc)
        len = length att
        write act' _ [] = act'
        write act' buf (att':y) =
            withCAString (attPath att') $ \path ->
            maybeWith withFileTag (attTag att') $ \tag ->
            withCAString (maybe (attPath att') id (attName att')) $ \name -> do
                (#poke MapiFileDesc, ulReserved)    buf (0::ULONG)
                (#poke MapiFileDesc, flFlags)       buf (attFlag att')
                (#poke MapiFileDesc, nPosition)     buf (maybe 0xffffffff id $ attPosition att')
                (#poke MapiFileDesc, lpszPathName)  buf path
                (#poke MapiFileDesc, lpszFileName)  buf name
                (#poke MapiFileDesc, lpFileType)    buf tag
                write act' (plusPtr buf as) y

data Message = Message
    { msgSubject    :: String
    , msgBody       :: String
    , msgType       :: Maybe String
    , msgDate       :: Maybe String
    , msgConversationId :: Maybe String
    , msgFlags      :: MapiFlag
    , msgFrom       :: Maybe Recipient
    , msgRecips     :: Recipients
    , msgAttachments :: Attachments
    } deriving (Show)

defMessage :: Message
defMessage = Message "" "" Nothing Nothing Nothing 0 Nothing [] []

withMessage
    :: MapiFuncs
    -> LHANDLE
    -> Message
    -> (Ptr Message -> IO a)
    -> IO a
withMessage f ses m act =
    withCAString (msgSubject m)             $ \subject ->
    withCAString (msgBody m)                $ \body ->
    maybeWith withCAString (msgType m)      $ \message_type ->
    maybeWith withCAString (msgDate m)      $ \date ->
    maybeWith withCAString (msgConversationId m) $ \conv_id ->
    withRecipients f ses (msgRecips m)          $ \rlen rbuf ->
    withAttachments (msgAttachments m)      $ \alen abuf ->
    maybeWith (withRecipient f ses RcOriginal) (msgFrom m) $ \from ->
    allocaBytes (#size MapiMessage)             $ \buf -> do
        (#poke MapiMessage, ulReserved)     buf (0::ULONG)
        (#poke MapiMessage, lpszSubject)    buf subject
        (#poke MapiMessage, lpszNoteText)   buf body
        (#poke MapiMessage, lpszMessageType) buf message_type
        (#poke MapiMessage, lpszDateReceived) buf date
        (#poke MapiMessage, lpszConversationID) buf conv_id
        (#poke MapiMessage, flFlags)        buf (msgFlags m)
        (#poke MapiMessage, lpOriginator)   buf from
        (#poke MapiMessage, nRecipCount)    buf (fromIntegral rlen :: ULONG)
        (#poke MapiMessage, lpRecips)       buf rbuf
        (#poke MapiMessage, nFileCount)     buf alen
        (#poke MapiMessage, lpFiles)        buf abuf
        act buf

mapiSendMail :: MapiFuncs -> LHANDLE -> Maybe HWND -> Message -> MapiFlag -> IO ()
mapiSendMail f ses hwnd msg flag = withMessage f ses msg $ \c_msg ->
    mapiFail_ "MAPISendMail" $ mapifSendMail f ses (maybeHWND hwnd) c_msg flag 0

handleIOException :: (IOException -> IO a) -> IO a -> IO a
handleIOException = handle
