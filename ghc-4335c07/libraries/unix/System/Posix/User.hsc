{-# LANGUAGE Trustworthy, CApiFFI #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix.User
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- POSIX user\/group support
--
-----------------------------------------------------------------------------

module System.Posix.User (
    -- * User environment
    -- ** Querying the user environment
    getRealUserID,
    getRealGroupID,
    getEffectiveUserID,
    getEffectiveGroupID,
    getGroups,
    getLoginName,
    getEffectiveUserName,

    -- *** The group database
    GroupEntry(..),
    getGroupEntryForID,
    getGroupEntryForName,
    getAllGroupEntries,

    -- *** The user database
    UserEntry(..),
    getUserEntryForID,
    getUserEntryForName,
    getAllUserEntries,

    -- ** Modifying the user environment
    setUserID,
    setGroupID,
    setEffectiveUserID,
    setEffectiveGroupID,
    setGroups

  ) where

#include "HsUnix.h"

import System.Posix.Types
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

#if !defined(HAVE_GETPWNAM_R) || !defined(HAVE_GETPWUID_R) || defined(HAVE_GETPWENT) || defined(HAVE_GETGRENT)
import Control.Concurrent.MVar  ( MVar, newMVar, withMVar )
#endif
#ifdef HAVE_GETPWENT
import Control.Exception
#endif
import Control.Monad
import System.IO.Error

-- internal types
data {-# CTYPE "struct passwd" #-} CPasswd
data {-# CTYPE "struct group"  #-} CGroup

-- -----------------------------------------------------------------------------
-- user environemnt

-- | @getRealUserID@ calls @getuid@ to obtain the real @UserID@
--   associated with the current process.
getRealUserID :: IO UserID
getRealUserID = c_getuid

foreign import ccall unsafe "getuid"
  c_getuid :: IO CUid

-- | @getRealGroupID@ calls @getgid@ to obtain the real @GroupID@
--   associated with the current process.
getRealGroupID :: IO GroupID
getRealGroupID = c_getgid

foreign import ccall unsafe "getgid"
  c_getgid :: IO CGid

-- | @getEffectiveUserID@ calls @geteuid@ to obtain the effective
--   @UserID@ associated with the current process.
getEffectiveUserID :: IO UserID
getEffectiveUserID = c_geteuid

foreign import ccall unsafe "geteuid"
  c_geteuid :: IO CUid

-- | @getEffectiveGroupID@ calls @getegid@ to obtain the effective
--   @GroupID@ associated with the current process.
getEffectiveGroupID :: IO GroupID
getEffectiveGroupID = c_getegid

foreign import ccall unsafe "getegid"
  c_getegid :: IO CGid

-- | @getGroups@ calls @getgroups@ to obtain the list of
--   supplementary @GroupID@s associated with the current process.
getGroups :: IO [GroupID]
getGroups = do
    ngroups <- c_getgroups 0 nullPtr
    allocaArray (fromIntegral ngroups) $ \arr -> do
       throwErrnoIfMinus1_ "getGroups" (c_getgroups ngroups arr)
       groups <- peekArray (fromIntegral ngroups) arr
       return groups

foreign import ccall unsafe "getgroups"
  c_getgroups :: CInt -> Ptr CGid -> IO CInt


-- | @setGroups@ calls @setgroups@ to set the list of
--   supplementary @GroupID@s associated with the current process.
setGroups :: [GroupID] -> IO ()
setGroups groups = do
    withArrayLen groups $ \ ngroups arr ->
       throwErrnoIfMinus1_ "setGroups" (c_setgroups (fromIntegral ngroups) arr)

foreign import ccall unsafe "setgroups"
  c_setgroups :: CInt -> Ptr CGid -> IO CInt



-- | @getLoginName@ calls @getlogin@ to obtain the login name
--   associated with the current process.
getLoginName :: IO String
getLoginName =  do
    -- ToDo: use getlogin_r
    str <- throwErrnoIfNull "getLoginName" c_getlogin
    peekCAString str

foreign import ccall unsafe "getlogin"
  c_getlogin :: IO CString

-- | @setUserID uid@ calls @setuid@ to set the real, effective, and
--   saved set-user-id associated with the current process to @uid@.
setUserID :: UserID -> IO ()
setUserID uid = throwErrnoIfMinus1_ "setUserID" (c_setuid uid)

foreign import ccall unsafe "setuid"
  c_setuid :: CUid -> IO CInt

-- | @setEffectiveUserID uid@ calls @seteuid@ to set the effective
--   user-id associated with the current process to @uid@. This
--   does not update the real user-id or set-user-id.
setEffectiveUserID :: UserID -> IO ()
setEffectiveUserID uid = throwErrnoIfMinus1_ "setEffectiveUserID" (c_seteuid uid)

foreign import ccall unsafe "seteuid"
  c_seteuid :: CUid -> IO CInt

-- | @setGroupID gid@ calls @setgid@ to set the real, effective, and
--   saved set-group-id associated with the current process to @gid@.
setGroupID :: GroupID -> IO ()
setGroupID gid = throwErrnoIfMinus1_ "setGroupID" (c_setgid gid)

foreign import ccall unsafe "setgid"
  c_setgid :: CGid -> IO CInt

-- | @setEffectiveGroupID uid@ calls @setegid@ to set the effective
--   group-id associated with the current process to @gid@. This
--   does not update the real group-id or set-group-id.
setEffectiveGroupID :: GroupID -> IO ()
setEffectiveGroupID gid =
  throwErrnoIfMinus1_ "setEffectiveGroupID" (c_setegid gid)


foreign import ccall unsafe "setegid"
  c_setegid :: CGid -> IO CInt

-- -----------------------------------------------------------------------------
-- User names

-- | @getEffectiveUserName@ gets the name
--   associated with the effective @UserID@ of the process.
getEffectiveUserName :: IO String
getEffectiveUserName = do
    euid <- getEffectiveUserID
    pw <- getUserEntryForID euid
    return (userName pw)

-- -----------------------------------------------------------------------------
-- The group database (grp.h)

data GroupEntry =
 GroupEntry {
  groupName    :: String,       -- ^ The name of this group (gr_name)
  groupPassword :: String,      -- ^ The password for this group (gr_passwd)
  groupID      :: GroupID,      -- ^ The unique numeric ID for this group (gr_gid)
  groupMembers :: [String]      -- ^ A list of zero or more usernames that are members (gr_mem)
 } deriving (Show, Read, Eq)

-- | @getGroupEntryForID gid@ calls @getgrgid_r@ to obtain
--   the @GroupEntry@ information associated with @GroupID@
--   @gid@. This operation may fail with 'isDoesNotExistError'
--   if no such group exists.
getGroupEntryForID :: GroupID -> IO GroupEntry
#ifdef HAVE_GETGRGID_R
getGroupEntryForID gid =
  allocaBytes (#const sizeof(struct group)) $ \pgr ->
   doubleAllocWhileERANGE "getGroupEntryForID" "group" grBufSize unpackGroupEntry $
     c_getgrgid_r gid pgr

foreign import capi unsafe "HsUnix.h getgrgid_r"
  c_getgrgid_r :: CGid -> Ptr CGroup -> CString
                 -> CSize -> Ptr (Ptr CGroup) -> IO CInt
#else
getGroupEntryForID = error "System.Posix.User.getGroupEntryForID: not supported"
#endif

-- | @getGroupEntryForName name@ calls @getgrnam_r@ to obtain
--   the @GroupEntry@ information associated with the group called
--   @name@. This operation may fail with 'isDoesNotExistError'
--   if no such group exists.
getGroupEntryForName :: String -> IO GroupEntry
#ifdef HAVE_GETGRNAM_R
getGroupEntryForName name =
  allocaBytes (#const sizeof(struct group)) $ \pgr ->
    withCAString name $ \ pstr ->
      doubleAllocWhileERANGE "getGroupEntryForName" "group" grBufSize unpackGroupEntry $
        c_getgrnam_r pstr pgr

foreign import capi unsafe "HsUnix.h getgrnam_r"
  c_getgrnam_r :: CString -> Ptr CGroup -> CString
                 -> CSize -> Ptr (Ptr CGroup) -> IO CInt
#else
getGroupEntryForName = error "System.Posix.User.getGroupEntryForName: not supported"
#endif

-- | @getAllGroupEntries@ returns all group entries on the system by
--   repeatedly calling @getgrent@

--
-- getAllGroupEntries may fail with isDoesNotExistError on Linux due to
-- this bug in glibc:
--   http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=466647
--
getAllGroupEntries :: IO [GroupEntry]
#ifdef HAVE_GETGRENT
getAllGroupEntries =
    withMVar lock $ \_ -> bracket_ c_setgrent c_endgrent $ worker []
    where worker accum =
              do resetErrno
                 ppw <- throwErrnoIfNullAndError "getAllGroupEntries" $
                        c_getgrent
                 if ppw == nullPtr
                     then return (reverse accum)
                     else do thisentry <- unpackGroupEntry ppw
                             worker (thisentry : accum)

foreign import ccall unsafe "getgrent"
  c_getgrent :: IO (Ptr CGroup)
foreign import ccall unsafe "setgrent"
  c_setgrent :: IO ()
foreign import ccall unsafe "endgrent"
  c_endgrent :: IO ()
#else
getAllGroupEntries = error "System.Posix.User.getAllGroupEntries: not supported"
#endif

#if defined(HAVE_GETGRGID_R) || defined(HAVE_GETGRNAM_R)
grBufSize :: Int
#if defined(HAVE_SYSCONF) && defined(HAVE_SC_GETGR_R_SIZE_MAX)
grBufSize = sysconfWithDefault 1024 (#const _SC_GETGR_R_SIZE_MAX)
#else
grBufSize = 1024
#endif
#endif

unpackGroupEntry :: Ptr CGroup -> IO GroupEntry
unpackGroupEntry ptr = do
   name    <- (#peek struct group, gr_name) ptr >>= peekCAString
   passwd  <- (#peek struct group, gr_passwd) ptr >>= peekCAString
   gid     <- (#peek struct group, gr_gid) ptr
   mem     <- (#peek struct group, gr_mem) ptr
   members <- peekArray0 nullPtr mem >>= mapM peekCAString
   return (GroupEntry name passwd gid members)

-- -----------------------------------------------------------------------------
-- The user database (pwd.h)

data UserEntry =
 UserEntry {
   userName      :: String,     -- ^ Textual name of this user (pw_name)
   userPassword  :: String,     -- ^ Password -- may be empty or fake if shadow is in use (pw_passwd)
   userID        :: UserID,     -- ^ Numeric ID for this user (pw_uid)
   userGroupID   :: GroupID,    -- ^ Primary group ID (pw_gid)
   userGecos     :: String,     -- ^ Usually the real name for the user (pw_gecos)
   homeDirectory :: String,     -- ^ Home directory (pw_dir)
   userShell     :: String      -- ^ Default shell (pw_shell)
 } deriving (Show, Read, Eq)

--
-- getpwuid and getpwnam leave results in a static object. Subsequent
-- calls modify the same object, which isn't threadsafe. We attempt to
-- mitigate this issue, on platforms that don't provide the safe _r versions
--
-- Also, getpwent/setpwent require a global lock since they maintain
-- an internal file position pointer.
#if !defined(HAVE_GETPWNAM_R) || !defined(HAVE_GETPWUID_R) || defined(HAVE_GETPWENT) || defined(HAVE_GETGRENT)
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()
{-# NOINLINE lock #-}
#endif

-- | @getUserEntryForID gid@ calls @getpwuid_r@ to obtain
--   the @UserEntry@ information associated with @UserID@
--   @uid@. This operation may fail with 'isDoesNotExistError'
--   if no such user exists.
getUserEntryForID :: UserID -> IO UserEntry
#ifdef HAVE_GETPWUID_R
getUserEntryForID uid =
  allocaBytes (#const sizeof(struct passwd)) $ \ppw ->
    doubleAllocWhileERANGE "getUserEntryForID" "user" pwBufSize unpackUserEntry $
      c_getpwuid_r uid ppw

foreign import capi unsafe "HsUnix.h getpwuid_r"
  c_getpwuid_r :: CUid -> Ptr CPasswd ->
                        CString -> CSize -> Ptr (Ptr CPasswd) -> IO CInt
#elif HAVE_GETPWUID
getUserEntryForID uid = do
  withMVar lock $ \_ -> do
    ppw <- throwErrnoIfNull "getUserEntryForID" $ c_getpwuid uid
    unpackUserEntry ppw

foreign import ccall unsafe "getpwuid"
  c_getpwuid :: CUid -> IO (Ptr CPasswd)
#else
getUserEntryForID = error "System.Posix.User.getUserEntryForID: not supported"
#endif

-- | @getUserEntryForName name@ calls @getpwnam_r@ to obtain
--   the @UserEntry@ information associated with the user login
--   @name@. This operation may fail with 'isDoesNotExistError'
--   if no such user exists.
getUserEntryForName :: String -> IO UserEntry
#if HAVE_GETPWNAM_R
getUserEntryForName name =
  allocaBytes (#const sizeof(struct passwd)) $ \ppw ->
    withCAString name $ \ pstr ->
      doubleAllocWhileERANGE "getUserEntryForName" "user" pwBufSize unpackUserEntry $
        c_getpwnam_r pstr ppw

foreign import capi unsafe "HsUnix.h getpwnam_r"
  c_getpwnam_r :: CString -> Ptr CPasswd
               -> CString -> CSize -> Ptr (Ptr CPasswd) -> IO CInt
#elif HAVE_GETPWNAM
getUserEntryForName name = do
  withCAString name $ \ pstr -> do
    withMVar lock $ \_ -> do
      ppw <- throwErrnoIfNull "getUserEntryForName" $ c_getpwnam pstr
      unpackUserEntry ppw

foreign import ccall unsafe "getpwnam"
  c_getpwnam :: CString -> IO (Ptr CPasswd)
#else
getUserEntryForName = error "System.Posix.User.getUserEntryForName: not supported"
#endif

-- | @getAllUserEntries@ returns all user entries on the system by
--   repeatedly calling @getpwent@
getAllUserEntries :: IO [UserEntry]
#ifdef HAVE_GETPWENT
getAllUserEntries =
    withMVar lock $ \_ -> bracket_ c_setpwent c_endpwent $ worker []
    where worker accum =
              do resetErrno
                 ppw <- throwErrnoIfNullAndError "getAllUserEntries" $
                        c_getpwent
                 if ppw == nullPtr
                     then return (reverse accum)
                     else do thisentry <- unpackUserEntry ppw
                             worker (thisentry : accum)

foreign import capi unsafe "HsUnix.h getpwent"
  c_getpwent :: IO (Ptr CPasswd)
foreign import capi unsafe "HsUnix.h setpwent"
  c_setpwent :: IO ()
foreign import capi unsafe "HsUnix.h endpwent"
  c_endpwent :: IO ()
#else
getAllUserEntries = error "System.Posix.User.getAllUserEntries: not supported"
#endif

#if defined(HAVE_GETPWUID_R) || defined(HAVE_GETPWNAM_R)
pwBufSize :: Int
#if  defined(HAVE_SYSCONF) && defined(HAVE_SC_GETPW_R_SIZE_MAX)
pwBufSize = sysconfWithDefault 1024 (#const _SC_GETPW_R_SIZE_MAX)
#else
pwBufSize = 1024
#endif
#endif

#ifdef HAVE_SYSCONF
foreign import ccall unsafe "sysconf"
  c_sysconf :: CInt -> IO CLong

-- We need a default value since sysconf can fail and return -1
-- even when the parameter name is defined in unistd.h.
-- One example of this is _SC_GETPW_R_SIZE_MAX under
-- Mac OS X 10.4.9 on i386.
sysconfWithDefault :: Int -> CInt -> Int
sysconfWithDefault def sc =
    unsafePerformIO $ do v <- fmap fromIntegral $ c_sysconf sc
                         return $ if v == (-1) then def else v
#endif

-- The following function is used by the getgr*_r, c_getpw*_r
-- families of functions. These functions return their result
-- in a struct that contains strings and they need a buffer
-- that they can use to store those strings. We have to be
-- careful to unpack the struct containing the result before
-- the buffer is deallocated.
doubleAllocWhileERANGE
  :: String
  -> String -- entry type: "user" or "group"
  -> Int
  -> (Ptr r -> IO a)
  -> (Ptr b -> CSize -> Ptr (Ptr r) -> IO CInt)
  -> IO a
doubleAllocWhileERANGE loc enttype initlen unpack action =
  alloca $ go initlen
 where
  go len res = do
    r <- allocaBytes len $ \buf -> do
           rc <- action buf (fromIntegral len) res
           if rc /= 0
             then return (Left rc)
             else do p <- peek res
                     when (p == nullPtr) $ notFoundErr
                     fmap Right (unpack p)
    case r of
      Right x -> return x
      Left rc | Errno rc == eRANGE ->
        -- ERANGE means this is not an error
        -- we just have to try again with a larger buffer
        go (2 * len) res
      Left rc ->
        ioError (errnoToIOError loc (Errno rc) Nothing Nothing)
  notFoundErr =
    ioError $ flip ioeSetErrorString ("no such " ++ enttype)
            $ mkIOError doesNotExistErrorType loc Nothing Nothing

unpackUserEntry :: Ptr CPasswd -> IO UserEntry
unpackUserEntry ptr = do
   name   <- (#peek struct passwd, pw_name)   ptr >>= peekCAString
   passwd <- (#peek struct passwd, pw_passwd) ptr >>= peekCAString
   uid    <- (#peek struct passwd, pw_uid)    ptr
   gid    <- (#peek struct passwd, pw_gid)    ptr
#ifdef HAVE_NO_PASSWD_PW_GECOS
   gecos  <- return ""  -- pw_gecos does not exist on android
#else
   gecos  <- (#peek struct passwd, pw_gecos)  ptr >>= peekCAString
#endif
   dir    <- (#peek struct passwd, pw_dir)    ptr >>= peekCAString
   shell  <- (#peek struct passwd, pw_shell)  ptr >>= peekCAString
   return (UserEntry name passwd uid gid gecos dir shell)

-- Used when a function returns NULL to indicate either an error or
-- EOF, depending on whether the global errno is nonzero.
throwErrnoIfNullAndError :: String -> IO (Ptr a) -> IO (Ptr a)
throwErrnoIfNullAndError loc act = do
    rc <- act
    errno <- getErrno
    if rc == nullPtr && errno /= eOK
       then throwErrno loc
       else return rc
