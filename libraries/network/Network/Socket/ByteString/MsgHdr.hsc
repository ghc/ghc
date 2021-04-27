{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Support module for the POSIX 'sendmsg' system call.
module Network.Socket.ByteString.MsgHdr
    ( MsgHdr(..)
    ) where

#include <sys/types.h>
#include <sys/socket.h>

import Network.Socket.Imports
import Network.Socket.Internal (zeroMemory)
import Network.Socket.Types (SockAddr)

import Network.Socket.ByteString.IOVec (IOVec)

-- We don't use msg_control, msg_controllen, and msg_flags as these
-- don't exist on OpenSolaris.
data MsgHdr = MsgHdr
    { msgName    :: !(Ptr SockAddr)
    , msgNameLen :: !CUInt
    , msgIov     :: !(Ptr IOVec)
    , msgIovLen  :: !CSize
    }

instance Storable MsgHdr where
  sizeOf _    = (#const sizeof(struct msghdr))
  alignment _ = alignment (undefined :: CInt)

  peek p = do
    name       <- (#peek struct msghdr, msg_name)       p
    nameLen    <- (#peek struct msghdr, msg_namelen)    p
    iov        <- (#peek struct msghdr, msg_iov)        p
    iovLen     <- (#peek struct msghdr, msg_iovlen)     p
    return $ MsgHdr name nameLen iov iovLen

  poke p mh = do
    -- We need to zero the msg_control, msg_controllen, and msg_flags
    -- fields, but they only exist on some platforms (e.g. not on
    -- Solaris).  Instead of using CPP, we zero the entire struct.
    zeroMemory p (#const sizeof(struct msghdr))
    (#poke struct msghdr, msg_name)       p (msgName       mh)
    (#poke struct msghdr, msg_namelen)    p (msgNameLen    mh)
    (#poke struct msghdr, msg_iov)        p (msgIov        mh)
    (#poke struct msghdr, msg_iovlen)     p (msgIovLen     mh)
