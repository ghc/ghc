module Network.Socket.Cbits where

#include "HsNet.h"

import Network.Socket.Imports

-- | This is the value of SOMAXCONN, typically 128.
-- 128 is good enough for normal network servers but
-- is too small for high performance servers.
maxListenQueue :: Int
maxListenQueue = #const SOMAXCONN

#if defined(mingw32_HOST_OS)
wsaNotInitialized :: CInt
wsaNotInitialized = #const WSANOTINITIALISED
#else
fGetFd :: CInt
fGetFd = #const F_GETFD
fGetFl :: CInt
fGetFl = #const F_GETFL
fdCloexec :: CInt
fdCloexec = #const FD_CLOEXEC
oNonBlock :: CInt
oNonBlock = #const O_NONBLOCK
# if defined(HAVE_ADVANCED_SOCKET_FLAGS)
sockNonBlock :: CInt
sockNonBlock = #const SOCK_NONBLOCK
sockCloexec :: CInt
sockCloexec = #const SOCK_CLOEXEC
# endif
#endif
