%
% (c) The GRASP/AQUA Project, Glasgow University, 1995-1997
%
\section[PosixDB]{Haskell 1.4 POSIX System Databases}

\begin{code}
module PosixDB (
    GroupEntry(..),
    UserEntry(..),

    getUserEntryForID,    -- :: UserID -> IO UserEntry
    getUserEntryForName,  -- :: String -> IO UserEntry

    getGroupEntryForID,   -- :: GroupID -> IO GroupEntry
    getGroupEntryForName  -- :: String -> IO GroupEntry

    ) where

import ST
import PrelIOBase
import Addr
import IO
import PosixUtil
import CString ( unvectorize, strcpy, packStringIO )
\end{code}


\begin{code}

data GroupEntry =
 GroupEntry {
  groupName    :: String,
  groupID      :: GroupID,
  groupMembers :: [String]
 }

data UserEntry =
 UserEntry {
   userName      :: String,
   userID        :: UserID,
   userGroupID   :: GroupID,
   homeDirectory :: String,
   userShell     :: String
 }


getGroupEntryForID :: GroupID -> IO GroupEntry
getGroupEntryForID gid = do
    ptr <- _ccall_ getgrgid gid
    if ptr == nullAddr then
	ioError (IOError Nothing NoSuchThing
	     "getGroupEntryForID" "no such group entry")
     else
	unpackGroupEntry ptr

getGroupEntryForName :: String -> IO GroupEntry
getGroupEntryForName name = do
    gname <- packStringIO name
    ptr <- _ccall_ getgrnam gname
    if ptr == nullAddr then
	ioError (IOError Nothing NoSuchThing
	     "getGroupEntryForName" "no such group entry")
     else
	unpackGroupEntry ptr

getUserEntryForID :: UserID -> IO UserEntry
getUserEntryForID uid = do
    ptr <- _ccall_ getpwuid uid
    if ptr == nullAddr then
	ioError (IOError Nothing NoSuchThing
	     "getUserEntryForID" "no such user entry")
     else
	unpackUserEntry ptr

getUserEntryForName :: String -> IO UserEntry
getUserEntryForName name = do
    uname <- packStringIO name
    ptr   <- _ccall_ getpwnam uname
    if ptr == nullAddr then
	ioError (IOError Nothing NoSuchThing
	     "getUserEntryForName" "no such user entry")
     else
	unpackUserEntry ptr
\end{code}

Local utility functions

\begin{code}
-- Copy the static structure returned by getgr* into a Haskell structure

unpackGroupEntry :: Addr -> IO GroupEntry
unpackGroupEntry ptr =
  do
   str  <- _casm_ ``%r = ((struct group *)%0)->gr_name;'' ptr
   name <- strcpy str
   gid  <- _casm_ ``%r = ((struct group *)%0)->gr_gid;'' ptr
   mem  <- _casm_ ``%r = ((struct group *)%0)->gr_mem;'' ptr
   members <- unvectorize mem 0
   return (GroupEntry name gid members)

-- Copy the static structure returned by getpw* into a Haskell structure

unpackUserEntry :: Addr -> IO UserEntry
unpackUserEntry ptr =
  do
   str   <- _casm_ ``%r = ((struct passwd *)%0)->pw_name;'' ptr
   name    <- strcpy str
   uid   <- _casm_ ``%r = ((struct passwd *)%0)->pw_uid;'' ptr
   gid   <- _casm_ ``%r = ((struct passwd *)%0)->pw_gid;'' ptr
   str   <- _casm_ ``%r = ((struct passwd *)%0)->pw_dir;'' ptr
   home    <- strcpy str
   str   <- _casm_ ``%r = ((struct passwd *)%0)->pw_shell;'' ptr
   shell   <- strcpy str
   return (UserEntry name uid gid home shell)
\end{code}
