%
% (c) The GRASP/AQUA Project, Glasgow University, 1995
%
\section[LibPosixDB]{Haskell 1.3 POSIX System Databases}

\begin{code}
module LibPosixDB (
    GroupEntry,
    UserEntry,

    getGroupEntryForID,
    getGroupEntryForName,
    getUserEntryForID,
    getUserEntryForName,
    groupID,
    groupMembers,
    groupName,
    homeDirectory,
    userGroupID,
    userID,
    userName,
    userShell
    ) where

import PreludeGlaST
import PS

import LibPosixUtil

data GroupEntry = GE String GroupID [String]

groupName :: GroupEntry -> String
groupName (GE name _ _) = name

groupID :: GroupEntry -> GroupID
groupID (GE _ gid _) = gid

groupMembers :: GroupEntry -> [String]
groupMembers (GE _ _ members) = members
  
getGroupEntryForID :: GroupID -> IO GroupEntry
getGroupEntryForID gid =
    _ccall_ getgrgid gid		    `thenPrimIO` \ ptr ->
    if ptr == ``NULL'' then
	failWith (NoSuchThing "no such group entry")
    else
	unpackGroupEntry ptr			    `thenPrimIO` \ group ->
	return group

getGroupEntryForName :: String -> IO GroupEntry
getGroupEntryForName name =
    _packBytesForCST name			    `thenStrictlyST` \ gname ->
    _ccall_ getgrnam gname			    `thenPrimIO` \ ptr ->
    if ptr == ``NULL'' then
	failWith (NoSuchThing "no such group entry")
    else
	unpackGroupEntry ptr			    `thenPrimIO` \ group ->
	return group

data UserEntry = UE String UserID GroupID String String

userName :: UserEntry -> String
userName (UE name _ _ _ _) = name

userID :: UserEntry -> UserID
userID (UE _ uid _ _ _) = uid

userGroupID :: UserEntry -> GroupID
userGroupID (UE _ _ gid _ _) = gid

homeDirectory :: UserEntry -> String
homeDirectory (UE _ _ _ home _) = home

userShell :: UserEntry -> String
userShell (UE _ _ _ _ shell) = shell

getUserEntryForID :: UserID -> IO UserEntry
getUserEntryForID uid =
    _ccall_ getpwuid uid			    `thenPrimIO` \ ptr ->
    if ptr == ``NULL'' then
	failWith (NoSuchThing "no such user entry")
    else
	unpackUserEntry ptr			    `thenPrimIO` \ user ->
	return user

getUserEntryForName :: String -> IO UserEntry
getUserEntryForName name =
    _packBytesForCST name			    `thenStrictlyST` \ uname ->
    _ccall_ getpwnam uname			    `thenPrimIO` \ ptr ->
    if ptr == ``NULL'' then
	failWith (NoSuchThing "no such user entry")
    else
	unpackUserEntry ptr			    `thenPrimIO` \ user ->
	return user

\end{code}

Local utility functions

\begin{code}

-- Copy the static structure returned by getgr* into a Haskell structure

unpackGroupEntry :: _Addr -> PrimIO GroupEntry
unpackGroupEntry ptr =
    _casm_ ``%r = ((struct group *)%0)->gr_name;'' ptr
						    `thenPrimIO` \ str ->
    strcpy str			    		    `thenPrimIO` \ name ->
    _casm_ ``%r = ((struct group *)%0)->gr_gid;'' ptr
						    `thenPrimIO` \ gid ->
    _casm_ ``%r = ((struct group *)%0)->gr_mem;'' ptr
						    `thenPrimIO` \ mem ->
    unvectorize mem 0				    `thenStrictlyST` \ members ->
    returnPrimIO (GE name gid members)

-- Copy the static structure returned by getpw* into a Haskell structure

unpackUserEntry :: _Addr -> PrimIO UserEntry
unpackUserEntry ptr =
    _casm_ ``%r = ((struct passwd *)%0)->pw_name;'' ptr
						    `thenPrimIO` \ str ->
    strcpy str					    `thenPrimIO` \ name ->
    _casm_ ``%r = ((struct passwd *)%0)->pw_uid;'' ptr
						    `thenPrimIO` \ uid ->
    _casm_ ``%r = ((struct passwd *)%0)->pw_gid;'' ptr
						    `thenPrimIO` \ gid ->
    _casm_ ``%r = ((struct passwd *)%0)->pw_dir;'' ptr
						    `thenPrimIO` \ str ->
    strcpy str					    `thenPrimIO` \ home ->
    _casm_ ``%r = ((struct passwd *)%0)->pw_shell;'' ptr
						    `thenPrimIO` \ str ->
    strcpy str					    `thenPrimIO` \ shell ->
    returnPrimIO (UE name uid gid home shell)

\end{code}
