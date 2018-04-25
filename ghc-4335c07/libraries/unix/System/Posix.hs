{-# LANGUAGE CPP #-}
{-# LANGUAGE Safe #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Posix
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  non-portable (requires POSIX)
--
-- <http://pubs.opengroup.org/onlinepubs/9699919799/ POSIX.1-2008> support
--
-----------------------------------------------------------------------------

module System.Posix (
  module System.Posix.Types,
  module System.Posix.Signals,
  module System.Posix.Directory,
  module System.Posix.Files,
  module System.Posix.Unistd,
  module System.Posix.IO,
  module System.Posix.Env,
  module System.Posix.Process,
  module System.Posix.Temp,
  module System.Posix.Terminal,
  module System.Posix.Time,
  module System.Posix.User,
  module System.Posix.Resource,
  module System.Posix.Semaphore,
  module System.Posix.SharedMem,
  module System.Posix.DynamicLinker,
-- XXX 'Module' type clashes with GHC
--  module System.Posix.DynamicLinker.Module
 ) where

import System.Posix.Types
import System.Posix.Signals
import System.Posix.Directory
import System.Posix.Files
import System.Posix.Unistd
import System.Posix.Process
import System.Posix.IO
import System.Posix.Env
import System.Posix.Temp
import System.Posix.Terminal
import System.Posix.Time
import System.Posix.User
import System.Posix.Resource
import System.Posix.Semaphore
import System.Posix.SharedMem
-- XXX: bad planning, we have two constructors called "Default"
import System.Posix.DynamicLinker hiding (Default)
--import System.Posix.DynamicLinker.Module

{- TODO

Here we detail our support for the IEEE Std 1003.1-2001 standard.  For
each header file defined by the standard, we categorise its
functionality as

 - "supported"

   Full equivalent functionality is provided by the specified Haskell
   module.

 - "unsupported" (functionality not provided by a Haskell module)

   The functionality is not currently provided.

 - "to be supported"

   Currently unsupported, but support is planned for the future.

Exceptions are listed where appropriate.

Interfaces supported
--------------------

unix package:

dirent.h 	System.Posix.Directory
dlfcn.h 	System.Posix.DynamicLinker
errno.h 	Foreign.C.Error
fcntl.h 	System.Posix.IO
signal.h	System.Posix.Signals
sys/stat.h	System.Posix.Files
sys/times.h	System.Posix.Process
sys/types.h	System.Posix.Types (with exceptions...)
sys/utsname.h	System.Posix.Unistd
sys/wait.h	System.Posix.Process
termios.h	System.Posix.Terminal (check exceptions)
unistd.h	System.Posix.*
utime.h		System.Posix.Files
pwd.h		System.Posix.User
grp.h		System.Posix.User
stdlib.h: 	System.Posix.Env (getenv()/setenv()/unsetenv())
		System.Posix.Temp (mkstemp())
sys/resource.h: System.Posix.Resource (get/setrlimit() only)

regex-posix package:

regex.h		Text.Regex.Posix

network package:

arpa/inet.h
net/if.h
netinet/in.h
netinet/tcp.h
sys/socket.h
sys/un.h

To be supported
---------------

limits.h	(pathconf()/fpathconf() already done)
poll.h
sys/resource.h  (getrusage(): use instead of times() for getProcessTimes?)
sys/select.h
sys/statvfs.h	 (?)
sys/time.h	(but maybe not the itimer?)
time.h		(System.Posix.Time)
stdio.h		(popen only: System.Posix.IO)
sys/mman.h

Unsupported interfaces
----------------------

aio.h
assert.h
complex.h
cpio.h
ctype.h
fenv.h
float.h
fmtmsg.h
fnmatch.h
ftw.h
glob.h
iconv.h
inttypes.h
iso646.h
langinfo.h
libgen.h
locale.h	(see System.Locale)
math.h
monetary.h
mqueue.h
ndbm.h
netdb.h
nl_types.h
pthread.h
sched.h
search.h
semaphore.h
setjmp.h
spawn.h
stdarg.h
stdbool.h
stddef.h
stdint.h
stdio.h		except: popen()
stdlib.h	except: exit(): System.Posix.Process
			free()/malloc(): Foreign.Marshal.Alloc
			getenv()/setenv(): ?? System.Environment
			rand() etc.: System.Random
string.h
strings.h
stropts.h
sys/ipc.h
sys/msg.h
sys/sem.h
sys/shm.h
sys/timeb.h
sys/uio.h
syslog.h
tar.h
tgmath.h
trace.h
ucontext.h
ulimit.h
utmpx.h
wchar.h
wctype.h
wordexp.h

-}
