%
% (c) The AQUA Project, Glasgow University, 1994-1996
%
\section[IOExts]{Module @IOExts@}

@IOExts@ provides useful functionality that fall outside the
standard Haskell IO interface. Expect the contents of IOExts
to be the same for Hugs and GHC (same goes for any other
Hugs/GHC extension libraries, unless a function/type is
explicitly flagged as being implementation specific
extension.)

\begin{code}
{-# OPTIONS -fno-implicit-prelude #-}

module IOExts
        ( fixIO
        , unsafePerformIO
        , unsafeInterleaveIO

        , IORef		    -- instance of: Eq
        , newIORef
        , readIORef
        , writeIORef
	, updateIORef

	, mkWeakIORef

	, IOArray	-- instance of: Eq
	, newIOArray
	, boundsIOArray
	, readIOArray
	, writeIOArray
	, freezeIOArray
	, thawIOArray
#ifndef __HUGS__
	, unsafeFreezeIOArray
	, unsafeThawIOArray
#endif
	
#ifdef __HUGS__
#else
	, openFileEx
	, IOModeEx(..)

        , hSetEcho
	, hGetEcho
	, hIsTerminalDevice
	, hConnectTo
	, withHandleFor
	, withStdout
	, withStdin
	, withStderr
#endif
        , trace
#ifdef __HUGS__
#else
        , performGC
#endif
	
	, unsafePtrEq
	
	, freeHaskellFunctionPtr
	
	, HandlePosition
	, HandlePosn(..)
	, hTell                -- :: Handle -> IO HandlePosition
	
	, hSetBinaryMode       -- :: Handle -> Bool -> IO Bool

        ) where

\end{code}

\begin{code}
#ifdef __HUGS__
import PreludeBuiltin
import ST
#else
import PrelBase
import PrelIOBase
import IO
import PrelHandle ( openFileEx, IOModeEx(..),
		    hSetEcho, hGetEcho, getHandleFd
		  )
import PrelST
import PrelArr
import PrelWeak
import PrelGHC
import PrelHandle
import PrelErr
import IO 	( hPutStr, hPutChar )
import PrelAddr ( Addr )
#endif
import Ix

unsafePtrEq :: a -> a -> Bool

#ifdef __HUGS__
unsafePtrEq = primReallyUnsafePtrEquality
#else
unsafePtrEq a b =
    case reallyUnsafePtrEquality# a b of
	 0# -> False
	 _  -> True
#endif
\end{code}

\begin{code}
newIORef    :: a -> IO (IORef a)
readIORef   :: IORef a -> IO a
writeIORef  :: IORef a -> a -> IO ()

#ifdef __HUGS__
type IORef a = STRef RealWorld a
newIORef   = newSTRef
readIORef  = readSTRef
writeIORef = writeSTRef
#else
newtype IORef a = IORef (MutableVar RealWorld a) 
    deriving Eq

newIORef v = stToIO (newVar v) >>= \ var -> return (IORef var)
readIORef  (IORef var) = stToIO (readVar var)
writeIORef (IORef var) v = stToIO (writeVar var v)
#endif

updateIORef :: IORef a -> (a -> a) -> IO ()
updateIORef ref f = do
  x <- readIORef ref
  let x' = f x
  writeIORef ref x'
  -- or should we return new value ? (or old?)

mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))
mkWeakIORef r@(IORef (MutableVar r#)) f = IO $ \s ->
  case mkWeak# r# r f s of (# s1, w #) -> (# s1, Weak w #)
\end{code}

\begin{code}
newIOArray          :: Ix ix => (ix,ix) -> elt -> IO (IOArray ix elt)
boundsIOArray       :: Ix ix => IOArray ix elt -> (ix, ix)
readIOArray         :: Ix ix => IOArray ix elt -> ix -> IO elt
writeIOArray        :: Ix ix => IOArray ix elt -> ix -> elt -> IO ()
freezeIOArray       :: Ix ix => IOArray ix elt -> IO (Array ix elt)
thawIOArray	    :: Ix ix => Array ix elt -> IO (IOArray ix elt)
#ifndef __HUGS__
unsafeFreezeIOArray :: Ix ix => IOArray ix elt -> IO (Array ix elt)
unsafeThawIOArray   :: Ix ix => Array ix elt -> IO (IOArray ix elt)
#endif

#ifdef __HUGS__
type IOArray ix elt = STArray RealWorld ix elt
newIOArray    = newSTArray
boundsIOArray = boundsSTArray
readIOArray   = readSTArray
writeIOArray  = writeSTArray
freezeIOArray = freezeSTArray
thawIOArray   = thawSTArray
#else
newtype IOArray ix elt = IOArray (MutableArray RealWorld ix elt)
    deriving Eq

newIOArray ixs elt = 
    stToIO (newArray ixs elt) >>= \arr -> 
    return (IOArray arr)

boundsIOArray (IOArray arr) = boundsOfArray arr

readIOArray (IOArray arr) ix = stToIO (readArray arr ix)

writeIOArray (IOArray arr) ix elt = stToIO (writeArray arr ix elt)

freezeIOArray (IOArray arr) = stToIO (freezeArray arr)

thawIOArray arr = do 
	marr <- stToIO (thawArray arr)
	return (IOArray marr)

unsafeFreezeIOArray (IOArray arr) = stToIO (unsafeFreezeArray arr)
unsafeThawIOArray   arr = do
        marr <- stToIO (unsafeThawArray arr)
	return (IOArray marr)
#endif
\end{code}

\begin{code}
{-# NOINLINE trace #-}
trace :: String -> a -> a
#ifdef __HUGS__
trace string expr = unsafePerformIO $ do
    putStrLn string
    return expr
#else
trace string expr = unsafePerformIO $ do
    fd <- getHandleFd stderr
    hPutStr stderr string
    hPutChar stderr '\n'
    postTraceHook fd
    return expr

foreign import "PostTraceHook" postTraceHook :: Int -> IO ()
#endif

\end{code}

Not something you want to call normally, but useful
in the cases where you do want to flush stuff out of
the heap or make sure you've got room enough

\begin{code}
#ifndef __HUGS__
foreign import "performGC" performGC :: IO ()
#endif
\end{code}

When using 'foreign export dynamic' to dress up a Haskell
IO action to look like a C function pointer, a little bit
of memory is allocated (along with a stable pointer to
the Haskell IO action). When done with the C function
pointer, you'll need to call @freeHaskellFunctionPtr()@ to
let go of these resources - here's the Haskell wrapper for
that RTS entry point, should you want to free it from
within Haskell.

\begin{code}
foreign import ccall "freeHaskellFunctionPtr" 
  freeHaskellFunctionPtr :: Addr -> IO ()

\end{code}

(Experimental) 

Support for redirecting I/O on a handle to another for the
duration of an IO action. To re-route a handle, it is first
flushed, followed by replacing its innards (i.e., FILE_OBJECT)
with that of the other. This happens before and after the
action is executed.

If the action raises an exception, the handle is replaced back
to its old contents, but without flushing it first - as this
may provoke exceptions. Notice that the action may perform
I/O on either Handle, with the result that the I/O is interleaved.
(Why you would want to do this, is a completely different matter.)

ToDo: probably want to restrict what kind of handles can be
replaced with another - i.e., don't want to be able to replace
a writeable handle with a readable one.

\begin{code}
withHandleFor :: Handle
	      -> Handle
	      -> IO a
	      -> IO a
withHandleFor h1 h2 act = do
   h1_fo <- getFO h1
   plugIn h1_fo
 where
  plugIn h1_fo = do
    hFlush h2
    h2_fo <- withHandle h2 $ \ h2_ -> return (h2_{haFO__=h1_fo}, haFO__ h2_)
    catch (act >>= \ x -> hFlush h2 >> setFO h2 h2_fo >> return x)
    	  (\ err -> setFO h2 h2_fo >> ioError err)

  setFO h fo = 
    withHandle h $ \ h_ -> return (h_{haFO__=fo}, ())

  getFO h = 
    wantRWHandle "withHandleFor" h $ \ h_ ->
    return (haFO__ h_)
        
\end{code}

Derived @withHandleFor@ combinators and, at the moment, these
are exported from @IOExts@ and not @withHandleFor@ itself.

\begin{code}
withStdin  h a = withHandleFor h stdin  a
withStdout h a = withHandleFor h stdout a
withStderr h a = withHandleFor h stderr a
\end{code}

@hTell@ is the lower-level version of @hGetPosn@ - return the
position, without bundling it together with the handle itself:

\begin{code}
hTell :: Handle -> IO HandlePosition
hTell h = do
  (HandlePosn _ x) <- hGetPosn h
  return x
\end{code}

@hSetBinaryMode@ lets you change the translation mode for a handle.
On some platforms (e.g., Win32) a distinction is made between being in
'text mode' or 'binary mode', with the former terminating lines
by \r\n rather than just \n.

Debating the Winnitude or otherwise of such a scheme is less than
interesting -- it's there, so we have to cope.

A side-effect of calling @hSetBinaryMode@ is that the output buffer
(if any) is flushed prior to changing the translation mode.

\begin{code}
hSetBinaryMode :: Handle -> Bool -> IO Bool
hSetBinaryMode handle is_binary = do 
        -- is_binary = True => set translation mode to binary.
    wantRWHandle "hSetBinaryMode" handle $ \ handle_ -> do
    let fo = haFO__ handle_
    rc	    <- setBinaryMode fo flg
    if rc >= 0 then 
       return (int2Bool rc)
     else
       constructErrorAndFail "hSetBinaryMode"
  where
   flg | is_binary = 1
       | otherwise = 0

   int2Bool 0 = False
   int2Bool _ = True

\end{code}
