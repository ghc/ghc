%
% (c) The GRASP/AQUA Project, Glasgow University, 1997-1998
%
\section{Fast strings}

Compact representations of character strings with
unique identifiers (hash-cons'ish).

\begin{code}
module FastString
       (
	FastString(..),     -- not abstract, for now.

         --names?
        mkFastString,       -- :: String -> FastString
        mkFastSubString,    -- :: Addr -> Int -> Int -> FastString

	-- These ones hold on to the Addr after they return, and aren't hashed; 
	-- they are used for literals
	mkFastCharString,   -- :: Addr -> FastString
	mkFastCharString#,  -- :: Addr# -> FastString
	mkFastCharString2,  -- :: Addr -> Int -> FastString

	mkFastString#,      -- :: Addr# -> Int# -> FastString
        mkFastSubStringBA#, -- :: ByteArray# -> Int# -> Int# -> FastString
        mkFastSubString#,   -- :: Addr# -> Int# -> Int# -> FastString

        mkFastStringInt,    -- :: [Int] -> FastString

        uniqueOfFS,	    -- :: FastString -> Int#
	lengthFS,	    -- :: FastString -> Int
	nullFastString,     -- :: FastString -> Bool

	unpackFS,	    -- :: FastString -> String
	unpackIntFS,	    -- :: FastString -> [Int]
	appendFS,	    -- :: FastString -> FastString -> FastString
        headFS,		    -- :: FastString -> Char
        headIntFS,	    -- :: FastString -> Int
        tailFS,		    -- :: FastString -> FastString
	concatFS,	    -- :: [FastString] -> FastString
        consFS,             -- :: Char -> FastString -> FastString
	indexFS,	    -- :: FastString -> Int -> Char

        hPutFS		    -- :: Handle -> FastString -> IO ()
       ) where

-- This #define suppresses the "import FastString" that
-- HsVersions otherwise produces
#define COMPILING_FAST_STRING
#include "HsVersions.h"

#if __GLASGOW_HASKELL__ < 301
import PackBase
import STBase		( StateAndPtr#(..) )
import IOHandle		( filePtr, readHandle, writeHandle )
import IOBase		( Handle__(..), IOError(..), IOErrorType(..),
		  	  IOResult(..), IO(..),
		  	  constructError
			)
#else
import PrelPack
#if __GLASGOW_HASKELL__ < 400
import PrelST		( StateAndPtr#(..) )
#endif

#if __GLASGOW_HASKELL__ <= 303
import PrelHandle	( readHandle, 
# if __GLASGOW_HASKELL__ < 303
			  filePtr,
# endif
			  writeHandle
			)
#endif

import PrelIOBase	( Handle__(..), IOError, IOErrorType(..),
#if __GLASGOW_HASKELL__ < 400
		  	  IOResult(..), 
#endif
			  IO(..),
#if __GLASGOW_HASKELL__ >= 303
			  Handle__Type(..),
#endif
		  	  constructError
			)
#endif

import PrimPacked
import GlaExts
#if __GLASGOW_HASKELL__ < 411
import PrelAddr		( Addr(..) )
#else
import Addr		( Addr(..) )
import Ptr		( Ptr(..) )
#endif
#if __GLASGOW_HASKELL__ < 407
import MutableArray	( MutableArray(..) )
#else
import PrelArr		( STArray(..), newSTArray )
import IOExts		( hPutBufFull, hPutBufBAFull )
#endif

import IOExts		( IORef, newIORef, readIORef, writeIORef )
import IO
import Char             ( chr, ord )

#define hASH_TBL_SIZE 993

#if __GLASGOW_HASKELL__ >= 400
#define IOok STret
#endif
\end{code} 

@FastString@s are packed representations of strings
with a unique id for fast comparisons. The unique id
is assigned when creating the @FastString@, using
a hash table to map from the character string representation
to the unique ID.

\begin{code}
data FastString
  = FastString   -- packed repr. on the heap.
      Int#       -- unique id
		 --  0 => string literal, comparison
		 --  will
      Int#       -- length
      ByteArray# -- stuff

  | CharStr      -- external C string
      Addr#      -- pointer to the (null-terminated) bytes in C land.
      Int#       -- length  (cached)

  | UnicodeStr   -- if contains characters outside '\1'..'\xFF'
      Int#       -- unique id
      [Int]      -- character numbers

instance Eq FastString where
  a == b = case cmpFS a b of { LT -> False; EQ -> True;  GT -> False }
  a /= b = case cmpFS a b of { LT -> True;  EQ -> False; GT -> True  }

instance Ord FastString where
    a <= b = case cmpFS a b of { LT -> True;  EQ -> True;  GT -> False }
    a <	 b = case cmpFS a b of { LT -> True;  EQ -> False; GT -> False }
    a >= b = case cmpFS a b of { LT -> False; EQ -> True;  GT -> True  }
    a >	 b = case cmpFS a b of { LT -> False; EQ -> False; GT -> True  }
    max x y | x >= y	=  x
            | otherwise	=  y
    min x y | x <= y	=  x
            | otherwise	=  y
    compare a b = cmpFS a b

lengthFS :: FastString -> Int
lengthFS (FastString _ l# _) = I# l#
lengthFS (CharStr a# l#) = I# l#
lengthFS (UnicodeStr _ s) = length s

nullFastString :: FastString -> Bool
nullFastString (FastString _ l# _) = l# ==# 0#
nullFastString (CharStr _ l#) = l# ==# 0#
nullFastString (UnicodeStr _ []) = True
nullFastString (UnicodeStr _ (_:_)) = False

unpackFS :: FastString -> String
unpackFS (FastString _ l# ba#) = unpackNBytesBA# ba# l#
unpackFS (CharStr addr len#) =
 unpack 0#
 where
    unpack nh
      | nh ==# len# = []
      | otherwise   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh
unpackFS (UnicodeStr _ s) = map chr s

unpackIntFS :: FastString -> [Int]
unpackIntFS (UnicodeStr _ s) = s
unpackIntFS fs = map ord (unpackFS fs)

appendFS :: FastString -> FastString -> FastString
appendFS fs1 fs2 = mkFastStringInt (unpackIntFS fs1 ++ unpackIntFS fs2)

concatFS :: [FastString] -> FastString
concatFS ls = mkFastStringInt (concat (map unpackIntFS ls)) -- ToDo: do better

headFS :: FastString -> Char
headFS (FastString _ l# ba#) = 
 if l# ># 0# then C# (indexCharArray# ba# 0#) else error ("headFS: empty FS")
headFS (CharStr a# l#) = 
 if l# ># 0# then C# (indexCharOffAddr# a# 0#) else error ("headFS: empty FS")
headFS (UnicodeStr _ (c:_)) = chr c
headFS (UnicodeStr _ []) = error ("headFS: empty FS")

headIntFS :: FastString -> Int
headIntFS (UnicodeStr _ (c:_)) = c
headIntFS fs = ord (headFS fs)

indexFS :: FastString -> Int -> Char
indexFS f i@(I# i#) =
 case f of
   FastString _ l# ba#
     | l# ># 0# && l# ># i#  -> C# (indexCharArray# ba# i#)
     | otherwise	     -> error (msg (I# l#))
   CharStr a# l#
     | l# ># 0# && l# ># i#  -> C# (indexCharOffAddr# a# i#)
     | otherwise	     -> error (msg (I# l#))
   UnicodeStr _ s	     -> chr (s!!i)
 where
  msg l =  "indexFS: out of range: " ++ show (l,i)

tailFS :: FastString -> FastString
tailFS (FastString _ l# ba#) = mkFastSubStringBA# ba# 1# (l# -# 1#)
tailFS fs = mkFastStringInt (tail (unpackIntFS fs))

consFS :: Char -> FastString -> FastString
consFS c fs = mkFastStringInt (ord c : unpackIntFS fs)

uniqueOfFS :: FastString -> Int#
uniqueOfFS (FastString u# _ _) = u#
uniqueOfFS (CharStr a# l#)     = case mkFastString# a# l# of { FastString u# _ _ -> u#} -- Ugh!
   {-
     [A somewhat moby hack]: to avoid entering all sorts
     of junk into the hash table, all C char strings
     are by default left out. The benefit of being in
     the table is that string comparisons are lightning fast,
     just an Int# comparison.
   
     But, if you want to get the Unique of a CharStr, we 
     enter it into the table and return that unique. This
     works, but causes the CharStr to be looked up in the hash
     table each time it is accessed..
   -}
uniqueOfFS (UnicodeStr u# _) = u#
\end{code}

Internally, the compiler will maintain a fast string symbol
table, providing sharing and fast comparison. Creation of
new @FastString@s then covertly does a lookup, re-using the
@FastString@ if there was a hit.

Caution: mkFastStringUnicode assumes that if the string is in the
table, it sits under the UnicodeStr constructor. Other mkFastString
variants analogously assume the FastString constructor.

\begin{code}
data FastStringTable = 
 FastStringTable
    Int#
    (MutableArray# RealWorld [FastString])

type FastStringTableVar = IORef FastStringTable

string_table :: FastStringTableVar
string_table = 
 unsafePerformIO (
#if __GLASGOW_HASKELL__ < 405
   stToIO (newArray (0::Int,hASH_TBL_SIZE) [])
	>>= \ (MutableArray _ arr#) ->
#elif __GLASGOW_HASKELL__ < 407
   stToIO (newArray (0::Int,hASH_TBL_SIZE) [])
	>>= \ (MutableArray _ _ arr#) ->
#else
   stToIO (newSTArray (0::Int,hASH_TBL_SIZE) [])
	>>= \ (STArray _ _ arr#) ->
#endif
   newIORef (FastStringTable 0# arr#))

lookupTbl :: FastStringTable -> Int# -> IO [FastString]
lookupTbl (FastStringTable _ arr#) i# =
  IO ( \ s# ->
#if __GLASGOW_HASKELL__ < 400
  case readArray# arr# i# s# of { StateAndPtr# s2# r ->
  IOok s2# r })
#else
  readArray# arr# i# s#)
#endif

updTbl :: FastStringTableVar -> FastStringTable -> Int# -> [FastString] -> IO ()
updTbl fs_table_var (FastStringTable uid# arr#) i# ls =
 IO (\ s# -> case writeArray# arr# i# ls s# of { s2# -> 
#if __GLASGOW_HASKELL__ < 400
	IOok s2# () })	>>
#else
	(# s2#, () #) }) >>
#endif
 writeIORef fs_table_var (FastStringTable (uid# +# 1#) arr#)

mkFastString# :: Addr# -> Int# -> FastString
mkFastString# a# len# =
 unsafePerformIO  (
  readIORef string_table	>>= \ ft@(FastStringTable uid# tbl#) ->
  let
   h = hashStr a# len#
  in
--  _trace ("hashed: "++show (I# h)) $
  lookupTbl ft h	>>= \ lookup_result ->
  case lookup_result of
    [] -> 
       -- no match, add it to table by copying out the
       -- the string into a ByteArray
       -- _trace "empty bucket" $
       case copyPrefixStr (A# a#) (I# len#) of
#if __GLASGOW_HASKELL__ < 405
	 (ByteArray _ barr#) ->  
#else
	 (ByteArray _ _ barr#) ->  
#endif
	   let f_str = FastString uid# len# barr# in
           updTbl string_table ft h [f_str] >>
           ({- _trace ("new: " ++ show f_str)   $ -} return f_str)
    ls -> 
       -- non-empty `bucket', scan the list looking
       -- entry with same length and compare byte by byte.
       -- _trace ("non-empty bucket"++show ls) $
       case bucket_match ls len# a# of
	 Nothing -> 
           case copyPrefixStr (A# a#) (I# len#) of
#if __GLASGOW_HASKELL__ < 405
	     (ByteArray _ barr#) ->  
#else
	     (ByteArray _ _ barr#) ->  
#endif
              let f_str = FastString uid# len# barr# in
              updTbl string_table ft h (f_str:ls) >>
	      ( {- _trace ("new: " ++ show f_str)  $ -} return f_str)
	 Just v  -> {- _trace ("re-use: "++show v) $ -} return v)
  where
   bucket_match [] _ _ = Nothing
   bucket_match (v@(FastString _ l# ba#):ls) len# a# =
      if len# ==# l# && eqStrPrefix a# ba# l# then
	 Just v
      else
	 bucket_match ls len# a#
   bucket_match (UnicodeStr _ _ : ls) len# a# =
      bucket_match ls len# a#

mkFastSubString# :: Addr# -> Int# -> Int# -> FastString
mkFastSubString# a# start# len# = mkFastCharString2 (A# (addrOffset# a# start#)) (I# len#)

mkFastSubStringBA# :: ByteArray# -> Int# -> Int# -> FastString
mkFastSubStringBA# barr# start# len# =
 unsafePerformIO  (
  readIORef string_table	>>= \ ft@(FastStringTable uid# tbl#) ->
  let
   h = hashSubStrBA barr# start# len#
  in
--  _trace ("hashed(b): "++show (I# h)) $
  lookupTbl ft h		>>= \ lookup_result ->
  case lookup_result of
    [] -> 
       -- no match, add it to table by copying out the
       -- the string into a ByteArray
       -- _trace "empty bucket(b)" $
#if __GLASGOW_HASKELL__ < 405
       case copySubStrBA (ByteArray btm barr#) (I# start#) (I# len#) of
         (ByteArray _ ba#) ->  
#else
       case copySubStrBA (ByteArray btm btm barr#) (I# start#) (I# len#) of
         (ByteArray _ _ ba#) ->  
#endif
          let f_str = FastString uid# len# ba# in
          updTbl string_table ft h [f_str]     >>
          -- _trace ("new(b): " ++ show f_str)   $
	  return f_str
    ls -> 
       -- non-empty `bucket', scan the list looking
       -- entry with same length and compare byte by byte. 
       -- _trace ("non-empty bucket(b)"++show ls) $
       case bucket_match ls start# len# barr# of
	 Nothing -> 
#if __GLASGOW_HASKELL__ < 405
          case copySubStrBA (ByteArray btm barr#) (I# start#) (I# len#) of
            (ByteArray _ ba#) ->  
#else
          case copySubStrBA (ByteArray btm btm barr#) (I# start#) (I# len#) of
            (ByteArray _ _ ba#) ->  
#endif
              let f_str = FastString uid# len# ba# in
              updTbl string_table ft h (f_str:ls) >>
	      -- _trace ("new(b): " ++ show f_str)   $
	      return f_str
	 Just v  -> 
              -- _trace ("re-use(b): "++show v) $
	      return v
  )
 where
   btm = error ""

   bucket_match [] _ _ _ = Nothing
   bucket_match (v:ls) start# len# ba# =
    case v of
     FastString _ l# barr# ->
      if len# ==# l# && eqStrPrefixBA barr# ba# start# len# then
	 Just v
      else
	 bucket_match ls start# len# ba#
     UnicodeStr _ _ -> bucket_match ls start# len# ba#

mkFastStringUnicode :: [Int] -> FastString
mkFastStringUnicode s =
 unsafePerformIO  (
  readIORef string_table	>>= \ ft@(FastStringTable uid# tbl#) ->
  let
   h = hashUnicode s
  in
--  _trace ("hashed(b): "++show (I# h)) $
  lookupTbl ft h		>>= \ lookup_result ->
  case lookup_result of
    [] -> 
       -- no match, add it to table by copying out the
       -- the string into a [Int]
          let f_str = UnicodeStr uid# s in
          updTbl string_table ft h [f_str]     >>
          -- _trace ("new(b): " ++ show f_str)   $
	  return f_str
    ls -> 
       -- non-empty `bucket', scan the list looking
       -- entry with same length and compare byte by byte. 
       -- _trace ("non-empty bucket(b)"++show ls) $
       case bucket_match ls of
	 Nothing -> 
              let f_str = UnicodeStr uid# s in
              updTbl string_table ft h (f_str:ls) >>
	      -- _trace ("new(b): " ++ show f_str)   $
	      return f_str
	 Just v  -> 
              -- _trace ("re-use(b): "++show v) $
	      return v
  )
 where
   bucket_match [] = Nothing
   bucket_match (v@(UnicodeStr _ s'):ls) =
       if s' == s then Just v else bucket_match ls
   bucket_match (FastString _ _ _ : ls) = bucket_match ls

mkFastCharString :: Addr -> FastString
mkFastCharString a@(A# a#) = 
 case strLength a of{ (I# len#) -> CharStr a# len# }

mkFastCharString# :: Addr# -> FastString
mkFastCharString# a# = 
 case strLength (A# a#) of { (I# len#) -> CharStr a# len# }

mkFastCharString2 :: Addr -> Int -> FastString
mkFastCharString2 a@(A# a#) (I# len#) = CharStr a# len#

mkFastStringNarrow :: String -> FastString
mkFastStringNarrow str =
 case packString str of
#if __GLASGOW_HASKELL__ < 405
  (ByteArray (_,I# len#) frozen#) -> 
#else
  (ByteArray _ (I# len#) frozen#) -> 
#endif
    mkFastSubStringBA# frozen# 0# len#
    {- 0-indexed array, len# == index to one beyond end of string,
       i.e., (0,1) => empty string.    -}

mkFastString :: String -> FastString
mkFastString str = if all good str
    then mkFastStringNarrow str
    else mkFastStringUnicode (map ord str)
    where
    good c = c >= '\1' && c <= '\xFF'

mkFastStringInt :: [Int] -> FastString
mkFastStringInt str = if all good str
    then mkFastStringNarrow (map chr str)
    else mkFastStringUnicode str
    where
    good c = c >= 1 && c <= 0xFF

mkFastSubString :: Addr -> Int -> Int -> FastString
mkFastSubString (A# a#) (I# start#) (I# len#) =
 mkFastString# (addrOffset# a# start#) len#
\end{code}

\begin{code}
hashStr  :: Addr# -> Int# -> Int#
 -- use the Addr to produce a hash value between 0 & m (inclusive)
hashStr a# len# =
  case len# of
   0# -> 0#
   1# -> ((ord# c0 *# 631#) +# len#) `remInt#` hASH_TBL_SIZE#
   2# -> ((ord# c0 *# 631#) +# (ord# c1 *# 217#) +# len#) `remInt#` hASH_TBL_SIZE#
   _  -> ((ord# c0 *# 631#) +# (ord# c1 *# 217#) +# (ord# c2 *# 43#) +# len#) `remInt#` hASH_TBL_SIZE#
  where
    c0 = indexCharOffAddr# a# 0#
    c1 = indexCharOffAddr# a# (len# `quotInt#` 2# -# 1#)
    c2 = indexCharOffAddr# a# (len# -# 1#)
{-
    c1 = indexCharOffAddr# a# 1#
    c2 = indexCharOffAddr# a# 2#
-}

hashSubStrBA  :: ByteArray# -> Int# -> Int# -> Int#
 -- use the byte array to produce a hash value between 0 & m (inclusive)
hashSubStrBA ba# start# len# =
  case len# of
   0# -> 0#
   1# -> ((ord# c0 *# 631#) +# len#) `remInt#` hASH_TBL_SIZE#
   2# -> ((ord# c0 *# 631#) +# (ord# c1 *# 217#) +# len#) `remInt#` hASH_TBL_SIZE#
   _  -> ((ord# c0 *# 631#) +# (ord# c1 *# 217#) +# (ord# c2 *# 43#) +# len#) `remInt#` hASH_TBL_SIZE#
  where
    c0 = indexCharArray# ba# 0#
    c1 = indexCharArray# ba# (len# `quotInt#` 2# -# 1#)
    c2 = indexCharArray# ba# (len# -# 1#)

--    c1 = indexCharArray# ba# 1#
--    c2 = indexCharArray# ba# 2#

hashUnicode :: [Int] -> Int#
 -- use the Addr to produce a hash value between 0 & m (inclusive)
hashUnicode [] = 0#
hashUnicode [I# c0] = ((c0 *# 631#) +# 1#) `remInt#` hASH_TBL_SIZE#
hashUnicode [I# c0, I# c1] = ((c0 *# 631#) +# (c1 *# 217#) +# 2#) `remInt#` hASH_TBL_SIZE#
hashUnicode s = ((c0 *# 631#) +# (c1 *# 217#) +# (c2 *# 43#) +# len#) `remInt#` hASH_TBL_SIZE#
  where
    I# len# = length s
    I# c0 = s !! 0
    I# c1 = s !! (I# (len# `quotInt#` 2# -# 1#))
    I# c2 = s !! (I# (len# -# 1#))

\end{code}

\begin{code}
cmpFS :: FastString -> FastString -> Ordering
cmpFS (UnicodeStr u1# s1) (UnicodeStr u2# s2) = if u1# ==# u2# then EQ
    else compare s1 s2
cmpFS (UnicodeStr _ s1) s2 = compare s1 (unpackIntFS s2)
cmpFS s1 (UnicodeStr _ s2) = compare (unpackIntFS s1) s2
cmpFS (FastString u1# _ b1#) (FastString u2# _ b2#) = -- assume non-null chars
  if u1# ==# u2# then
     EQ
  else
   unsafePerformIO (
#if __GLASGOW_HASKELL__ < 405
    _ccall_ strcmp (ByteArray bot b1#) (ByteArray bot b2#)	>>= \ (I# res) ->
#else
    _ccall_ strcmp (ByteArray bot bot b1#) (ByteArray bot bot b2#) >>= \ (I# res) ->
#endif
    return (
    if      res <#  0# then LT
    else if res ==# 0# then EQ
    else		    GT
    ))
  where
#if __GLASGOW_HASKELL__ < 405
   bot :: (Int,Int)
#else
   bot :: Int
#endif
   bot = error "tagCmp"
cmpFS (CharStr bs1 len1) (CharStr bs2 len2)
  = unsafePerformIO (
    _ccall_ strcmp ba1 ba2	>>= \ (I# res) ->
    return (
    if      res <#  0# then LT
    else if res ==# 0# then EQ
    else		    GT
    ))
  where
    ba1 = A# bs1
    ba2 = A# bs2
cmpFS (FastString _ len1 bs1) (CharStr bs2 len2)
 = unsafePerformIO (
    _ccall_ strcmp ba1 ba2	>>= \ (I# res) ->
    return (
     if      res <#  0# then LT
     else if res ==# 0# then EQ
     else		     GT
    ))
  where
#if __GLASGOW_HASKELL__ < 405
    ba1 = ByteArray ((error "")::(Int,Int)) bs1
#else
    ba1 = ByteArray (error "") ((error "")::Int) bs1
#endif
    ba2 = A# bs2

cmpFS a@(CharStr _ _) b@(FastString _ _ _)
  = -- try them the other way 'round
    case (cmpFS b a) of { LT -> GT; EQ -> EQ; GT -> LT }

\end{code}

Outputting @FastString@s is quick, just block copying the chunk (using
@fwrite@).

\begin{code}
hPutFS :: Handle -> FastString -> IO ()
#if __GLASGOW_HASKELL__ <= 302
hPutFS handle (FastString _ l# ba#) =
 if l# ==# 0# then
    return ()
 else
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
          fail ioError
      ClosedHandle ->
	  writeHandle handle htype		    >>
	  fail MkIOError(handle,IllegalOperation,"handle is closed")
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
	  fail MkIOError(handle,IllegalOperation,"handle is closed")
      ReadHandle _ _ _ ->
	  writeHandle handle htype		    >>
	  fail MkIOError(handle,IllegalOperation,"handle is not open for writing")
      other -> 
          let fp = filePtr htype in
	   -- here we go..
#if __GLASGOW_HASKELL__ < 405
          _ccall_ writeFile (ByteArray ((error "")::(Int,Int)) ba#) fp (I# l#) >>= \rc ->
#else
          _ccall_ writeFile (ByteArray ((error "")::Int) ((error "")::Int) ba#) fp (I# l#) >>= \rc ->
#endif
          if rc==0 then
              return ()
          else
              constructError "hPutFS"   >>= \ err ->
	      fail err
hPutFS handle (CharStr a# l#) =
 if l# ==# 0# then
    return ()
 else
    readHandle handle				    >>= \ htype ->
    case htype of 
      ErrorHandle ioError ->
	  writeHandle handle htype		    >>
          fail ioError
      ClosedHandle ->
	  writeHandle handle htype		    >>
	  fail MkIOError(handle,IllegalOperation,"handle is closed")
      SemiClosedHandle _ _ ->
	  writeHandle handle htype		    >>
	  fail MkIOError(handle,IllegalOperation,"handle is closed")
      ReadHandle _ _ _ ->
	  writeHandle handle htype		    >>
	  fail MkIOError(handle,IllegalOperation,"handle is not open for writing")
      other -> 
          let fp = filePtr htype in
	   -- here we go..
          _ccall_ writeFile (A# a#) fp (I# l#)	>>= \rc ->
          if rc==0 then
              return ()
          else
              constructError "hPutFS"   	>>= \ err ->
	      fail err


#else
hPutFS handle (FastString _ l# ba#)
  | l# ==# 0#  = return ()
#if __GLASGOW_HASKELL__ < 405
  | otherwise  = hPutBufBA handle (ByteArray bot ba#) (I# l#)
#elif __GLASGOW_HASKELL__ < 407
  | otherwise  = hPutBufBA handle (ByteArray bot bot ba#) (I# l#)
#else
  | otherwise  = do mba <- stToIO $ unsafeThawByteArray (ByteArray (bot::Int) bot ba#)
                    hPutBufBAFull  handle mba (I# l#)
#endif
 where
  bot = error "hPutFS.ba"

--ToDo: avoid silly code duplic.

hPutFS handle (CharStr a# l#)
  | l# ==# 0#  = return ()
#if __GLASGOW_HASKELL__ < 407
  | otherwise  = hPutBuf handle (A# a#) (I# l#)
#elif __GLASGOW_HASKELL__ < 411
  | otherwise  = hPutBufFull handle (A# a#) (I# l#)
#else
  | otherwise  = hPutBufFull handle (Ptr a#) (I# l#)
#endif

#endif
\end{code}
