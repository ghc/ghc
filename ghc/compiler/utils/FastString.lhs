%
% (c) The GRASP/AQUA Project, Glasgow University, 1997
%
\section{Fast strings}

Compact representations of character strings with
unique identifiers.

\begin{code}
module FastString
       (
	FastString(..),     -- not abstract, for now.

         --names?
        mkFastString,       -- :: String -> FastString
	mkFastCharString,   -- :: _Addr -> FastString
	mkFastCharString2,  -- :: _Addr -> Int -> FastString
        mkFastSubString,    -- :: _Addr -> Int -> Int -> FastString
        mkFastSubStringFO,  -- :: ForeignObj -> Int -> Int -> FastString

	mkFastString#,      -- :: Addr# -> Int# -> FastString
        mkFastSubStringBA#, -- :: ByteArray# -> Int# -> Int# -> FastString
        mkFastSubString#,   -- :: Addr# -> Int# -> Int# -> FastString
        mkFastSubStringFO#, -- :: ForeignObj# -> Int# -> Int# -> FastString
       
	lengthFS,	    -- :: FastString -> Int
	nullFastString,     -- :: FastString -> Bool

	getByteArray#,	    -- :: FastString -> ByteArray#
        getByteArray,       -- :: FastString -> _ByteArray Int
	unpackFS,	    -- :: FastString -> String
	appendFS,	    -- :: FastString -> FastString -> FastString
        headFS,		    -- :: FastString -> Char
        tailFS,		    -- :: FastString -> FastString
	concatFS,	    -- :: [FastString] -> FastString
        consFS,             -- :: Char -> FastString -> FastString

        hPutFS,		    -- :: Handle -> FastString -> IO ()
        tagCmpFS	    -- :: FastString -> FastString -> _CMP_TAG
       ) where

import PreludeGlaST
import PreludeGlaMisc
import HandleHack

import PrimPacked
import Ubiq

#define hASH_TBL_SIZE 993

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

instance Eq FastString where
  a == b = case tagCmpFS a b of { _LT -> False; _EQ -> True;  _GT -> False }
  a /= b = case tagCmpFS a b of { _LT -> True;  _EQ -> False; _GT -> True  }

{-
 (FastString u1# _ _) == (FastString u2# _ _) = u1# ==# u2#
-}

instance Uniquable FastString where
 uniqueOf (FastString u# _ _) = mkUniqueGrimily u#
 uniqueOf (CharStr a# l#) =
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
   mkUniqueGrimily (case mkFastString# a# l# of { FastString u# _ _ -> u#}) -- Ugh!

instance Uniquable Int where
 uniqueOf (I# i#) = mkUniqueGrimily i#

instance Text FastString  where
    readsPrec p = error "readsPrec: FastString: ToDo"
    showsPrec p ps@(FastString u# _ _) r = showsPrec p (unpackFS ps) r
    showsPrec p ps r = showsPrec p (unpackFS ps) r

getByteArray# :: FastString -> ByteArray#
getByteArray# (FastString _ _ ba#) = ba#

getByteArray :: FastString -> _ByteArray Int
getByteArray (FastString _ l# ba#) = _ByteArray (0,I# l#) ba#

lengthFS :: FastString -> Int
lengthFS (FastString _ l# _) = I# l#
lengthFS (CharStr a# l#) = I# l#

nullFastString :: FastString -> Bool
nullFastString (FastString _ l# _) = l# ==# 0#
nullFastString (CharStr _ l#) = l# ==# 0#

unpackFS :: FastString -> String
unpackFS (FastString _ l# ba#) = byteArrayToString (_ByteArray (0,I# l#) ba#)
unpackFS (CharStr addr len#) =
 unpack 0#
 where
    unpack nh
      | nh ==# len# = []
      | otherwise   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh

appendFS :: FastString -> FastString -> FastString
appendFS fs1 fs2 = mkFastString (unpackFS fs1 ++ unpackFS fs2)

concatFS :: [FastString] -> FastString
concatFS ls = mkFastString (concat (map (unpackFS) ls)) -- ToDo: do better

headFS :: FastString -> Char
headFS f@(FastString _ l# ba#) = 
 if l# ># 0# then C# (indexCharArray# ba# 0#) else error ("headFS: empty FS: " ++ unpackFS f)
headFS f@(CharStr a# l#) = 
 if l# ># 0# then C# (indexCharOffAddr# a# 0#) else error ("headFS: empty FS: " ++ unpackFS f)

tailFS :: FastString -> FastString
tailFS (FastString _ l# ba#) = mkFastSubStringBA# ba# 1# (l# -# 1#)

consFS :: Char -> FastString -> FastString
consFS c fs = mkFastString (c:unpackFS fs)

\end{code}

Internally, the compiler will maintain a fast string symbol
table, providing sharing and fast comparison. Creation of
new @FastString@s then covertly does a lookup, re-using the
@FastString@ if there was a hit.

\begin{code}
data FastStringTable = 
 FastStringTable
    Int#
    (MutableArray# _RealWorld [FastString])

type FastStringTableVar = MutableVar _RealWorld FastStringTable

string_table :: FastStringTableVar
string_table = 
 unsafePerformPrimIO (
   newArray (0::Int,hASH_TBL_SIZE) [] `thenPrimIO` \ (_MutableArray _ arr#) ->
   newVar (FastStringTable 0# arr#))

lookupTbl :: FastStringTable -> Int# -> [FastString]
lookupTbl (FastStringTable _ arr#) i# =
 unsafePerformPrimIO ( \ (S# s#) ->
   case readArray# arr# i# s# of { StateAndPtr# s2# r ->
    (r, S# s2#) } )

updTbl :: FastStringTableVar -> FastStringTable -> Int# -> [FastString] -> PrimIO ()
updTbl (_MutableArray _ var#) (FastStringTable uid# arr#) i# ls (S# s#) =
 case writeArray# arr# i# ls s# of { s2# ->
 case writeArray# var# 0# (FastStringTable (uid# +# 1#) arr#) s2# of { s3# ->
  ((), S# s3#) }}

mkFastString# :: Addr# -> Int# -> FastString
mkFastString# a# len# =
 unsafePerformPrimIO  (
  readVar string_table `thenPrimIO` \ ft@(FastStringTable uid# tbl#) ->
  let
   h = hashStr a# len#
  in
--  _trace ("hashed: "++show (I# h)) $
  case lookupTbl ft h of
    [] -> 
       -- no match, add it to table by copying out the
       -- the string into a ByteArray
       -- _trace "empty bucket" $
       case copyPrefixStr (A# a#) (I# len#) of
	 (_ByteArray _ barr#) ->  
	   let f_str = FastString uid# len# barr# in
           updTbl string_table ft h [f_str] `seqPrimIO`
           ({- _trace ("new: " ++ show f_str)   $ -} returnPrimIO f_str)
    ls -> 
       -- non-empty `bucket', scan the list looking
       -- entry with same length and compare byte by byte.
       -- _trace ("non-empty bucket"++show ls) $
       case bucket_match ls len# a# of
	 Nothing -> 
           case copyPrefixStr (A# a#) (I# len#) of
  	    (_ByteArray _ barr#) ->  
              let f_str = FastString uid# len# barr# in
              updTbl string_table ft h (f_str:ls) `seqPrimIO`
	      ( {- _trace ("new: " ++ show f_str)  $ -} returnPrimIO f_str)
	 Just v  -> {- _trace ("re-use: "++show v) $ -} returnPrimIO v)
  where
   bucket_match [] _ _ = Nothing
   bucket_match (v@(FastString _ l# ba#):ls) len# a# =
      if len# ==# l# && eqStrPrefix a# ba# l# then
	 Just v
      else
	 bucket_match ls len# a#

mkFastSubString# :: Addr# -> Int# -> Int# -> FastString
mkFastSubString# a# start# len# = mkFastCharString2 (A# (addrOffset# a# start#)) (I# len#)

mkFastSubStringFO# :: ForeignObj# -> Int# -> Int# -> FastString
mkFastSubStringFO# fo# start# len# =
 unsafePerformPrimIO  (
  readVar string_table                 `thenPrimIO` \ ft@(FastStringTable uid# tbl#) ->
  let h = hashSubStrFO fo# start# len# in
  case lookupTbl ft h of
    [] -> 
       -- no match, add it to table by copying out the
       -- the string into a ByteArray
       case copySubStrFO (_ForeignObj fo#) (I# start#) (I# len#) of
	 (_ByteArray _ barr#) ->  
	   let f_str = FastString uid# len# barr# in
           updTbl string_table ft h [f_str]       `seqPrimIO`
	   returnPrimIO f_str
    ls -> 
       -- non-empty `bucket', scan the list looking
       -- entry with same length and compare byte by byte.
       case bucket_match ls start# len# fo# of
	 Nothing -> 
           case copySubStrFO (_ForeignObj fo#) (I# start#) (I# len#) of
   	     (_ByteArray _ barr#) ->  
              let f_str = FastString uid# len# barr# in
              updTbl string_table ft  h (f_str:ls) `seqPrimIO`
	      ( {- _trace ("new: " ++ show f_str) $ -} returnPrimIO f_str)
	 Just v  -> {- _trace ("re-use: "++show v) $ -} returnPrimIO v)
  where
   bucket_match [] _ _ _ = Nothing
   bucket_match (v@(FastString _ l# barr#):ls) start# len# fo# =
      if len# ==# l# && eqStrPrefixFO fo# barr# start# len# then
	 Just v
      else
	 bucket_match ls start# len# fo#


mkFastSubStringBA# :: ByteArray# -> Int# -> Int# -> FastString
mkFastSubStringBA# barr# start# len# =
 unsafePerformPrimIO  (
  readVar string_table                   `thenPrimIO` \ ft@(FastStringTable uid# tbl#) ->
  let h = hashSubStrBA barr# start# len# in
  -- _trace ("hashed(b): "++show (I# h)) $
  case lookupTbl ft h of
    [] -> 
       -- no match, add it to table by copying out the
       -- the string into a ByteArray
       -- _trace "empty bucket(b)" $
       case copySubStrBA (_ByteArray btm barr#) (I# start#) (I# len#) of
         (_ByteArray _ ba#) ->  
          let f_str = FastString uid# len# ba# in
          updTbl string_table ft h [f_str]     `seqPrimIO`
          -- _trace ("new(b): " ++ show f_str)   $
	  returnPrimIO f_str
    ls -> 
       -- non-empty `bucket', scan the list looking
       -- entry with same length and compare byte by byte. 
       -- _trace ("non-empty bucket(b)"++show ls) $
       case bucket_match ls start# len# barr# of
	 Nothing -> 
          case copySubStrBA (_ByteArray (error "") barr#) (I# start#) (I# len#) of
            (_ByteArray _ ba#) ->  
              let f_str = FastString uid# len# ba# in
              updTbl string_table ft h (f_str:ls) `seqPrimIO`
	      -- _trace ("new(b): " ++ show f_str)   $
	      returnPrimIO f_str
	 Just v  -> 
              -- _trace ("re-use(b): "++show v) $
	      returnPrimIO v
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

mkFastCharString :: _Addr -> FastString
mkFastCharString a@(A# a#) = 
 case strLength a of{ (I# len#) -> CharStr a# len# }

mkFastCharString2 :: _Addr -> Int -> FastString
mkFastCharString2 a@(A# a#) (I# len#) = CharStr a# len#

mkFastString :: String -> FastString
mkFastString str = 
 case stringToByteArray str of
  (_ByteArray (_,I# len#) frozen#) -> 
    mkFastSubStringBA# frozen# 0# len#
    {- 0-indexed array, len# == index to one beyond end of string,
       i.e., (0,1) => empty string.    -}

mkFastSubString :: _Addr -> Int -> Int -> FastString
mkFastSubString (A# a#) (I# start#) (I# len#) =
 mkFastString# (addrOffset# a# start#) len#

mkFastSubStringFO :: _ForeignObj -> Int -> Int -> FastString
mkFastSubStringFO (_ForeignObj fo#) (I# start#) (I# len#) =
 mkFastSubStringFO# fo# start# len#

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
{- Currently UNUSED:
  if len# ==# 0# then
     0#
  else
     ((ord# c0 *# 631#) +# (ord# c1 *# 217#) +# (ord# c2 *# 43#) +# len#)
	`remInt#` hASH_TBL_SIZE#
-}
  where
    c0 = indexCharOffAddr# a# 0#
    c1 = indexCharOffAddr# a# 1# --(len# `quotInt#` 2# -# 1#)
    c2 = indexCharOffAddr# a# 2# --(len# -# 1#)

hashSubStrFO  :: ForeignObj# -> Int# -> Int# -> Int#
 -- use the Addr to produce a hash value between 0 & m (inclusive)
hashSubStrFO fo# start# len# =
  case len# of
   0# -> 0#
   1# -> ((ord# c0 *# 631#) +# len#) `remInt#` hASH_TBL_SIZE#
   2# -> ((ord# c0 *# 631#) +# (ord# c1 *# 217#) +# len#) `remInt#` hASH_TBL_SIZE#
   _  -> ((ord# c0 *# 631#) +# (ord# c1 *# 217#) +# (ord# c2 *# 43#) +# len#) `remInt#` hASH_TBL_SIZE#
{-
  if len# ==# 0# then
     0#
  else
     ((ord# c0 *# 631#) +# (ord# c1 *# 217#) +# (ord# c2 *# 43#) +# len#)
	`remInt#` hASH_TBL_SIZE#
-}
  where
    c0 = indexCharOffFO# fo# 0#
    c1 = indexCharOffFO# fo# 1# --(len# `quotInt#` 2# -# 1#)
    c2 = indexCharOffFO# fo# 2# --(len# -# 1#)


hashSubStrBA  :: ByteArray# -> Int# -> Int# -> Int#
 -- use the Addr to produce a hash value between 0 & m (inclusive)
hashSubStrBA ba# start# len# =
  case len# of
   0# -> 0#
   1# -> ((ord# c0 *# 631#) +# len#) `remInt#` hASH_TBL_SIZE#
   2# -> ((ord# c0 *# 631#) +# (ord# c1 *# 217#) +# len#) `remInt#` hASH_TBL_SIZE#
   _  -> ((ord# c0 *# 631#) +# (ord# c1 *# 217#) +# (ord# c2 *# 43#) +# len#) `remInt#` hASH_TBL_SIZE#
{-
  if len# ==# 0# then
     0#
  else
     ((ord# c0 *# 631#) +# (ord# c1 *# 217#) +# (ord# c2 *# 43#) +# len#)
	`remInt#` hASH_TBL_SIZE#
-}
  where
    c0 = indexCharArray# ba# 0#
    c1 = indexCharArray# ba# 1# --(len# `quotInt#` 2# -# 1#)
    c2 = indexCharArray# ba# 2# --(len# -# 1#)

\end{code}

\begin{code}
tagCmpFS :: FastString -> FastString -> _CMP_TAG
tagCmpFS (FastString u1# _ b1#) (FastString u2# _ b2#) = -- assume non-null chars
  if u1# ==# u2# then
     _EQ
  else
   unsafePerformPrimIO (
    _ccall_ strcmp (_ByteArray bottom b1#) (_ByteArray bottom b2#) `thenPrimIO` \ (I# res) ->
    returnPrimIO (
    if      res <#  0# then _LT
    else if res ==# 0# then _EQ
    else		    _GT
    ))
  where
   bottom = error "tagCmp"
tagCmpFS (CharStr bs1 len1) (CharStr bs2 len2)
  = unsafePerformPrimIO (
    _ccall_ strcmp ba1 ba2  `thenPrimIO` \ (I# res) ->
    returnPrimIO (
    if      res <#  0# then _LT
    else if res ==# 0# then _EQ
    else		    _GT
    ))
  where
    ba1 = A# bs1
    ba2 = A# bs2
tagCmpFS (FastString _ len1 bs1) (CharStr bs2 len2)
 = unsafePerformPrimIO (
    _ccall_ strcmp ba1 ba2 `thenPrimIO` \ (I# res) ->
    returnPrimIO (
     if      res <#  0# then _LT
     else if res ==# 0# then _EQ
     else		    _GT
    ))
  where
    ba1 = _ByteArray (error "") bs1
    ba2 = A# bs2

tagCmpFS a@(CharStr _ _) b@(FastString _ _ _)
  = -- try them the other way 'round
    case (tagCmpFS b a) of { _LT -> _GT; _EQ -> _EQ; _GT -> _LT }

instance Ord FastString where
    a <= b = case tagCmpFS a b of { _LT -> True;  _EQ -> True;  _GT -> False }
    a <	 b = case tagCmpFS a b of { _LT -> True;  _EQ -> False; _GT -> False }
    a >= b = case tagCmpFS a b of { _LT -> False; _EQ -> True;  _GT -> True  }
    a >	 b = case tagCmpFS a b of { _LT -> False; _EQ -> False; _GT -> True  }
    max x y | x >= y	=  x
            | otherwise	=  y
    min x y | x <= y	=  x
            | otherwise	=  y
    _tagCmp a b = tagCmpFS a b

\end{code}

Outputting @FastString@s is quick, just block copying the chunk (using
@fwrite@).

\begin{code}
hPutFS :: Handle -> FastString -> IO ()
hPutFS handle (FastString _ l# ba#) =
 if l# ==# 0# then
    return ()
 else
    _readHandle handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  _writeHandle handle htype		    >>
          failWith ioError
      _ClosedHandle ->
	  _writeHandle handle htype		    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  _writeHandle handle htype		    >>
	  failWith (IllegalOperation "handle is closed")
      _ReadHandle _ _ _ ->
	  _writeHandle handle htype		    >>
	  failWith (IllegalOperation "handle is not open for writing")
      other -> 
          let fp = _filePtr htype in
	   -- here we go..
          _ccall_ writeFile (_ByteArray (error "") ba#) fp (I# l#) `thenPrimIO` \rc ->
          if rc==0 then
              return ()
          else
              _constructError "hPutFS"   `thenPrimIO` \ err ->
	      failWith err
hPutFS handle (CharStr a# l#) =
 if l# ==# 0# then
    return ()
 else
    _readHandle handle				    >>= \ htype ->
    case htype of 
      _ErrorHandle ioError ->
	  _writeHandle handle htype		    >>
          failWith ioError
      _ClosedHandle ->
	  _writeHandle handle htype		    >>
	  failWith (IllegalOperation "handle is closed")
      _SemiClosedHandle _ _ ->
	  _writeHandle handle htype		    >>
	  failWith (IllegalOperation "handle is closed")
      _ReadHandle _ _ _ ->
	  _writeHandle handle htype		    >>
	  failWith (IllegalOperation "handle is not open for writing")
      other -> 
          let fp = _filePtr htype in
	   -- here we go..
          _ccall_ writeFile (A# a#) fp (I# l#) `thenPrimIO` \rc ->
          if rc==0 then
              return ()
          else
              _constructError "hPutFS"   `thenPrimIO` \ err ->
	      failWith err

--ToDo: avoid silly code duplic.
\end{code}
