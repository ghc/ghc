%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1994
%
\section[PrelPS]{Packed strings}

This sits on top of the sequencing/arrays world, notably @ByteArray#@s.

Glorious hacking (all the hard work) by Bryan O'Sullivan.

\begin{code}
module PreludePS{-yes, a Prelude module!-} (
	_packString,
	_packStringST,
	_packCString,
	_packCBytes,
	_packCBytesST,
	_packStringForC,
        _packBytesForC,
        _packBytesForCST,
	_nilPS,
	_consPS,
--OLD:	packString#,
--OLD:	packToCString,
	_byteArrayToPS,
	_unsafeByteArrayToPS,
	_psToByteArray,

	_unpackPS,
	unpackPS#,
--	toCString,
	_putPS,

	_headPS,
	_tailPS,
	_nullPS,
	_appendPS,
	_lengthPS,
	_indexPS,
	_mapPS,
	_filterPS,
	_foldlPS,
	_foldrPS,
	_takePS,
	_dropPS,
	_splitAtPS,
	_takeWhilePS,
	_dropWhilePS,
	_spanPS,
	_breakPS,
	_linesPS,
	_wordsPS,
	_reversePS,
	_concatPS,

	_substrPS,
--???	_hashPS,

	-- to make interface self-sufficient
	_PackedString, -- abstract!
	_FILE
    ) where

import PreludeGlaST
import Stdio		( _FILE )
import TyArray		( _ByteArray(..) )

import Cls
import Core
import IChar
import IList
import IInt
import Prel		( otherwise, (&&), (||), chr, ($), not, (.), isSpace, flip )
import List		( length, (++), map, filter, foldl, foldr,
			  lines, words, reverse, null, foldr1
			)
import TyArray		( Array(..) )
import Text
\end{code}

%************************************************************************
%*									*
\subsection{@_PackedString@ type declaration and interface (signatures)}
%*									*
%************************************************************************

The things we want:
\begin{code}
data _PackedString
  = _PS		ByteArray#  -- the bytes
		Int#	    -- length (*not* including NUL at the end)
		Bool	    -- True <=> contains a NUL
  | _CPS	Addr#	    -- pointer to the (null-terminated) bytes in C land
		Int#	    -- length, as per strlen
		-- definitely doesn't contain a NUL

_packString	 :: [Char] -> _PackedString
_packStringST	 :: [Char] -> _ST s _PackedString
_packCString	 :: _Addr  -> _PackedString
_packCBytes	 :: Int -> _Addr -> _PackedString
_packCBytesST	 :: Int -> _Addr -> _ST s _PackedString
_packStringForC  :: [Char] -> ByteArray#	-- calls injected by compiler
_packBytesForC   :: [Char] -> _ByteArray Int
_packBytesForCST :: [Char] -> _ST s (_ByteArray Int)
_nilPS		 :: _PackedString
_consPS		 :: Char -> _PackedString -> _PackedString
_byteArrayToPS	 :: _ByteArray Int -> _PackedString
_psToByteArray	 :: _PackedString -> _ByteArray Int

--OLD: packString#	:: [Char] -> ByteArray#
--OLD: packToCString	:: [Char] -> _ByteArray Int -- hmmm... weird name

_unpackPS	:: _PackedString -> [Char]
unpackPS#	:: Addr#	 -> [Char] -- calls injected by compiler
--???toCString	:: _PackedString -> ByteArray#
_putPS		:: _FILE -> _PackedString -> PrimIO () -- ToDo: more sensible type
\end{code}

\begin{code}
_headPS	    :: _PackedString -> Char
_tailPS	    :: _PackedString -> _PackedString
_nullPS	    :: _PackedString -> Bool
_appendPS   :: _PackedString -> _PackedString -> _PackedString
_lengthPS   :: _PackedString -> Int
_indexPS    :: _PackedString -> Int -> Char
	    -- 0-origin indexing into the string
_mapPS	    :: (Char -> Char) -> _PackedString -> _PackedString {-or String?-}
_filterPS   :: (Char -> Bool) -> _PackedString -> _PackedString {-or String?-}
_foldlPS    :: (a -> Char -> a) -> a -> _PackedString -> a
_foldrPS    :: (Char -> a -> a) -> a -> _PackedString -> a
_takePS	    :: Int -> _PackedString -> _PackedString
_dropPS	    :: Int -> _PackedString -> _PackedString
_splitAtPS  :: Int -> _PackedString -> (_PackedString, _PackedString)
_takeWhilePS:: (Char -> Bool) -> _PackedString -> _PackedString
_dropWhilePS:: (Char -> Bool) -> _PackedString -> _PackedString
_spanPS	    :: (Char -> Bool) -> _PackedString -> (_PackedString, _PackedString)
_breakPS    :: (Char -> Bool) -> _PackedString -> (_PackedString, _PackedString)
_linesPS    :: _PackedString -> [_PackedString]
_wordsPS    :: _PackedString -> [_PackedString]
_reversePS  :: _PackedString -> _PackedString
_concatPS   :: [_PackedString] -> _PackedString

_substrPS   :: _PackedString -> Int -> Int -> _PackedString
	    -- pluck out a piece of a _PS
	    -- start and end chars you want; both 0-origin-specified
--??? _hashPS	    :: _PackedString -> Int -> Int
	    -- use the _PS to produce a hash value between 0 & m (inclusive)
\end{code}

%************************************************************************
%*									*
\subsection{Constructor functions}
%*									*
%************************************************************************

Easy ones first.  @_packString@ requires getting some heap-bytes and
scribbling stuff into them.

\begin{code}
_packCString (A# a#)	-- the easy one; we just believe the caller
  = _CPS a# len
  where
    len = case (strlen# a#) of { I# x -> x }

_nilPS = _CPS ""# 0#
_consPS c cs = _packString (c : (_unpackPS cs)) -- ToDo:better

_packStringForC str
  = case (_packString str) of
      _PS bytes _ _ -> bytes

_packBytesForC str
  = _psToByteArray (_packString str)

_packBytesForCST str
  = _packStringST str	`thenStrictlyST` \ (_PS bytes n has_null) -> 
    --later? ASSERT(not has_null)
    returnStrictlyST (_ByteArray (0, I# (n -# 1#)) bytes)

_packString str = _runST (_packStringST str)

_packStringST str
  = let  len = length str  in
    pack_me len str
  where
    pack_me :: Int -> [Char] -> _ST s _PackedString

    pack_me len@(I# length#) str
      =	    -- allocate an array that will hold the string
	    -- (not forgetting the NUL byte at the end)
	  new_ps_array (length# +# 1#) `thenStrictlyST` \ ch_array ->

	    -- fill in packed string from "str"
	  fill_in ch_array 0# str   `seqStrictlyST`

	    -- freeze the puppy:
	  freeze_ps_array ch_array `thenStrictlyST` \ (_ByteArray _ frozen#) ->

	  let has_null = byteArrayHasNUL# frozen# length# in
	  
	  returnStrictlyST (_PS frozen# length# has_null)
      where
	fill_in :: _MutableByteArray s Int -> Int# -> [Char] -> _ST s ()

	fill_in arr_in# idx []
	  = write_ps_array arr_in# idx (chr# 0#) `seqStrictlyST`
	    returnStrictlyST ()

	fill_in arr_in# idx (C# c : cs)
	  = write_ps_array arr_in# idx c	 `seqStrictlyST`
	    fill_in arr_in# (idx +# 1#) cs

_packCBytes len addr = _runST (_packCBytesST len addr)

_packCBytesST len@(I# length#) (A# addr)
  =   -- allocate an array that will hold the string
      -- (not forgetting the NUL byte at the end)
    new_ps_array (length# +# 1#)  `thenStrictlyST` \ ch_array ->

      -- fill in packed string from "addr"
    fill_in ch_array 0#   `seqStrictlyST`

      -- freeze the puppy:
    freeze_ps_array ch_array `thenStrictlyST` \ (_ByteArray _ frozen#) ->

    let has_null = byteArrayHasNUL# frozen# length# in
	  
    returnStrictlyST (_PS frozen# length# has_null)
  where
    fill_in :: _MutableByteArray s Int -> Int# -> _ST s ()

    fill_in arr_in# idx
      | idx ==# length#
      = write_ps_array arr_in# idx (chr# 0#) `seqStrictlyST`
	returnStrictlyST ()
      | otherwise
      = case (indexCharOffAddr# addr idx) of { ch ->
	write_ps_array arr_in# idx ch `seqStrictlyST`
	fill_in arr_in# (idx +# 1#) }

_byteArrayToPS (_ByteArray ixs@(_, ix_end) frozen#)
  = let
	n# = case (
		if null (range ixs)
		  then 0
		  else ((index ixs ix_end) + 1)
	     ) of { I# x -> x }
    in
    _PS frozen# n# (byteArrayHasNUL# frozen# n#)

_unsafeByteArrayToPS (_ByteArray _ frozen#) (I# n#)
  = _PS frozen# n# (byteArrayHasNUL# frozen# n#)

_psToByteArray (_PS bytes n has_null)
  = _ByteArray (0, I# (n -# 1#)) bytes

_psToByteArray (_CPS addr len#)
  = let
	len		= I# len#
	byte_array_form = _packCBytes len (A# addr)
    in
    case byte_array_form of { _PS bytes _ _ ->
    _ByteArray (0, len - 1) bytes }
\end{code}

%************************************************************************
%*									*
\subsection{Destructor functions (taking @_PackedStrings@ apart)}
%*									*
%************************************************************************

\begin{code}
unpackPS# addr -- calls injected by compiler
  = _unpackPS (_CPS addr len)
  where
    len = case (strlen# addr) of { I# x -> x }

-- OK, but this code gets *hammered*:
-- _unpackPS ps
--   = [ _indexPS ps n | n <- [ 0::Int .. _lengthPS ps - 1 ] ]

_unpackPS (_PS bytes len has_null)
  = unpack 0#
  where
    unpack nh
      | nh >=# len  = []
      | otherwise   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharArray# bytes nh

_unpackPS (_CPS addr len)
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = []
      | otherwise	   = C# ch : unpack (nh +# 1#)
      where
	ch = indexCharOffAddr# addr nh
\end{code}

\begin{code}
_putPS file ps@(_PS bytes len has_null)
  | len ==# 0#
  = returnPrimIO ()
  | otherwise
  = let
	byte_array = _ByteArray (0, I# (len -# 1#)) bytes
    in
    _ccall_ fwrite byte_array (1::Int){-size-} (I# len) file
					`thenPrimIO` \ (I# written) ->
    if written ==# len then
	returnPrimIO ()
    else
	error "_putPS: fwrite failed!\n"

_putPS file (_CPS addr len)
  | len ==# 0#
  = returnPrimIO ()
  | otherwise
  = _ccall_ fputs (A# addr) file `thenPrimIO` \ (I# _){-force type-} ->
    returnPrimIO ()
\end{code}

%************************************************************************
%*									*
\subsection{List-mimicking functions for @_PackedStrings@}
%*									*
%************************************************************************

First, the basic functions that do look into the representation;
@indexPS@ is the most important one.
\begin{code}
_lengthPS ps = I# (lengthPS# ps)

{-# INLINE lengthPS# #-}

lengthPS# (_PS  _ i _) = i
lengthPS# (_CPS _ i)   = i

{-# INLINE strlen# #-}

strlen# :: Addr# -> Int
strlen# a
  = unsafePerformPrimIO (
    _ccall_ strlen (A# a)  `thenPrimIO` \ len@(I# _) ->
    returnPrimIO len
    )

byteArrayHasNUL# :: ByteArray# -> Int#{-length-} -> Bool

byteArrayHasNUL# bs len
  = unsafePerformPrimIO (
    _ccall_ byteArrayHasNUL__ ba (I# len)  `thenPrimIO` \ (I# res) ->
    returnPrimIO (
    if res ==# 0# then False else True
    ))
  where
    ba = _ByteArray (0, I# (len -# 1#)) bs

-----------------------
_indexPS ps (I# n) = C# (indexPS# ps n)

{-# INLINE indexPS# #-}

indexPS# (_PS bs i _) n
  = --ASSERT (n >=# 0# && n <# i)	-- error checking: my eye!  (WDP 94/10)
    indexCharArray# bs n

indexPS# (_CPS a _) n
  = indexCharOffAddr# a n
\end{code}

Now, the rest of the functions can be defined without digging
around in the representation.
\begin{code}
_headPS ps
  | _nullPS ps = error "_headPS: head []"
  | otherwise  = C# (indexPS# ps 0#)

_tailPS ps
  | len <=# 0# = error "_tailPS: tail []"
  | len ==# 1# = _nilPS
  | otherwise  = substrPS# ps 1# (len -# 1#)
  where
    len = lengthPS# ps

_nullPS (_PS  _ i _) = i ==# 0#
_nullPS (_CPS _ i)   = i ==# 0#

-- ToDo: some non-lousy implementations...

_appendPS xs ys
  | _nullPS xs = ys
  | _nullPS ys = xs
  | otherwise  = _packString (_unpackPS xs ++ _unpackPS ys)

_mapPS f xs = _packString (map f (_unpackPS xs))

_filterPS p  ps = _packString (filter p (_unpackPS ps))
_foldlPS f b ps = foldl f b (_unpackPS ps)
_foldrPS f b ps = foldr f b (_unpackPS ps)

_takePS (I# n) ps = substrPS# ps 0# (n -# 1#)
_dropPS (I# n) ps = substrPS# ps n  (lengthPS# ps -# 1#)
_splitAtPS  n ps  = (_takePS n ps, _dropPS n ps)

_takeWhilePS pred ps
  = let
	break_pt = char_pos_that_dissatisfies
			(\ c -> pred (C# c))
			ps
			(lengthPS# ps)
			0#
    in
    substrPS# ps 0# (break_pt -# 1#)

_dropWhilePS pred ps
  = let
	len	 = lengthPS# ps
	break_pt = char_pos_that_dissatisfies
			(\ c -> pred (C# c))
			ps
			len
			0#
    in
    substrPS# ps break_pt (len -# 1#)

char_pos_that_dissatisfies :: (Char# -> Bool) -> _PackedString -> Int# -> Int# -> Int#

char_pos_that_dissatisfies p ps len pos
  | pos >=# len		= pos -- end
  | p (indexPS# ps pos) = -- predicate satisfied; keep going
			  char_pos_that_dissatisfies p ps len (pos +# 1#)
  | otherwise		= pos -- predicate not satisfied

char_pos_that_dissatisfies p ps len pos -- dead code: HACK to avoid badly-typed error msg
  = 0#

-- ToDo: could certainly go quicker
_spanPS  p ps = (_takeWhilePS p ps, _dropWhilePS p ps)
_breakPS p ps = _spanPS (not . p) ps

_linesPS ps = map _packString (lines (_unpackPS ps))
_wordsPS ps = map _packString (words (_unpackPS ps))

_reversePS ps = _packString (reverse (_unpackPS ps))

_concatPS [] = _nilPS
_concatPS pss
  = let
	tot_len# = case (foldr ((+) . _lengthPS) 0 pss) of { I# x -> x }
	tot_len	 = I# tot_len#
    in
    _runST (
    new_ps_array (tot_len# +# 1#)   `thenStrictlyST` \ arr# -> -- incl NUL byte!
    packum arr# pss 0#		    `seqStrictlyST`
    freeze_ps_array arr#	    `thenStrictlyST` \ (_ByteArray _ frozen#) ->

    let has_null = byteArrayHasNUL# frozen# tot_len# in
	  
    returnStrictlyST (_PS frozen# tot_len# has_null)
    )
  where
    packum :: _MutableByteArray s Int -> [_PackedString] -> Int# -> _ST s ()

    packum arr [] pos
      = write_ps_array arr pos (chr# 0#) `seqStrictlyST`
	returnStrictlyST ()
    packum arr (ps : pss) pos
      = fill arr pos ps 0# (lengthPS# ps)  `thenStrictlyST` \ (I# next_pos) ->
	packum arr pss next_pos

    fill :: _MutableByteArray s Int -> Int# -> _PackedString -> Int# -> Int# -> _ST s Int

    fill arr arr_i ps ps_i ps_len
     | ps_i ==# ps_len
       = returnStrictlyST (I# (arr_i +# ps_len))
     | otherwise
       = write_ps_array arr (arr_i +# ps_i) (indexPS# ps ps_i) `seqStrictlyST`
	 fill arr arr_i ps (ps_i +# 1#) ps_len
\end{code}

%************************************************************************
%*									*
\subsection{Instances for @_PackedStrings@: @Eq@, @Ord@, @Text@}
%*									*
%************************************************************************

Instances:
\begin{code}
instance Eq _PackedString where
    a == b = case _tagCmpPS a b of { _LT -> False; _EQ -> True;  _GT -> False }
    a /= b = case _tagCmpPS a b of { _LT -> True;  _EQ -> False; _GT -> True  }

instance Ord _PackedString where
    a <= b = case _tagCmpPS a b of { _LT -> True;  _EQ -> True;  _GT -> False }
    a <	 b = case _tagCmpPS a b of { _LT -> True;  _EQ -> False; _GT -> False }
    a >= b = case _tagCmpPS a b of { _LT -> False; _EQ -> True;  _GT -> True  }
    a >	 b = case _tagCmpPS a b of { _LT -> False; _EQ -> False; _GT -> True  }
    _tagCmp a b = _tagCmpPS a b
\end{code}

We try hard to make this go fast:
\begin{code}
_tagCmpPS :: _PackedString -> _PackedString -> _CMP_TAG

_tagCmpPS (_PS  bs1 len1 has_null1) (_PS  bs2 len2 has_null2)
  | not has_null1 && not has_null2
  = unsafePerformPrimIO (
    _ccall_ strcmp ba1 ba2  `thenPrimIO` \ (I# res) ->
    returnPrimIO (
    if      res <#  0# then _LT
    else if res ==# 0# then _EQ
    else		    _GT
    ))
  where
    ba1 = _ByteArray (0, I# (len1 -# 1#)) bs1
    ba2 = _ByteArray (0, I# (len2 -# 1#)) bs2

_tagCmpPS (_PS  bs1 len1 has_null1) (_CPS bs2 len2)
  | not has_null1
  = unsafePerformPrimIO (
    _ccall_ strcmp ba1 ba2  `thenPrimIO` \ (I# res) ->
    returnPrimIO (
    if      res <#  0# then _LT
    else if res ==# 0# then _EQ
    else		    _GT
    ))
  where
    ba1 = _ByteArray (0, I# (len1 -# 1#)) bs1
    ba2 = A# bs2

_tagCmpPS (_CPS bs1 len1) (_CPS bs2 len2)
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

_tagCmpPS a@(_CPS _ _) b@(_PS _ _ has_null2)
  | not has_null2
  = -- try them the other way 'round
    case (_tagCmpPS b a) of { _LT -> _GT; _EQ -> _EQ; _GT -> _LT }

_tagCmpPS ps1 ps2 -- slow catch-all case (esp for "has_null" True)
  = looking_at 0#
  where
    end1 = lengthPS# ps1 -# 1#
    end2 = lengthPS# ps2 -# 1#

    looking_at char#
      = if char# ># end1 then
	   if char# ># end2 then -- both strings ran out at once
	      _EQ
	   else	-- ps1 ran out before ps2
	      _LT
	else if char# ># end2 then
	   _GT	-- ps2 ran out before ps1
	else
	   let
	      ch1 = indexPS# ps1 char#
	      ch2 = indexPS# ps2 char#
	   in
	   if ch1 `eqChar#` ch2 then
	      looking_at (char# +# 1#)
	   else if ch1 `ltChar#` ch2 then _LT
				     else _GT

instance Text _PackedString where
  readsPrec p = error "readsPrec: _PackedString: ToDo"
  showsPrec p ps r = showsPrec p (_unpackPS ps) r
\end{code}

%************************************************************************
%*									*
\subsection{Uniquely PackedString functions}
%*									*
%************************************************************************

For @_substrPS@, see the next section.

@_hashPS@ is just what we happen to need in GHC...
\begin{code}
{- LATER?
_hashPS ps (I# hASH_TBL_SIZE#)
  = I# (h `remInt#` hASH_TBL_SIZE#)
  where
    len = lengthPS# ps

    h | len <=# 0# = 0# -- probably should just be an "error"
      | len ==# 1# = ord# c1
      | len ==# 2# = ord# c2
      | len ==# 3# = ord# c2 +# ord# c3
      | len ==# 4# = ord# c2 +# ord# c3 +# ord# c4
      | len ==# 5# = ord# c2 +# ord# c3 +# ord# c4 +# ord# c5
      | len >=# 6# = ord# c2 +# ord# c3 +# ord# c4 +# ord# c5 +# ord# c6
      | otherwise  = 999# -- will never happen

    c1 = indexPS# ps 0#
    c2 = indexPS# ps 1#
    c3 = indexPS# ps 2#
    c4 = indexPS# ps 3#
    c5 = indexPS# ps 4#
    c6 = indexPS# ps 5#
-}
\end{code}

%************************************************************************
%*									*
\subsection{Local utility functions}
%*									*
%************************************************************************

The definition of @_substrPS@ is essentially:
@take (end - begin + 1) (drop begin str)@.
\begin{code}
_substrPS ps (I# begin) (I# end) = substrPS# ps begin end

substrPS# ps s e
  | s <# 0# || e <# s
  = error "_substrPS: bounds out of range"

  | s >=# len || result_len# <=# 0#
  = _nilPS

  | otherwise
  = _runST (
	new_ps_array (result_len# +# 1#) `thenStrictlyST` \ ch_arr -> -- incl NUL byte!
	fill_in ch_arr 0#	         `seqStrictlyST`
	freeze_ps_array ch_arr	         `thenStrictlyST` \ (_ByteArray _ frozen#) ->

	let has_null = byteArrayHasNUL# frozen# result_len# in
	  
	returnStrictlyST (_PS frozen# result_len# has_null)
    )
  where
    len = lengthPS# ps

    result_len# = (if e <# len then (e +# 1#) else len) -# s
    result_len  = I# result_len#

    -----------------------
    fill_in :: _MutableByteArray s Int -> Int# -> _ST s ()

    fill_in arr_in# idx
      | idx ==# result_len#
      = write_ps_array arr_in# idx (chr# 0#) `seqStrictlyST`
	returnStrictlyST ()
      | otherwise
      = let
	    ch = indexPS# ps (s +# idx)
	in
	write_ps_array arr_in# idx ch	     `seqStrictlyST`
	fill_in arr_in# (idx +# 1#)
\end{code}

(Very :-) ``Specialised'' versions of some CharArray things...
\begin{code}
new_ps_array	:: Int# -> _ST s (_MutableByteArray s Int)
write_ps_array	:: _MutableByteArray s Int -> Int# -> Char# -> _ST s () 
freeze_ps_array :: _MutableByteArray s Int -> _ST s (_ByteArray Int)

new_ps_array size (S# s)
  = case (newCharArray# size s)	  of { StateAndMutableByteArray# s2# barr# ->
    (_MutableByteArray bot barr#, S# s2#)}
  where
    bot = error "new_ps_array"

write_ps_array (_MutableByteArray _ barr#) n ch (S# s#)
  = case writeCharArray# barr# n ch s#	of { s2#   ->
    ((), S# s2#)}

-- same as unsafeFreezeByteArray
freeze_ps_array (_MutableByteArray ixs arr#) (S# s#)
  = case unsafeFreezeByteArray# arr# s# of { StateAndByteArray# s2# frozen# ->
    (_ByteArray ixs frozen#, S# s2#) }
\end{code}
