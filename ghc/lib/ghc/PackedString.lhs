%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1994
%
\section[PackedString]{Packed strings}

A non-weird/abstract interface to the wired-in @PackedString@ type.

\begin{code}
module PackedString (
	PackedString(..),

	packString,		-- :: [Char] -> PackedString
	packCString,	        -- :: _Addr  -> PackedString
	packCBytes,		-- :: Int    -> _Addr -> PackedString

	packStringST,		-- :: [Char] -> _ST s PackedString
	packCBytesST,		-- :: Int    -> _Addr -> _ST s PackedString
	packBytesForC,		-- :: [Char] -> _ByteArray Int
	packBytesForCST,	-- :: [Char] -> _ST s (_ByteArray Int)

--NO:	packStringForC,
	nilPS,			-- :: PackedString
	consPS,			-- :: Char -> PackedString -> PackedString
	byteArrayToPS,		-- :: _ByteArray Int -> PackedString
	psToByteArray,		-- :: PackedString -> _ByteArray Int

	unpackPS,		-- :: PackedString -> [Char]
--NO:	unpackPS#,
	putPS,			-- :: _FILE -> PackedString -> PrimIO ()
	getPS,			-- :: _FILE -> Int -> PrimIO PackedString

	 {- alt. names for packString, unpackPS -}
	implode, 		-- :: [Char] -> PackedString
        explode,       		-- :: PackedString -> [Char]

	headPS,			-- :: PackedString -> Char
	tailPS,			-- :: PackedString -> PackedString
	nullPS,			-- :: PackedString -> Bool
	appendPS,		-- :: PackedString -> PackedString -> PackedString
	lengthPS,		-- :: PackedString -> Int
	indexPS,		-- :: PackedString -> Int -> Char
	mapPS,			-- :: (Char -> Char) -> PackedString -> PackedString
	filterPS,		-- :: (Char -> Bool) -> PackedString -> PackedString
	foldlPS,		-- :: (a -> Char -> a) -> a -> PackedString -> a 
	foldrPS,		-- :: (Char -> a -> a) -> a -> PackedString -> a
	takePS,			-- :: Int -> PackedString -> PackedString
	dropPS,			-- :: Int -> PackedString -> PackedString
	splitAtPS,		-- :: Int -> PackedString -> PackedString
	takeWhilePS,		-- :: (Char -> Bool) -> PackedString -> PackedString
	dropWhilePS,		-- :: (Char -> Bool) -> PackedString -> PackedString
	spanPS,			-- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
	breakPS,		-- :: (Char -> Bool) -> PackedString -> (PackedString, PackedString)
	linesPS,		-- :: PackedString -> [PackedString]
	wordsPS,		-- :: PackedString -> [PackedString]
	reversePS,		-- :: PackedString -> PackedString
	concatPS,		-- :: [PackedString] -> PackedString

	substrPS,		-- :: PackedString -> Int -> Int -> PackedString

	-- to make interface self-sufficient
	_PackedString, -- abstract!
	_FILE
    ) where

import PS

type PackedString = _PackedString

packString	= _packString
packCString	= _packCString

packCBytes	= _packCBytes
--packStringForC	= _packStringForC
nilPS		= _nilPS
consPS		= _consPS
byteArrayToPS	= _byteArrayToPS
psToByteArray	= _psToByteArray

packStringST    = _packStringST
packCBytesST    = _packCBytesST
packBytesForC   = _packBytesForC
packBytesForCST = _packBytesForCST

unpackPS	= _unpackPS
putPS		= _putPS
getPS		= _getPS

implode		= _packString -- alt. names
explode		= _unpackPS

headPS		= _headPS
tailPS		= _tailPS
nullPS		= _nullPS
appendPS	= _appendPS
lengthPS	= _lengthPS
indexPS		= _indexPS
mapPS		= _mapPS
filterPS	= _filterPS
foldlPS		= _foldlPS
foldrPS		= _foldrPS
takePS		= _takePS
dropPS		= _dropPS
splitAtPS	= _splitAtPS
takeWhilePS	= _takeWhilePS
dropWhilePS	= _dropWhilePS
spanPS		= _spanPS
breakPS		= _breakPS
linesPS		= _linesPS
wordsPS		= _wordsPS
reversePS	= _reversePS
concatPS	= _concatPS

substrPS	= _substrPS
\end{code}
