%
% (c) The GRASP/AQUA Project, Glasgow University, 1993-1994
%
\section[PackedString]{Packed strings}

A non-weird interface to the wired-in @PackedString@ type.

\begin{code}
module PackedString (
	PackedString(..),

	packString,
	packCString,
	packCBytes,
--NO:	packStringForC,
	nilPS,
	consPS,
	byteArrayToPS,
	psToByteArray,

	unpackPS,
--NO:	unpackPS#,
	putPS,

	implode, explode, -- alt. names for packString, unpackPS

	headPS,
	tailPS,
	nullPS,
	appendPS,
	lengthPS,
	indexPS,
	mapPS,
	filterPS,
	foldlPS,
	foldrPS,
	takePS,
	dropPS,
	splitAtPS,
	takeWhilePS,
	dropWhilePS,
	spanPS,
	breakPS,
	linesPS,
	wordsPS,
	reversePS,
	concatPS,

	substrPS,

	-- to make interface self-sufficient
	_PackedString, -- abstract!
	_FILE
    ) where

type PackedString = _PackedString

packString	= _packString
packCString	= _packCString

packCBytes	= _packCBytes
--packStringForC	= _packStringForC
nilPS		= _nilPS
consPS		= _consPS
byteArrayToPS	= _byteArrayToPS
psToByteArray	= _psToByteArray

unpackPS	= _unpackPS
putPS		= _putPS

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
