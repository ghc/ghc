-- compiled to produce a 1.3 (whatever that is) Prelude.hi file
--
module Prelude (

	-- 1.3 extras (I/O is later)
	thenMaybe, 
	curry, uncurry,
-- LATER?
--	copy, lookup,

	-- NO: really builtin (0.20+): trace,

	(%), numerator, denominator, approxRational,

	realPart, imagPart, conjugate, mkPolar, cis,
	polar, magnitude, phase,

	_appendPS, _breakPS, _concatPS, _dropPS, _dropWhilePS,
	_filterPS, _foldlPS, _foldrPS, _headPS, _indexPS, _lengthPS,
	_linesPS, _mapPS, _nilPS, _consPS, _nullPS, _packCBytes,
	_packCString, _packString, _byteArrayToPS, _psToByteArray,
	_reversePS, _spanPS, _splitAtPS, _substrPS, _tailPS, _takePS,
	_takeWhilePS, _unpackPS, _wordsPS, _putPS,

	(&&), (||), not, otherwise,
	(^), (^^), appendBin, asTypeOf, atan2, fromIntegral,
	fromRealFrac, gcd, isAlpha, isAlphanum, isAscii, isControl,
	isDigit, isLower, isNullBin, isPrint, isSpace, isUpper, lcm,
	maxChar, maxInt, minChar, minInt, nullBin, subtract, toLower,
	toUpper, until, fst, snd, id, const, (.), flip, ($),
	ord, chr,

#if defined(__UNBOXED_INSTANCES__)
	minInt#, maxInt#,
	minChar#, maxChar#,
	toChar#, fromChar#,
	isAscii#, isControl#, isPrint#, isSpace#, 
	isUpper#, isLower#, isAlpha#, isDigit#, isAlphanum#,
	toUpper#, toLower#,
#endif

	head, last, tail, init, null, (++), (\\), genericLength,
	length, (!!), map, filter, partition, {-BUILTIN: foldl,-}
	foldl1, scanl, scanl1, {-BUILTIN: foldr,-} foldr1, scanr, scanr1,
	iterate, repeat, cycle, take, drop, splitAt, takeWhile,
	dropWhile, span, break, lines, words, unlines, unwords, nub,
	reverse,  and, or, any, all, elem, notElem, sum,
	product, sums, products, maximum, minimum, concat,
	transpose, zip, zip3, zip4, zip5, zip6, zip7, zipWith,
	zipWith3, zipWith4, zipWith5, zipWith6, zipWith7, unzip,
	unzip3, unzip4, unzip5, unzip6, unzip7,

	array, listArray, (!), bounds, indices, elems, assocs,
	accumArray, (//), accum, amap, ixmap,

	reads, shows, show, read, lex, showChar, showString,
	readParen, showParen, readLitChar, showLitChar, readSigned,
	showSigned, showSpace__, readDec, showInt, readFloat, showFloat,
	_showHex, _showRadix, _showDigit, -- non-std
	_readList, _showList, _truncate, _round, _ceiling, _floor,

	_readRational, _showRational, -- extras!

	-- 1.3 I/O stuff from PreludeIO, some *renamed*
	(>>),
	(>>=),
	accumulate,
	appendFile,
	either,
	fail,
	failWith,
	getChar,
	hClose,
	hFileSize,
	hFlush,
	hGetChar,
	hGetContents,
	hGetPosn,
	hIsBlockBuffered,
	hIsClosed,
	hIsEOF,
	hIsLineBuffered,
	hIsNotBuffered,
	hIsOpen,
	hIsReadable,
	hIsSeekable,
	hIsWritable,
	hLookAhead,
	hPutChar,
	hPutStr,
	hPutText,
	hReady,
	hSeek,
	hSetBuffering,
	hSetPosn,
	handle,
	interact,
	isEOF,
	openFile,
	putChar,
	putStr,
	putText,
	print,
	readFile,
	return,
	sequence,
	stderr,
	stdin,
	stdout,
	try,
	writeFile,

	primIOToIO, ioToPrimIO,	-- extra, and very dodgy

	-- and for foldr/build
	_build

    ) where

-- few *Ty(s) imports
import TyArray		( Array(..), Assoc(..), _ByteArray )
import TyComplex	( Complex(..) )
--import Builtin	( trace )
import Cls	hiding	( String )
import Core
import PreludeGlaST	( thenStrictlyST, _MutableByteArray, _MutableArray )
import PreludePrimIO	( thenPrimIO, returnPrimIO )
--import PrelCore13
import IArray
import IBool
import IChar
import IComplex
import IDouble
import IFloat
import IInt
import IInteger
import IList
import IRatio
import ITup0
import ITup2
import ITup3
import ITup4
import ITup5
import List
import Prel
import Text
import PS

import PreludeIO renaming ( -- IOError13    to IOError -- can't rename PreludeCore types
			    appendFile13    to appendFile
			  , interact13	    to interact
			  , print13	    to print
			  , readFile13	    to readFile
			  , stderr13	    to stderr
			  , stdin13	    to stdin
			  , stdout13	    to stdout
			  , writeFile13	    to writeFile
			  )

primIOToIO :: PrimIO a -> IO a
primIOToIO m = m `thenPrimIO` \ a -> return a

ioToPrimIO :: IO a -> PrimIO a
ioToPrimIO io
  = io		`thenPrimIO` \ r ->
    case r of
      Right a -> returnPrimIO a
      Left  e -> error ("I/O Error (ioToPrimIO): " ++ shows e "\n")


-- 1.3 extra functions

thenMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
thenMaybe Nothing _ = Nothing
thenMaybe (Just x) f = f x

curry   :: ((a,b) -> c) -> a -> b -> c
curry f x y = f (x,y)

uncurry :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y) = f x y

{- LATER?:
copy     :: Int -> a -> [a]
copy  n x = take n (repeat x)

lookup :: Eq a => [(a,b)] -> a -> Maybe b
lookup [] _ = Nothing
lookup ((key,val) : rest) x | key == x = Just val
                            | otherwise = lookup rest x
-}
