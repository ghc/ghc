-- compiled solely to produce an .hi file
--
module Prelude (

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
	toInt#, fromInt#,
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
	_readList, _showList,

	_readRational, _showRational, -- extras!

	stdin, stdout, stderr, stdecho, done, readFile, writeFile,
	appendFile, readBinFile, writeBinFile, appendBinFile,
	deleteFile, statusFile, readChan, appendChan, readBinChan,
	appendBinChan, statusChan, echo, getArgs, getProgName, getEnv,
	setEnv, sigAction, abort, exit, print, prints, interact,
	strDispatch, strListDispatch, binDispatch, succDispatch,

	-- and for foldr/build
	_build, _augment, 

	_newArray, _freezeArray, _arrEleBottom,
	_rangeComplaint_Ix_Int
    ) where

-- few *Ty(s) imports
import TyArray		( Array(..), Assoc(..), _ByteArray )
import TyComplex	( Complex(..) )
import TyIO
--import Builtin	( trace )
import Cls	hiding	( String )
import Core
import PreludeGlaST	( _MutableByteArray, _MutableArray )
import IArray
import IBool
import IChar
import IComplex
import IDouble
import IFloat
import IInt
import IInteger
import IList
import IO
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
