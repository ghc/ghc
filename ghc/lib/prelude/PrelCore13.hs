-- proto Haskell 1.3 version
--
module PreludeCore (

	Eq(..), Ord(..), Num(..), Real(..), Integral(..),
	Fractional(..), Floating(..), RealFrac(..), RealFloat(..),
	Ix(..), Enum(..), Text(..), Binary(..),
	_CCallable(..), _CReturnable(..),

	-- NO: really builtin (0.20+): trace,

	Array{-abstract-},
	Assoc(..),  -- *should* have disappeared: ToDo: LATER
	_ByteArray, _MutableArray, _MutableByteArray,
	-- if you want the types *unabstractly*, import PreludeGlaST

	Bin{-abstract-},

	Complex(..),

	_PackedString{-abstract-}, _FILE,

	ReadS(..), ShowS(..),

	-- 1.3 I/O stuff from PreludeIO, some *renamed*
	IOError13(..), -- ToDo: rename
	Either(..),
	BufferMode(..),
	IOMode(..),
	SeekMode(..),
	Maybe(..),
	FilePath(..),
	Handle(..),
	HandlePosn(..),
	IO(..),
	_Handle,
	_MVar,

	-- for unfoldings ...
	_fromRational, __i0, __i1, __i2, __im1, __i8, __i10, __i16, __rhalf,
	__integer0, __integer1, __integer2, __integerm1
    ) where

-- few *Ty(s) imports
import UTypes		( Bin ) -- no data constructors, please!
import TyArray		( Array(..), Assoc(..), _ByteArray )
import TyComplex	( Complex(..) )
--import Builtin	( trace )
import Cls	hiding	( String )
import Core
import PS
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
import IRatio
import ITup0
import ITup2
import ITup3
import ITup4
import ITup5
import List
import Prel
import Text

import PreludeIO -- renaming ( IOError13 to IOError ) -- can't rename PreludeCore types
