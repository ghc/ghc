-- compiled solely to produce an .hi file
--
module PreludeCore (

	Eq(..), Ord(..), Num(..), Real(..), Integral(..),
	Fractional(..), Floating(..), RealFrac(..), RealFloat(..),
	Ix(..), Enum(..), Text(..), Binary(..),
	_CCallable(..), _CReturnable(..),

	-- NO: really builtin (0.20+): trace,

	Array{-abstract-}, Assoc(..),
	_ByteArray, _MutableArray, _MutableByteArray,
	-- if you want the types *unabstractly*, import PreludeGlaST

	Bin{-abstract-},

	Complex(..),

	_PackedString{-abstract-}, _FILE,

	ReadS(..), ShowS(..),

--	IOResult, -- has to be here because wired into compiler
	Dialogue(..), Request(..), Response(..), SigAct(..), IOError(..),
	SuccCont(..), StrCont(..), StrListCont(..), BinCont(..), FailCont(..)
    ) where

-- few *Ty(s) imports
import UTypes		( Bin ) -- no data constructors, please!
import TyArray		( Array(..), Assoc(..), _ByteArray )
import TyComplex	( Complex(..) )
import TyIO
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
