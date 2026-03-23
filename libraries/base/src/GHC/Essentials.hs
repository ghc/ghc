{-# LANGUAGE MagicHash, Trustworthy, RankNTypes, CPP #-}
{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -fdefines-known-key-names #-} -- See Note [JS/WASM primitives known-keys for other targets]

-- |
--
-- Module      :  GHC.Essentials
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--

module GHC.Essentials
    ( Eq(..), Ord(..)  -- With their methods
    , Show, Read

    -- Foldable/Traversable with those methods need for deriving
    , Foldable(foldr, foldMap, null), all
    , Traversable(traverse)

    , Functor, fmap, (<$)
    , Monad, (>>), (>>=), return, fail, guard, mfix, join
    , Alternative
    , MonadFail, MonadPlus

    -- Misc
    , (.), (&&), not, foldrList, build, map
    , seq#, ($), assert, considerAccessible
    , augment, otherwise

    -- Applicative
    , Applicative, pure, mzip, (<*>), (*>), liftA2

    -- Semigroup, Monoid
    , Semigroup, Monoid
    , (<>), mappend, mempty

    -- Enum
    , Enum
    , enumFrom, enumFromThen, enumFromTo, enumFromThenTo
    , succ, pred, fromEnum, toEnum, enumIntToWord
    , toEnumError, succError, predError

    -- Bounded
    , Bounded
    , minBound, maxBound

    -- Ix
    , Ix, range, inRange, index, unsafeIndex, unsafeRangeSize

    -- Data
    , Data, Fixity(Prefix,Infix)
    , gfoldl, gunfold, toConstr, dataTypeOf, dataCast1, dataCast2
    , mkConstrTag, Constr, mkDataType, DataType, constrIndex

    -- Typeable
    , Typeable
    , gcast1, gcast2

    -- Generics
    , Generic(..), Generic1(..)
    , Datatype(..), Constructor(..), Selector(..)
    , U1(..), Par1(..), Rec0, Rec1(..), K1(..), M1(..), S1, C1, D1
    , V1, (:+:)(L1, R1), (:*:)((:*:)), (:.:)(Comp1, unComp1)
    , UAddr, UChar, UDouble, UFloat, UInt, UWord, Meta(..)
    , URec(UAddr, UChar, UDouble, UFloat, UInt, UWord,
           uAddr#, uChar#, uDouble#, uFloat#, uInt#, uWord#)
    , FixityI(..), Associativity(..), SourceUnpackedness(..), SourceStrictness(..)
    , DecidedStrictness(..)

    -- DataToTag
    , DataToTag
    , dataToTag#

    -- Implicit Params
    , IP

    -- Other data types
    , Either(..)
    , Void
    , NonEmpty

    -- SpecConstr
    , SPEC(..)

    -- FFI
    , Ptr, FunPtr
    , ConstPtr

    -- Show internals
    , showsPrec, shows, showString, showSpace, showCommaSpace, showParen

    -- Read internals
    , readList, readListDefault, readListPrec, readListPrecDefault
    , readPrec, parens, choose, lexP, expectP
    , readField, readFieldHash, readSymField
    , Lexeme(Punc, Ident, Symbol)

    -- ReadPrec internals
    , step, reset, prec, pfail, (+++)

    -- Int/Word newtype constructors
    , Int8(I8#), Int16(I16#), Int32(I32#), Int64(I64#)
    , Word8(W8#), Word16(W16#), Word32(W32#), Word64(W64#)

    -- Error
    , error
    , assertError

    -- Numbers
    , Num, Integral, Real, Floating, Fractional, RealFloat, RealFrac
    , (+), (-), (*), negate, fromInteger
    , divInt#, modInt#

    , Ratio( (:%) ), Rational
    , mkRationalBase2, mkRationalBase10
    , toInteger, toRational
    , fromIntegral, fromRational
    , realToFrac

    -- Strings
    , IsString
    , fromString

    -- Records
    , HasField
    , fromLabel, getField, setField

    -- Overloaded lists
    , IL.fromList, IL.fromListN, IL.toList

    -- Arrows
    , arr, (>>>), first, app, (|||), loop

    -- IO
    , IO(IO), thenIO, bindIO, returnIO, print

    -- WithDict
    , WithDict

    -- Type-level naturals/symbols/chars
    , KnownNat, KnownSymbol, KnownChar

    -- Custom type errors
    , TypeError
    , ErrorMessage(..)

    -- Unsatisfiable
    , Unsatisfiable, unsatisfiable

    -- Static pointers
    , IsStatic( fromStaticPtr ), makeStatic
    , StaticPtr(..), StaticPtrInfo( StaticPtrInfo )

    -- Stable pointers
    , StablePtr, newStablePtr

    -- Dynamic
    , toDyn

    -- Run Top Handler (Note [Dealing with main])
    , runMainIO

    -- Base strings
    , CS.unpackCString# , CS.unpackCStringUtf8#

    -- Names that have BuiltinRules
    , CS.unpackFoldrCString#, CS.unpackFoldrCStringUtf8#, CS.unpackAppendCString#
    , CS.unpackAppendCStringUtf8#, CS.cstringLength#
    , eqString, inline, runRW#

    , UnsafeEquality( UnsafeRefl ), unsafeEqualityProof, unsafeCoerce#

    -- Typeable and type representations
    , SomeTypeRep( SomeTypeRep ), TR.Module( Module )
    , TyCon( TyCon ), TrName( TrNameS )
    , KindRep( KindRepTyConApp, KindRepVar, KindRepApp, KindRepFun, KindRepType, KindRepTypeLitS )
    , TypeLitSort( TypeLitSymbol, TypeLitNat, TypeLitChar )
    , typeRep#
    , mkTrCon, mkTrAppChecked, mkTrFun
    , typeNatTypeRep, typeSymbolTypeRep, typeCharTypeRep
    , krepStar, krepStarArr, krepStarArrStarArr, krepConstraint

    -- More things with RULES
    , integerToFloat#, integerToDouble#, rationalToFloat#, rationalToDouble#

    -- Bignums
    , bigNatEq#, bigNatCompare, bigNatCompareWord#
    , naturalToWord#, naturalPopCount#, naturalShiftR#, naturalShiftL#
    , naturalAdd, naturalSub, naturalSubThrow, naturalSubUnsafe
    , naturalMul, naturalQuotRem#, naturalQuot, naturalRem, naturalAnd
    , naturalOr, naturalXor, naturalTestBit#, naturalBit#, naturalGcd, naturalLcm

    , integerFromNatural, integerToNaturalClamp, integerToNaturalThrow, integerToNatural
    , integerToWord#, integerToInt#, integerToWord64#, integerToInt64#, integerFromWord#
    , integerFromWord64#, integerFromInt64#, integerAdd, integerMul, integerSub
    , integerNegate, integerAbs, integerPopCount#, integerQuot, integerRem, integerDiv
    , integerMod, integerDivMod#, integerQuotRem#, integerEncodeFloat#, integerEncodeDouble#
    , integerGcd, integerLcm, integerAnd, integerOr, integerXor
    , integerComplement, integerBit#, integerTestBit#, integerShiftL#, integerShiftR#

    -- Template Haskell
    , Lift, Quote  -- The Lift and Quote classeso
    , Q, DecsQ, ExpQ, TypeQ, PatQ
    , Name, Decs, TH.Type, FunDep
    , Pred, Code, InjectivityAnn, ModName
    , Con, BangType, VarBangType, RuleBndr, TySynEqn, Role, DerivClause
    , Kind, TyVarBndrUnit, TyVarBndrSpec, TyVarBndrVis, DerivStrategy
    , sequenceQ, newName, mkName, mkNameG_v, mkNameG_d, mkNameG_tc, mkNameG_fld, mkNameL
    , mkNameQ, mkNameS, mkModName, unType, unTypeCode, unsafeCodeCoerce
    , lift, liftString, liftTyped
    , Dec, funD, valD, dataD, newtypeD, typeDataD, tySynD, classD, instanceWithOverlapD
         , standaloneDerivWithStrategyD, sigD, kiSigD, defaultD, defaultSigD, forImpD
         , pragInlD, pragOpaqueD
         , pragSpecED, pragSpecInlED
         , pragSpecInstD, pragRuleD, pragCompleteD, pragAnnD, pragSCCFunD
         , pragSCCFunNamedD, dataInstD, newtypeInstD, tySynInstD, openTypeFamilyD
         , closedTypeFamilyD, dataFamilyD, infixLWithSpecD, infixRWithSpecD, infixNWithSpecD
         , roleAnnotD, patSynD, patSynSigD, implicitParamBindD
    , Lit, charL, stringL, integerL, intPrimL, wordPrimL, floatPrimL
         , doublePrimL, rationalL, stringPrimL, charPrimL
    , Pat, litP, varP, tupP, unboxedTupP, unboxedSumP, conP, infixP, tildeP
         , bangP, asP, wildP, recP, listP, sigP, viewP, orP, typeP, invisP
    , Exp, varE, conE, litE, appE, appTypeE, infixE, infixApp, sectionL, sectionR
         , lamE, lamCaseE, lamCasesE, tupE, unboxedTupE, unboxedSumE, condE, multiIfE
         , letE, caseE, doE, mdoE, compE, fromE, fromThenE, fromToE, fromThenToE
         , listE, sigE, recConE, recUpdE, staticE, unboundVarE, labelE, implicitParamVarE
         , getFieldE, projectionE, typeE, forallE, forallVisE, constrainedE
    , TH.cxt
    , TH.noSourceUnpackedness, TH.sourceNoUnpack, TH.sourceUnpack
    , TH.noSourceStrictness, TH.sourceLazy, TH.sourceStrict
    , FieldExp, fieldExp
    , FieldPat, fieldPat
    , Match, match
    , Clause, clause
    , Stmt, bindS, letS, noBindS, parS, recS
    , Body, normalB, guardedB
    , Guard, normalGE, patGE
    , normalC, recC, infixC, forallC, gadtC, recGadtC
    , TH.bang, TH.bangType, TH.varBangType
    , TH.unidir, TH.implBidir, TH.explBidir
    , TH.prefixPatSyn, TH.recordPatSyn, TH.infixPatSyn
    , forallT, forallVisT, varT, conT, infixT, tupleT
    , unboxedTupleT, unboxedSumT, arrowT, mulArrowT, listT
    , appT, appKindT, sigT, equalityT, litT, promotedT
    , promotedTupleT, promotedNilT, promotedConsT
    , wildCardT, implicitParamT
    , numTyLit, strTyLit, charTyLit
    , plainTV, kindedTV
    , plainBndrTV, kindedBndrTV
    , plainInvisTV, kindedInvisTV
    , specifiedSpec, inferredSpec
    , bndrReq, bndrInvis
    , nominalR, representationalR, phantomR, inferR
    , starK, constraintK
    , noSig, kindSig, tyVarSig
    , injectivityAnn
    , TH.cCall, TH.stdCall, TH.cApi, TH.prim, TH.javaScript
    , TH.unsafe, TH.safe, TH.interruptible
    , TH.ruleVar, TH.typedRuleVar
    , TH.funDep, TH.tySynEqn
    , TH.valueAnnotation, TH.typeAnnotation, TH.moduleAnnotation
    , TH.derivClause
    , TH.stockStrategy, TH.anyclassStrategy, TH.newtypeStrategy, TH.viaStrategy
    , TH.QuasiQuoter(..)
    , TH.Inline(..), TH.RuleMatch(..)
    , TH.Phases(..) , TH.Overlap(..)
    , TH.NamespaceSpecifier(..)

    -- GHCi
    , GHCiSandboxIO(ghciStepIO)

    -- Callstacks
    , CallStack, emptyCallStack, pushCallStack, SrcLoc(..)

    -- Exception context
    , ExceptionContext, emptyExceptionContext

    , toAnnotationWrapper

    -- Debug
    , trace

    -- JS primitives
    , JSVal
    , unsafeUnpackJSStringUtf8##
    ) where

import GHC.Internal.Base hiding( foldr )
import GHC.Internal.Debug.Trace( trace )
import GHC.Internal.Show
import GHC.Internal.Read
import GHC.Internal.Num
import GHC.Internal.Real
import Data.String( IsString )
import GHC.Internal.Ix
import GHC.Internal.Magic.Dict( WithDict )
import GHC.Internal.TypeNats( KnownNat )
import GHC.Internal.TypeLits( KnownSymbol, KnownChar )
import GHC.Internal.Enum
import GHC.Internal.Data.Dynamic( toDyn )
import GHC.Internal.Data.Data
import GHC.Internal.Data.String( fromString )
import GHC.Internal.Data.Either( Either(..) )
import GHC.Internal.Data.Foldable( Foldable(..), null, all )
import GHC.Internal.Data.Traversable( Traversable, traverse )
import GHC.Internal.Float
import GHC.Internal.IO( seq# )
import GHC.Internal.Control.Monad( MonadFail, fail, guard )
import GHC.Internal.Control.Monad.Fix( mfix, loop )
import GHC.Internal.Control.Monad.Zip( mzip )
import GHC.Internal.Control.Arrow( arr, first, app, (|||) )
import GHC.Internal.Desugar( (>>>) )  -- See Note [Tricky known-occ cases]
import GHC.Internal.OverloadedLabels( fromLabel )
import GHC.Internal.Records
import GHC.Internal.Types as CS
  ( unpackCString# , unpackCStringUtf8#
  , unpackFoldrCString#, unpackFoldrCStringUtf8#, unpackAppendCString#
  , unpackAppendCStringUtf8#, cstringLength# )
import GHC.Internal.TypeError( TypeError, ErrorMessage(..), Unsatisfiable, unsatisfiable )
import GHC.Internal.System.IO( print )
import qualified GHC.Internal.IsList as IL
import GHC.Internal.Err( error )
import GHC.Internal.IO.Exception( assertError )
import GHC.Internal.Int( Int8(I8#), Int16(I16#), Int32(I32#), Int64(I64#) )
import GHC.Internal.Word( Word8(W8#), Word16(W16#), Word32(W32#), Word64(W64#) )

import GHC.Internal.Unsafe.Coerce( UnsafeEquality(..), unsafeEqualityProof, unsafeCoerce# )

import GHC.Internal.StaticPtr( IsStatic(..), StaticPtr(..), StaticPtrInfo(..) )
import GHC.Internal.StaticPtr.Internal( makeStatic )

import GHC.Internal.Stable( StablePtr, newStablePtr )
import GHC.Internal.Data.Typeable( gcast1, gcast2 )
import GHC.Internal.Data.Typeable.Internal as TR
import GHC.Internal.Generics hiding( Fixity(..), prec )
import GHC.Internal.Bignum.BigNat

import GHC.Internal.TH.Syntax as TH
       hiding( Fixity(..), SourceUnpackedness(..), SourceStrictness(..)
             , DecidedStrictness(..), Module(..) )
import GHC.Internal.TH.Lib as TH
import GHC.Internal.TH.Lift
import GHC.Internal.TH.Monad as TH
import GHC.Internal.TopHandler
import GHC.Internal.GHCi
import GHC.Internal.Desugar (toAnnotationWrapper)
import GHC.Internal.Stack.Types
import GHC.Internal.Exception.Context
import GHC.Internal.Ptr
import GHC.Internal.Foreign.C.ConstPtr
#if defined(javascript_HOST_ARCH)
import GHC.Internal.JS.Prim (unsafeUnpackJSStringUtf8##, JSVal)
#elif defined(wasm32_HOST_ARCH)
import GHC.Internal.Wasm.Prim (JSVal)
#else
data JSVal
#endif

#if !defined(javascript_HOST_ARCH)
{-
Note [JS/WASM primitives known-keys for other targets]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The known-keys table (GHC.Builtin.KnownKeys.knownKeyTable) must list all
known-keys regardless of target, so it includes the JS/WASM primitives too.

The known-keys table cannot CPP the JS/WASM primitives out of the list, because
the target platform is not fixed at the compiler's compile time, only at the
compiler's run-time. e.g. a cross-compiler from an x64 host to a JS target
definitely needs JSVal in the known-keys table, even if the host is x64.

Therefore, we need to export *some* definition of JSVal and unsafeUnpackJSStringUtf8##,
even if the target for which we are compiling GHC.Essentials is not JavaScript nor WASM.

We define stubs which will be unused in the "other" case (these known-keys
won't be used unless compiling to JS, or WASM) and toggle
-fdefines-known-key-names in this module to make these stubs known-key definitions.
-}

-- See Note [JS/WASM primitives known-keys for other targets]
unsafeUnpackJSStringUtf8## :: a
unsafeUnpackJSStringUtf8## = error "unsafeUnpackJSStringUtf8## known-key was used when the compilation target wasn't javascript_HOST_ARCH"
#endif
