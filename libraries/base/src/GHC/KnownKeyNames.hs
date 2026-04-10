{-# LANGUAGE MagicHash, Trustworthy, RankNTypes #-}

-- |
--
-- Module      :  GHC.KnownKeyNames
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  ghc-devs@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--

module GHC.KnownKeyNames
    ( Eq(..), Ord(..)  -- With their methods
    , Show, Read
    , Foldable, Traversable
    , Functor, fmap
    , Monad, (>>), (>>=), return, fail, guard, mfix, join
    , Alternative

    -- Misc
    , (.), (&&), not, map, foldr, build
    , seq#

    -- Applicative
    , Applicative, pure, mzip, (<*>), (*>)

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
    , Data
    , gfoldl, gunfold, toConstr, dataTypeOf, dataCast1, dataCast2
    , mkConstrTag, Constr, mkDataType, DataType, constrIndex

    -- Typeable
    , Typeable
    , gcast1, gcast2

    -- Generics
    , Generic, Generic1

    -- DataToTag
    , DataToTag
    , dataToTag#

    -- Numbers
    , Num, Integral, Real, Fractional, RealFloat, RealFrac
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
    , IO, thenIO, bindIO, returnIO, print

    -- WithDict
    , WithDict

    -- Unsatisfiable
    , Unsatisfiable, unsatisfiable

    -- Static pointers
    , IsStatic( fromStaticPtr ), makeStatic

    -- Dynamic
    , toDyn

    -- Names that have BuiltinRules
    , CS.unpackFoldrCString#, CS.unpackFoldrCStringUtf8#, CS.unpackAppendCString#
    , CS.unpackAppendCStringUtf8#, CS.cstringLength#
    , eqString, inline

    , UnsafeEquality( UnsafeRefl ), unsafeEqualityProof

    -- Typeable and type representations
    , SomeTypeRep( SomeTypeRep ), TR.Module( Module )
    , TyCon( TyCon ), TrName( TrNameS )
    , KindRep( KindRepTyConApp, KindRepVar, KindRepApp, KindRepFun, KindRepTYPE, KindRepTypeLitS )
    , TypeLitSort( TypeLitSymbol, TypeLitNat, TypeLitChar )
    , typeRep#
    , mkTrCon, mkTrAppChecked, mkTrFun
    , typeNatTypeRep, typeSymbolTypeRep, typeCharTypeRep

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
    , Q, DecsQ, ExpQ, TypeQ, PatQ
    , Name, Decs, TH.Type, FunDep
    , Pred, Code, InjectivityAnn, Overlap, ModName, QuasiQuoter
    , Stmt, Con, BangType, VarBangType, RuleBndr, TySynEqn, Role, DerivClause
    , Kind, TyVarBndrUnit, TyVarBndrSpec, TyVarBndrVis, DerivStrategy
    , sequenceQ, newName, mkName, mkNameG_v, mkNameG_d, mkNameG_tc, mkNameG_fld, mkNameL
    , mkNameQ, mkNameS, mkModName, unType, unTypeCode, unsafeCodeCoerce
    , lift, liftString, liftTyped
    , Dec, funD, valD, dataD, newtypeD, typeDataD, tySynD, classD, instanceWithOverlapD
         , standaloneDerivWithStrategyD, sigD, kiSigD, defaultD, defaultSigD, forImpD
         , pragInlD, pragOpaqueD
-- ToDo: why are these two out of scope??
--         , pragSpecD, pragSpecInlD
-- End of ToDo
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
    , FieldExp, fieldExp
    , FieldPat, fieldPat
    , Match, match
    , Clause, clause
    ) where

import GHC.Internal.Base
import GHC.Internal.Show
import GHC.Internal.Read
import GHC.Internal.Num
import GHC.Internal.Real
import Data.String( IsString )
import GHC.Internal.Ix
import GHC.Internal.Magic.Dict( WithDict )
import GHC.Internal.Enum
import GHC.Internal.Data.Dynamic( toDyn )
import GHC.Internal.Data.Data
import GHC.Internal.Data.String( fromString )
import GHC.Internal.Data.Foldable( Foldable )
import GHC.Internal.Data.Traversable( Traversable )
import GHC.Internal.Float( RealFloat )
import GHC.Internal.IO( seq# )
import GHC.Internal.Control.Monad( fail, guard )
import GHC.Internal.Control.Monad.Fix( mfix, loop )
import GHC.Internal.Control.Monad.Zip( mzip )
import GHC.Internal.Control.Arrow( arr, (>>>), first, app, (|||) )
import GHC.Internal.OverloadedLabels( fromLabel )
import GHC.Internal.Records
import GHC.Internal.CString as CS
import GHC.Internal.TypeError( Unsatisfiable, unsatisfiable )
import GHC.Internal.System.IO( print )
import qualified GHC.Internal.IsList as IL

import GHC.Internal.Unsafe.Coerce( UnsafeEquality(..), unsafeEqualityProof )

import GHC.Internal.StaticPtr( IsStatic(..) )
import GHC.Internal.StaticPtr.Internal( makeStatic )

import GHC.Internal.Data.Typeable( gcast1, gcast2 )
import GHC.Internal.Data.Typeable.Internal as TR
import GHC.Internal.Generics

import GHC.Internal.Bignum.BigNat

import GHC.Internal.TH.Syntax as TH
import GHC.Internal.TH.Lib hiding( InjectivityAnn, Role )
import GHC.Internal.TH.Lift
import GHC.Internal.TH.Monad
