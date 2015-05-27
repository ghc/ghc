{-
(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


HsTypes: Abstract syntax: user-defined types
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}

module HsTypes (
        HsType(..), LHsType, HsKind, LHsKind,
        HsTyOp,LHsTyOp,
        HsTyVarBndr(..), LHsTyVarBndr,
        LHsTyVarBndrs(..),
        HsWithBndrs(..),
        HsTupleSort(..), HsExplicitFlag(..),
        HsContext, LHsContext,
        HsQuasiQuote(..),
        HsTyWrapper(..),
        HsTyLit(..),
        HsIPName(..), hsIPNameFS,

        LBangType, BangType, HsBang(..), HsSrcBang, HsImplBang,
        getBangType, getBangStrictness,

        ConDeclField(..), LConDeclField, pprConDeclFields,

        mkHsQTvs, hsQTvBndrs, isHsKindedTyVar, hsTvbAllKinded,
        mkExplicitHsForAllTy, mkImplicitHsForAllTy, mkQualifiedHsForAllTy,
        mkHsForAllTy,
        flattenTopLevelLHsForAllTy,flattenTopLevelHsForAllTy,
        flattenHsForAllTyKeepAnns,
        hsExplicitTvs,
        hsTyVarName, mkHsWithBndrs, hsLKiTyVarNames,
        hsLTyVarName, hsLTyVarNames, hsLTyVarLocName, hsLTyVarLocNames,
        splitLHsInstDeclTy_maybe,
        splitHsClassTy_maybe, splitLHsClassTy_maybe,
        splitHsFunType,
        splitHsAppTys, hsTyGetAppHead_maybe, mkHsAppTys, mkHsOpTy,
        isWildcardTy, isNamedWildcardTy,

        -- Printing
        pprParendHsType, pprHsForAll, pprHsForAllExtra,
        pprHsContext, pprHsContextNoArrow, pprHsContextMaybe
    ) where

import {-# SOURCE #-} HsExpr ( HsSplice, pprUntypedSplice )

import PlaceHolder ( PostTc,PostRn,DataId,PlaceHolder(..) )

import Name( Name )
import RdrName( RdrName )
import DataCon( HsBang(..), HsSrcBang, HsImplBang )
import TysPrim( funTyConName )
import Type
import HsDoc
import BasicTypes
import SrcLoc
import StaticFlags
import Outputable
import FastString
import Lexer ( AddAnn, mkParensApiAnn )
import Maybes( isJust )

import Data.Data hiding ( Fixity )
import Data.Maybe ( fromMaybe )
#if __GLASGOW_HASKELL__ < 709
import Data.Monoid hiding ((<>))
#endif

{-
************************************************************************
*                                                                      *
        Quasi quotes; used in types and elsewhere
*                                                                      *
************************************************************************
-}

data HsQuasiQuote id = HsQuasiQuote
                           id           -- The quasi-quoter
                           SrcSpan      -- The span of the enclosed string
                           FastString   -- The enclosed string
  deriving (Data, Typeable)

instance OutputableBndr id => Outputable (HsQuasiQuote id) where
    ppr = ppr_qq

ppr_qq :: OutputableBndr id => HsQuasiQuote id -> SDoc
ppr_qq (HsQuasiQuote quoter _ quote) =
    char '[' <> ppr quoter <> ptext (sLit "|") <>
    ppr quote <> ptext (sLit "|]")

{-
************************************************************************
*                                                                      *
\subsection{Bang annotations}
*                                                                      *
************************************************************************
-}

type LBangType name = Located (BangType name)
type BangType name  = HsType name       -- Bangs are in the HsType data type

getBangType :: LHsType a -> LHsType a
getBangType (L _ (HsBangTy _ ty)) = ty
getBangType ty                    = ty

getBangStrictness :: LHsType a -> HsSrcBang
getBangStrictness (L _ (HsBangTy s _)) = s
getBangStrictness _                    = HsNoBang

{-
************************************************************************
*                                                                      *
\subsection{Data types}
*                                                                      *
************************************************************************

This is the syntax for types as seen in type signatures.

Note [HsBSig binder lists]
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider a binder (or pattern) decoarated with a type or kind,
   \ (x :: a -> a). blah
   forall (a :: k -> *) (b :: k). blah
Then we use a LHsBndrSig on the binder, so that the
renamer can decorate it with the variables bound
by the pattern ('a' in the first example, 'k' in the second),
assuming that neither of them is in scope already
See also Note [Kind and type-variable binders] in RnTypes
-}

type LHsContext name = Located (HsContext name)
      -- ^ 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnUnit'

      -- For details on above see note [Api annotations] in ApiAnnotation

type HsContext name = [LHsType name]

type LHsType name = Located (HsType name)
      -- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnComma' when
      --   in a list

      -- For details on above see note [Api annotations] in ApiAnnotation
type HsKind name = HsType name
type LHsKind name = Located (HsKind name)
      -- ^ 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

      -- For details on above see note [Api annotations] in ApiAnnotation

type LHsTyVarBndr name = Located (HsTyVarBndr name)

data LHsTyVarBndrs name
  = HsQTvs { hsq_kvs :: [Name]                  -- Kind variables
           , hsq_tvs :: [LHsTyVarBndr name]     -- Type variables
             -- See Note [HsForAllTy tyvar binders]
    }
  deriving( Typeable )
deriving instance (DataId name) => Data (LHsTyVarBndrs name)

mkHsQTvs :: [LHsTyVarBndr RdrName] -> LHsTyVarBndrs RdrName
-- Just at RdrName because in the Name variant we should know just
-- what the kind-variable binders are; and we don't
-- We put an empty list (rather than a panic) for the kind vars so
-- that the pretty printer works ok on them.
mkHsQTvs tvs = HsQTvs { hsq_kvs = [], hsq_tvs = tvs }

emptyHsQTvs :: LHsTyVarBndrs name   -- Use only when you know there are no kind binders
emptyHsQTvs =  HsQTvs { hsq_kvs = [], hsq_tvs = [] }

hsQTvBndrs :: LHsTyVarBndrs name -> [LHsTyVarBndr name]
hsQTvBndrs = hsq_tvs

instance Monoid (LHsTyVarBndrs name) where
  mempty = emptyHsQTvs
  mappend (HsQTvs kvs1 tvs1) (HsQTvs kvs2 tvs2)
    = HsQTvs (kvs1 ++ kvs2) (tvs1 ++ tvs2)

------------------------------------------------
--            HsWithBndrs
-- Used to quantify the binders of a type in cases
-- when a HsForAll isn't appropriate:
--    * Patterns in a type/data family instance (HsTyPats)
--    * Type of a rule binder (RuleBndr)
--    * Pattern type signatures (SigPatIn)
-- In the last of these, wildcards can happen, so we must accommodate them

data HsWithBndrs name thing
  = HsWB { hswb_cts :: thing             -- Main payload (type or list of types)
         , hswb_kvs :: PostRn name [Name] -- Kind vars
         , hswb_tvs :: PostRn name [Name] -- Type vars
         , hswb_wcs :: PostRn name [Name] -- Wildcards
    }
  deriving (Typeable)
deriving instance (Data name, Data thing, Data (PostRn name [Name]))
  => Data (HsWithBndrs name thing)

mkHsWithBndrs :: thing -> HsWithBndrs RdrName thing
mkHsWithBndrs x = HsWB { hswb_cts = x, hswb_kvs = PlaceHolder
                                     , hswb_tvs = PlaceHolder
                                     , hswb_wcs = PlaceHolder }


-- | These names are used early on to store the names of implicit
-- parameters.  They completely disappear after type-checking.
newtype HsIPName = HsIPName FastString-- ?x
  deriving( Eq, Data, Typeable )

hsIPNameFS :: HsIPName -> FastString
hsIPNameFS (HsIPName n) = n

instance Outputable HsIPName where
    ppr (HsIPName n) = char '?' <> ftext n -- Ordinary implicit parameters

instance OutputableBndr HsIPName where
    pprBndr _ n   = ppr n         -- Simple for now
    pprInfixOcc  n = ppr n
    pprPrefixOcc n = ppr n

data HsTyVarBndr name
  = UserTyVar        -- no explicit kinding
         name

  | KindedTyVar
         (Located name)
         (LHsKind name)  -- The user-supplied kind signature
        -- ^
        --  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen',
        --          'ApiAnnotation.AnnDcolon', 'ApiAnnotation.AnnClose'

        -- For details on above see note [Api annotations] in ApiAnnotation
  deriving (Typeable)
deriving instance (DataId name) => Data (HsTyVarBndr name)

-- | Does this 'HsTyVarBndr' come with an explicit kind annotation?
isHsKindedTyVar :: HsTyVarBndr name -> Bool
isHsKindedTyVar (UserTyVar {})   = False
isHsKindedTyVar (KindedTyVar {}) = True

-- | Do all type variables in this 'LHsTyVarBndr' come with kind annotations?
hsTvbAllKinded :: LHsTyVarBndrs name -> Bool
hsTvbAllKinded = all (isHsKindedTyVar . unLoc) . hsQTvBndrs

data HsType name
  = HsForAllTy  HsExplicitFlag          -- Renamer leaves this flag unchanged, to record the way
                                        -- the user wrote it originally, so that the printer can
                                        -- print it as the user wrote it
                (Maybe SrcSpan)         -- Indicates whether extra constraints may be inferred.
                                        -- When Nothing, no, otherwise the location of the extra-
                                        -- constraints wildcard is stored. For instance, for the
                                        -- signature (Eq a, _) => a -> a -> Bool, this field would
                                        -- be something like (Just 1:8), with 1:8 being line 1,
                                        -- column 8.
                (LHsTyVarBndrs name)
                (LHsContext name)
                (LHsType name)
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnForall',
      --         'ApiAnnotation.AnnDot','ApiAnnotation.AnnDarrow'

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsTyVar             name            -- Type variable, type constructor, or data constructor
                                        -- see Note [Promotions (HsTyVar)]
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsAppTy             (LHsType name)
                        (LHsType name)
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsFunTy             (LHsType name)   -- function type
                        (LHsType name)
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnRarrow',

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsListTy            (LHsType name)  -- Element type
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'['@,
      --         'ApiAnnotation.AnnClose' @']'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsPArrTy            (LHsType name)  -- Elem. type of parallel array: [:t:]
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'[:'@,
      --         'ApiAnnotation.AnnClose' @':]'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsTupleTy           HsTupleSort
                        [LHsType name]  -- Element types (length gives arity)
    -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'(' or '(#'@,
    --         'ApiAnnotation.AnnClose' @')' or '#)'@

    -- For details on above see note [Api annotations] in ApiAnnotation

  | HsOpTy              (LHsType name) (LHsTyOp name) (LHsType name)
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsParTy             (LHsType name)   -- See Note [Parens in HsSyn] in HsExpr
        -- Parenthesis preserved for the precedence re-arrangement in RnTypes
        -- It's important that a * (b + c) doesn't get rearranged to (a*b) + c!
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
      --         'ApiAnnotation.AnnClose' @')'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsIParamTy          HsIPName         -- (?x :: ty)
                        (LHsType name)   -- Implicit parameters as they occur in contexts
      -- ^
      -- > (?x :: ty)
      --
      -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsEqTy              (LHsType name)   -- ty1 ~ ty2
                        (LHsType name)   -- Always allowed even without TypeOperators, and has special kinding rule
      -- ^
      -- > ty1 ~ ty2
      --
      -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnTilde'

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsKindSig           (LHsType name)  -- (ty :: kind)
                        (LHsKind name)  -- A type with a kind signature
      -- ^
      -- > (ty :: kind)
      --
      -- - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'('@,
      --         'ApiAnnotation.AnnDcolon','ApiAnnotation.AnnClose' @')'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsQuasiQuoteTy      (HsQuasiQuote name)
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsSpliceTy          (HsSplice name)
                        (PostTc name Kind)
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'$('@,
      --         'ApiAnnotation.AnnClose' @')'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsDocTy             (LHsType name) LHsDocString -- A documented type
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsBangTy    HsSrcBang (LHsType name)   -- Bang-style type annotations
      -- ^ - 'ApiAnnotation.AnnKeywordId' :
      --         'ApiAnnotation.AnnOpen' @'{-\# UNPACK' or '{-\# NOUNPACK'@,
      --         'ApiAnnotation.AnnClose' @'#-}'@
      --         'ApiAnnotation.AnnBang' @\'!\'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsRecTy     [LConDeclField name]    -- Only in data type declarations
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @'{'@,
      --         'ApiAnnotation.AnnClose' @'}'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsCoreTy Type       -- An escape hatch for tunnelling a *closed*
                        -- Core Type through HsSyn.
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsExplicitListTy       -- A promoted explicit list
        (PostTc name Kind) -- See Note [Promoted lists and tuples]
        [LHsType name]
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @"'["@,
      --         'ApiAnnotation.AnnClose' @']'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsExplicitTupleTy      -- A promoted explicit tuple
        [PostTc name Kind] -- See Note [Promoted lists and tuples]
        [LHsType name]
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnOpen' @"'("@,
      --         'ApiAnnotation.AnnClose' @')'@

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsTyLit HsTyLit      -- A promoted numeric literal.
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsWrapTy HsTyWrapper (HsType name)  -- only in typechecker output
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsWildcardTy           -- A type wildcard
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation

  | HsNamedWildcardTy name -- A named wildcard
      -- ^ - 'ApiAnnotation.AnnKeywordId' : None

      -- For details on above see note [Api annotations] in ApiAnnotation
  deriving (Typeable)
deriving instance (DataId name) => Data (HsType name)

-- Note [Literal source text] in BasicTypes for SourceText fields in
-- the following
data HsTyLit
  = HsNumTy SourceText Integer
  | HsStrTy SourceText FastString
    deriving (Data, Typeable)

data HsTyWrapper
  = WpKiApps [Kind]  -- kind instantiation: [] k1 k2 .. kn
  deriving (Data, Typeable)

type LHsTyOp name = HsTyOp (Located name)
type HsTyOp name = (HsTyWrapper, name)

mkHsOpTy :: LHsType name -> Located name -> LHsType name -> HsType name
mkHsOpTy ty1 op ty2 = HsOpTy ty1 (WpKiApps [], op) ty2

{-
Note [HsForAllTy tyvar binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
After parsing:
  * Implicit => empty
    Explicit => the variables the user wrote

After renaming
  * Implicit => the *type* variables free in the type
    Explicit => the variables the user wrote (renamed)

Qualified currently behaves exactly as Implicit,
but it is deprecated to use it for implicit quantification.
In this case, GHC 7.10 gives a warning; see
Note [Context quantification] and Trac #4426.
In GHC 7.12, Qualified will no longer bind variables
and this will become an error.

The kind variables bound in the hsq_kvs field come both
  a) from the kind signatures on the kind vars (eg k1)
  b) from the scope of the forall (eg k2)
Example:   f :: forall (a::k1) b. T a (b::k2)


Note [Unit tuples]
~~~~~~~~~~~~~~~~~~
Consider the type
    type instance F Int = ()
We want to parse that "()"
    as HsTupleTy HsBoxedOrConstraintTuple [],
NOT as HsTyVar unitTyCon

Why? Because F might have kind (* -> Constraint), so we when parsing we
don't know if that tuple is going to be a constraint tuple or an ordinary
unit tuple.  The HsTupleSort flag is specifically designed to deal with
that, but it has to work for unit tuples too.

Note [Promotions (HsTyVar)]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
HsTyVar: A name in a type or kind.
  Here are the allowed namespaces for the name.
    In a type:
      Var: not allowed
      Data: promoted data constructor
      Tv: type variable
      TcCls before renamer: type constructor, class constructor, or promoted data constructor
      TcCls after renamer: type constructor or class constructor
    In a kind:
      Var, Data: not allowed
      Tv: kind variable
      TcCls: kind constructor or promoted type constructor


Note [Promoted lists and tuples]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Notice the difference between
   HsListTy    HsExplicitListTy
   HsTupleTy   HsExplicitListTupleTy

E.g.    f :: [Int]                      HsListTy

        g3  :: T '[]                   All these use
        g2  :: T '[True]                  HsExplicitListTy
        g1  :: T '[True,False]
        g1a :: T [True,False]             (can omit ' where unambiguous)

  kind of T :: [Bool] -> *        This kind uses HsListTy!

E.g.    h :: (Int,Bool)                 HsTupleTy; f is a pair
        k :: S '(True,False)            HsExplicitTypleTy; S is indexed by
                                           a type-level pair of booleans
        kind of S :: (Bool,Bool) -> *   This kind uses HsExplicitTupleTy

Note [Distinguishing tuple kinds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Apart from promotion, tuples can have one of three different kinds:

        x :: (Int, Bool)                -- Regular boxed tuples
        f :: Int# -> (# Int#, Int# #)   -- Unboxed tuples
        g :: (Eq a, Ord a) => a         -- Constraint tuples

For convenience, internally we use a single constructor for all of these,
namely HsTupleTy, but keep track of the tuple kind (in the first argument to
HsTupleTy, a HsTupleSort). We can tell if a tuple is unboxed while parsing,
because of the #. However, with -XConstraintKinds we can only distinguish
between constraint and boxed tuples during type checking, in general. Hence the
four constructors of HsTupleSort:

        HsUnboxedTuple                  -> Produced by the parser
        HsBoxedTuple                    -> Certainly a boxed tuple
        HsConstraintTuple               -> Certainly a constraint tuple
        HsBoxedOrConstraintTuple        -> Could be a boxed or a constraint
                                        tuple. Produced by the parser only,
                                        disappears after type checking
-}

data HsTupleSort = HsUnboxedTuple
                 | HsBoxedTuple
                 | HsConstraintTuple
                 | HsBoxedOrConstraintTuple
                 deriving (Data, Typeable)

data HsExplicitFlag = Qualified | Implicit | Explicit deriving (Data, Typeable)

type LConDeclField name = Located (ConDeclField name)
      -- ^ May have 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnComma' when
      --   in a list

      -- For details on above see note [Api annotations] in ApiAnnotation
data ConDeclField name  -- Record fields have Haddoc docs on them
  = ConDeclField { cd_fld_names :: [Located name],
                   cd_fld_type  :: LBangType name,
                   cd_fld_doc   :: Maybe LHsDocString }
      -- ^ - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnDcolon'

      -- For details on above see note [Api annotations] in ApiAnnotation
  deriving (Typeable)
deriving instance (DataId name) => Data (ConDeclField name)

-----------------------
-- A valid type must have a for-all at the top of the type, or of the fn arg
-- types

mkImplicitHsForAllTy  ::                                                 LHsType RdrName -> HsType RdrName
mkExplicitHsForAllTy  :: [LHsTyVarBndr RdrName] -> LHsContext RdrName -> LHsType RdrName -> HsType RdrName
mkQualifiedHsForAllTy ::                           LHsContext RdrName -> LHsType RdrName -> HsType RdrName

-- | mkImplicitHsForAllTy is called when we encounter
--    f :: type
-- Wrap around a HsForallTy if one is not there already.
mkImplicitHsForAllTy (L _ (HsForAllTy exp extra tvs cxt ty))
  = HsForAllTy exp' extra tvs cxt ty
  where
    exp' = case exp of
             Qualified -> Implicit
                          -- Qualified is used only for a nested forall,
                          -- this is now top level
             _         -> exp
mkImplicitHsForAllTy ty = mkHsForAllTy Implicit  [] (noLoc []) ty

mkExplicitHsForAllTy  tvs ctxt ty = mkHsForAllTy Explicit  tvs ctxt ty
mkQualifiedHsForAllTy     ctxt ty = mkHsForAllTy Qualified []  ctxt ty

-- |Smart constructor for HsForAllTy, which populates the extra-constraints
-- field if a wildcard is present in the context.
mkHsForAllTy :: HsExplicitFlag -> [LHsTyVarBndr RdrName] -> LHsContext RdrName -> LHsType RdrName -> HsType RdrName
mkHsForAllTy exp tvs (L l []) ty
  = HsForAllTy exp Nothing (mkHsQTvs tvs) (L l []) ty
mkHsForAllTy exp tvs ctxt     ty
  = HsForAllTy exp extra   (mkHsQTvs tvs) cleanCtxt        ty
  where -- Separate the extra-constraints wildcard when present
        (cleanCtxt, extra)
          | (L l HsWildcardTy) <- ignoreParens (last (unLoc ctxt)) = (init `fmap` ctxt, Just l)
          | otherwise = (ctxt, Nothing)
        ignoreParens (L _ (HsParTy ty)) = ty
        ignoreParens ty                 = ty


-- |When a sigtype is parsed, the type found is wrapped in an Implicit
-- HsForAllTy via mkImplicitHsForAllTy, to ensure that a signature always has a
-- forall at the outer level. For Api Annotations this nested structure is
-- important to ensure that all `forall` and `.` locations are retained.  From
-- the renamer onwards this structure is flattened, to ease the renaming and
-- type checking process.
flattenTopLevelLHsForAllTy :: LHsType name -> LHsType name
flattenTopLevelLHsForAllTy (L l ty) = L l (flattenTopLevelHsForAllTy ty)

flattenTopLevelHsForAllTy :: HsType name -> HsType name
flattenTopLevelHsForAllTy (HsForAllTy exp extra tvs (L l []) ty)
  = snd $ mk_forall_ty [] l exp extra tvs ty
flattenTopLevelHsForAllTy ty = ty

flattenHsForAllTyKeepAnns :: HsType name -> ([AddAnn],HsType name)
flattenHsForAllTyKeepAnns (HsForAllTy exp extra tvs (L l []) ty)
  = mk_forall_ty [] l exp extra tvs ty
flattenHsForAllTyKeepAnns ty = ([],ty)

-- mk_forall_ty makes a pure for-all type (no context)
mk_forall_ty :: [AddAnn] -> SrcSpan -> HsExplicitFlag -> Maybe SrcSpan
             -> LHsTyVarBndrs name
             -> LHsType name -> ([AddAnn],HsType name)
mk_forall_ty ann _ exp1 extra1 tvs1 (L _ (HsForAllTy exp2 extra qtvs2 ctxt ty))
  = (ann,HsForAllTy (exp1 `plus` exp2) (mergeExtra extra1 extra)
                    (tvs1 `mappend` qtvs2) ctxt ty)
  where
        -- Bias the merging of extra's to the top level, so that a single
        -- wildcard context will prevail
        mergeExtra (Just s) _ = Just s
        mergeExtra _        e = e
mk_forall_ty ann l exp  extra tvs  (L lp (HsParTy ty))
  = mk_forall_ty (ann ++ mkParensApiAnn lp) l exp extra tvs ty
mk_forall_ty ann l exp extra tvs  ty
  = (ann,HsForAllTy exp extra tvs (L l []) ty)
        -- Even if tvs is empty, we still make a HsForAll!
        -- In the Implicit case, this signals the place to do implicit quantification
        -- In the Explicit case, it prevents implicit quantification
        --      (see the sigtype production in Parser.y)
        --      so that (forall. ty) isn't implicitly quantified

plus :: HsExplicitFlag -> HsExplicitFlag -> HsExplicitFlag
Qualified `plus` Qualified = Qualified
Explicit  `plus` _         = Explicit
_         `plus` Explicit  = Explicit
_         `plus` _         = Implicit

---------------------
hsExplicitTvs :: LHsType Name -> [Name]
-- The explicitly-given forall'd type variables of a HsType
hsExplicitTvs (L _ (HsForAllTy Explicit _ tvs _ _)) = hsLKiTyVarNames tvs
hsExplicitTvs _                                     = []

---------------------
hsTyVarName :: HsTyVarBndr name -> name
hsTyVarName (UserTyVar n)           = n
hsTyVarName (KindedTyVar (L _ n) _) = n

hsLTyVarName :: LHsTyVarBndr name -> name
hsLTyVarName = hsTyVarName . unLoc

hsLTyVarNames :: LHsTyVarBndrs name -> [name]
-- Type variables only
hsLTyVarNames qtvs = map hsLTyVarName (hsQTvBndrs qtvs)

hsLKiTyVarNames :: LHsTyVarBndrs Name -> [Name]
-- Kind and type variables
hsLKiTyVarNames (HsQTvs { hsq_kvs = kvs, hsq_tvs = tvs })
  = kvs ++ map hsLTyVarName tvs

hsLTyVarLocName :: LHsTyVarBndr name -> Located name
hsLTyVarLocName = fmap hsTyVarName

hsLTyVarLocNames :: LHsTyVarBndrs name -> [Located name]
hsLTyVarLocNames qtvs = map hsLTyVarLocName (hsQTvBndrs qtvs)

---------------------
isWildcardTy :: HsType a -> Bool
isWildcardTy HsWildcardTy = True
isWildcardTy _ = False

isNamedWildcardTy :: HsType a -> Bool
isNamedWildcardTy (HsNamedWildcardTy _) = True
isNamedWildcardTy _ = False

splitHsAppTys :: LHsType n -> [LHsType n] -> (LHsType n, [LHsType n])
splitHsAppTys (L _ (HsAppTy f a)) as = splitHsAppTys f (a:as)
splitHsAppTys (L _ (HsParTy f))   as = splitHsAppTys f as
splitHsAppTys f                   as = (f,as)

-- retrieve the name of the "head" of a nested type application
-- somewhat like splitHsAppTys, but a little more thorough
-- used to examine the result of a GADT-like datacon, so it doesn't handle
-- *all* cases (like lists, tuples, (~), etc.)
hsTyGetAppHead_maybe :: LHsType n -> Maybe (n, [LHsType n])
hsTyGetAppHead_maybe = go []
  where
    go tys (L _ (HsTyVar n))             = Just (n, tys)
    go tys (L _ (HsAppTy l r))           = go (r : tys) l
    go tys (L _ (HsOpTy l (_, L _ n) r)) = Just (n, l : r : tys)
    go tys (L _ (HsParTy t))             = go tys t
    go tys (L _ (HsKindSig t _))         = go tys t
    go _   _                             = Nothing

mkHsAppTys :: OutputableBndr n => LHsType n -> [LHsType n] -> HsType n
mkHsAppTys fun_ty [] = pprPanic "mkHsAppTys" (ppr fun_ty)
mkHsAppTys fun_ty (arg_ty:arg_tys)
  = foldl mk_app (HsAppTy fun_ty arg_ty) arg_tys
  where
    mk_app fun arg = HsAppTy (noLoc fun) arg
       -- Add noLocs for inner nodes of the application;
       -- they are never used

splitLHsInstDeclTy_maybe
    :: LHsType name
    -> Maybe (LHsTyVarBndrs name, HsContext name, Located name, [LHsType name])
        -- Split up an instance decl type, returning the pieces
splitLHsInstDeclTy_maybe inst_ty = do
    let (tvs, cxt, ty) = splitLHsForAllTy inst_ty
    (cls, tys) <- splitLHsClassTy_maybe ty
    return (tvs, cxt, cls, tys)

splitLHsForAllTy
    :: LHsType name
    -> (LHsTyVarBndrs name, HsContext name, LHsType name)
splitLHsForAllTy poly_ty
  = case unLoc poly_ty of
        HsParTy ty                -> splitLHsForAllTy ty
        HsForAllTy _ _ tvs cxt ty -> (tvs, unLoc cxt, ty)
        _                         -> (emptyHsQTvs, [], poly_ty)
        -- The type vars should have been computed by now, even if they were implicit

splitHsClassTy_maybe :: HsType name -> Maybe (name, [LHsType name])
splitHsClassTy_maybe ty = fmap (\(L _ n, tys) -> (n, tys)) $ splitLHsClassTy_maybe (noLoc ty)

splitLHsClassTy_maybe :: LHsType name -> Maybe (Located name, [LHsType name])
--- Watch out.. in ...deriving( Show )... we use this on
--- the list of partially applied predicates in the deriving,
--- so there can be zero args.

-- In TcDeriv we also use this to figure out what data type is being
-- mentioned in a deriving (Generic (Foo bar baz)) declaration (i.e. "Foo").
splitLHsClassTy_maybe ty
  = checkl ty []
  where
    checkl (L l ty) args = case ty of
        HsTyVar t          -> Just (L l t, args)
        HsAppTy l r        -> checkl l (r:args)
        HsOpTy l (_, tc) r -> checkl (fmap HsTyVar tc) (l:r:args)
        HsParTy t          -> checkl t args
        HsKindSig ty _     -> checkl ty args
        _                  -> Nothing

-- splitHsFunType decomposes a type (t1 -> t2 ... -> tn)
-- Breaks up any parens in the result type:
--      splitHsFunType (a -> (b -> c)) = ([a,b], c)
-- Also deals with (->) t1 t2; that is why it only works on LHsType Name
--   (see Trac #9096)
splitHsFunType :: LHsType Name -> ([LHsType Name], LHsType Name)
splitHsFunType (L _ (HsParTy ty))
  = splitHsFunType ty

splitHsFunType (L _ (HsFunTy x y))
  | (args, res) <- splitHsFunType y
  = (x:args, res)

splitHsFunType orig_ty@(L _ (HsAppTy t1 t2))
  = go t1 [t2]
  where  -- Look for (->) t1 t2, possibly with parenthesisation
    go (L _ (HsTyVar fn))    tys | fn == funTyConName
                                 , [t1,t2] <- tys
                                 , (args, res) <- splitHsFunType t2
                                 = (t1:args, res)
    go (L _ (HsAppTy t1 t2)) tys = go t1 (t2:tys)
    go (L _ (HsParTy ty))    tys = go ty tys
    go _                     _   = ([], orig_ty)  -- Failure to match

splitHsFunType other = ([], other)

{-
************************************************************************
*                                                                      *
\subsection{Pretty printing}
*                                                                      *
************************************************************************
-}

instance (OutputableBndr name) => Outputable (HsType name) where
    ppr ty = pprHsType ty

instance Outputable HsTyLit where
    ppr = ppr_tylit

instance (OutputableBndr name) => Outputable (LHsTyVarBndrs name) where
    ppr (HsQTvs { hsq_kvs = kvs, hsq_tvs = tvs })
      = sep [ ifPprDebug $ braces (interppSP kvs), interppSP tvs ]

instance (OutputableBndr name) => Outputable (HsTyVarBndr name) where
    ppr (UserTyVar n)     = ppr n
    ppr (KindedTyVar n k) = parens $ hsep [ppr n, dcolon, ppr k]

instance (Outputable thing) => Outputable (HsWithBndrs name thing) where
    ppr (HsWB { hswb_cts = ty }) = ppr ty

pprHsForAll :: OutputableBndr name => HsExplicitFlag -> LHsTyVarBndrs name -> LHsContext name -> SDoc
pprHsForAll exp = pprHsForAllExtra exp Nothing

-- | Version of 'pprHsForAll' that can also print an extra-constraints
-- wildcard, e.g. @_ => a -> Bool@ or @(Show a, _) => a -> String@. This
-- underscore will be printed when the 'Maybe SrcSpan' argument is a 'Just'
-- containing the location of the extra-constraints wildcard. A special
-- function for this is needed, as the extra-constraints wildcard is removed
-- from the actual context and type, and stored in a separate field, thus just
-- printing the type will not print the extra-constraints wildcard.
pprHsForAllExtra :: OutputableBndr name => HsExplicitFlag -> Maybe SrcSpan -> LHsTyVarBndrs name -> LHsContext name -> SDoc
pprHsForAllExtra exp extra qtvs cxt
  | show_forall = forall_part <+> pprHsContextExtra show_extra (unLoc cxt)
  | otherwise   = pprHsContextExtra show_extra (unLoc cxt)
  where
    show_extra  = isJust extra
    show_forall =  opt_PprStyle_Debug
                || (not (null (hsQTvBndrs qtvs)) && is_explicit)
    is_explicit = case exp of {Explicit -> True; Implicit -> False; Qualified -> False}
    forall_part = forAllLit <+> ppr qtvs <> dot

pprHsContext :: (OutputableBndr name) => HsContext name -> SDoc
pprHsContext = maybe empty (<+> darrow) . pprHsContextMaybe

pprHsContextNoArrow :: (OutputableBndr name) => HsContext name -> SDoc
pprHsContextNoArrow = fromMaybe empty . pprHsContextMaybe

pprHsContextMaybe :: (OutputableBndr name) => HsContext name -> Maybe SDoc
pprHsContextMaybe []         = Nothing
pprHsContextMaybe [L _ pred] = Just $ ppr_mono_ty FunPrec pred
pprHsContextMaybe cxt        = Just $ parens (interpp'SP cxt)

-- True <=> print an extra-constraints wildcard, e.g. @(Show a, _) =>@
pprHsContextExtra :: (OutputableBndr name) => Bool -> HsContext name -> SDoc
pprHsContextExtra False = pprHsContext
pprHsContextExtra True
  = \ctxt -> case ctxt of
               [] -> char '_' <+> darrow
               _  -> parens (sep (punctuate comma ctxt')) <+> darrow
                 where ctxt' = map ppr ctxt ++ [char '_']

pprConDeclFields :: OutputableBndr name => [LConDeclField name] -> SDoc
pprConDeclFields fields = braces (sep (punctuate comma (map ppr_fld fields)))
  where
    ppr_fld (L _ (ConDeclField { cd_fld_names = ns, cd_fld_type = ty,
                                 cd_fld_doc = doc }))
        = ppr_names ns <+> dcolon <+> ppr ty <+> ppr_mbDoc doc
    ppr_names [n] = ppr n
    ppr_names ns = sep (punctuate comma (map ppr ns))

{-
Note [Printing KindedTyVars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Trac #3830 reminded me that we should really only print the kind
signature on a KindedTyVar if the kind signature was put there by the
programmer.  During kind inference GHC now adds a PostTcKind to UserTyVars,
rather than converting to KindedTyVars as before.

(As it happens, the message in #3830 comes out a different way now,
and the problem doesn't show up; but having the flag on a KindedTyVar
seems like the Right Thing anyway.)
-}

-- Printing works more-or-less as for Types

pprHsType, pprParendHsType :: (OutputableBndr name) => HsType name -> SDoc

pprHsType ty       = getPprStyle $ \sty -> ppr_mono_ty TopPrec (prepare sty ty)
pprParendHsType ty = ppr_mono_ty TyConPrec ty

-- Before printing a type
-- (a) Remove outermost HsParTy parens
-- (b) Drop top-level for-all type variables in user style
--     since they are implicit in Haskell
prepare :: PprStyle -> HsType name -> HsType name
prepare sty (HsParTy ty)          = prepare sty (unLoc ty)
prepare _   ty                    = ty

ppr_mono_lty :: (OutputableBndr name) => TyPrec -> LHsType name -> SDoc
ppr_mono_lty ctxt_prec ty = ppr_mono_ty ctxt_prec (unLoc ty)

ppr_mono_ty :: (OutputableBndr name) => TyPrec -> HsType name -> SDoc
ppr_mono_ty ctxt_prec (HsForAllTy exp extra tvs ctxt ty)
  = maybeParen ctxt_prec FunPrec $
    sep [pprHsForAllExtra exp extra tvs ctxt, ppr_mono_lty TopPrec ty]

ppr_mono_ty _    (HsBangTy b ty)     = ppr b <> ppr_mono_lty TyConPrec ty
ppr_mono_ty _    (HsQuasiQuoteTy qq) = ppr qq
ppr_mono_ty _    (HsRecTy flds)      = pprConDeclFields flds
ppr_mono_ty _    (HsTyVar name)      = pprPrefixOcc name
ppr_mono_ty prec (HsFunTy ty1 ty2)   = ppr_fun_ty prec ty1 ty2
ppr_mono_ty _    (HsTupleTy con tys) = tupleParens std_con (interpp'SP tys)
  where std_con = case con of
                    HsUnboxedTuple -> UnboxedTuple
                    _              -> BoxedTuple
ppr_mono_ty _    (HsKindSig ty kind) = parens (ppr_mono_lty TopPrec ty <+> dcolon <+> ppr kind)
ppr_mono_ty _    (HsListTy ty)       = brackets (ppr_mono_lty TopPrec ty)
ppr_mono_ty _    (HsPArrTy ty)       = paBrackets (ppr_mono_lty TopPrec ty)
ppr_mono_ty prec (HsIParamTy n ty)   = maybeParen prec FunPrec (ppr n <+> dcolon <+> ppr_mono_lty TopPrec ty)
ppr_mono_ty _    (HsSpliceTy s _)    = pprUntypedSplice s
ppr_mono_ty _    (HsCoreTy ty)       = ppr ty
ppr_mono_ty _    (HsExplicitListTy _ tys) = quote $ brackets (interpp'SP tys)
ppr_mono_ty _    (HsExplicitTupleTy _ tys) = quote $ parens (interpp'SP tys)
ppr_mono_ty _    (HsTyLit t)         = ppr_tylit t
ppr_mono_ty _    HsWildcardTy        = char '_'
ppr_mono_ty _    (HsNamedWildcardTy name) = ppr name

ppr_mono_ty ctxt_prec (HsWrapTy (WpKiApps _kis) ty)
  = ppr_mono_ty ctxt_prec ty
-- We are not printing kind applications. If we wanted to do so, we should do
-- something like this:
{-
  = go ctxt_prec kis ty
  where
    go ctxt_prec [] ty = ppr_mono_ty ctxt_prec ty
    go ctxt_prec (ki:kis) ty
      = maybeParen ctxt_prec TyConPrec $
        hsep [ go FunPrec kis ty
             , ptext (sLit "@") <> pprParendKind ki ]
-}

ppr_mono_ty ctxt_prec (HsEqTy ty1 ty2)
  = maybeParen ctxt_prec TyOpPrec $
    ppr_mono_lty TyOpPrec ty1 <+> char '~' <+> ppr_mono_lty TyOpPrec ty2

ppr_mono_ty ctxt_prec (HsAppTy fun_ty arg_ty)
  = maybeParen ctxt_prec TyConPrec $
    hsep [ppr_mono_lty FunPrec fun_ty, ppr_mono_lty TyConPrec arg_ty]

ppr_mono_ty ctxt_prec (HsOpTy ty1 (_wrapper, L _ op) ty2)
  = maybeParen ctxt_prec TyOpPrec $
    sep [ ppr_mono_lty TyOpPrec ty1
        , sep [pprInfixOcc op, ppr_mono_lty TyOpPrec ty2 ] ]
    -- Don't print the wrapper (= kind applications)
    -- c.f. HsWrapTy

ppr_mono_ty _         (HsParTy ty)
  = parens (ppr_mono_lty TopPrec ty)
  -- Put the parens in where the user did
  -- But we still use the precedence stuff to add parens because
  --    toHsType doesn't put in any HsParTys, so we may still need them

ppr_mono_ty ctxt_prec (HsDocTy ty doc)
  = maybeParen ctxt_prec TyOpPrec $
    ppr_mono_lty TyOpPrec ty <+> ppr (unLoc doc)
  -- we pretty print Haddock comments on types as if they were
  -- postfix operators

--------------------------
ppr_fun_ty :: (OutputableBndr name) => TyPrec -> LHsType name -> LHsType name -> SDoc
ppr_fun_ty ctxt_prec ty1 ty2
  = let p1 = ppr_mono_lty FunPrec ty1
        p2 = ppr_mono_lty TopPrec ty2
    in
    maybeParen ctxt_prec FunPrec $
    sep [p1, ptext (sLit "->") <+> p2]

--------------------------
ppr_tylit :: HsTyLit -> SDoc
ppr_tylit (HsNumTy _ i) = integer i
ppr_tylit (HsStrTy _ s) = text (show s)
