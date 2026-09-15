-- | Handy functions for creating much Core syntax
module GHC.Core.Make (
        -- * Constructing normal syntax
        mkCoreLet, mkCoreLets,
        mkCoreApp, mkCoreApps, mkCoreConApps, mkCoreConWrapApps,
        mkCoreLams, mkCoreTyLams,
        mkWildCase, mkIfThenElse,
        mkWildValBinder,
        mkSingleAltCase,
        sortQuantVars, castBottomExpr,

        -- * Constructing boxed literals
        mkLitRubbish,
        mkWordExpr,
        mkIntExpr, mkIntExprInt, mkUncheckedIntExpr,
        mkIntegerExpr, mkNaturalExpr,
        mkFloatExpr, mkDoubleExpr,
        mkCharExpr, mkStringExprWith, mkStringExprFSWith,
        MkStringIds (..), getMkStringIds,

        -- * Constructing small tuples
        mkCoreVarTupTy, mkCoreTup, mkCoreBoxedTuple,
        mkCoreUnboxedTuple, mkCoreUnboxedSum,
        mkCoreTupBoxity, unitExpr,

        -- * Pattern matching on chunked tuples
        mkChunkedTupleCase,

        -- * Constructing list expressions
        mkNilExpr, mkConsExpr, mkListExpr,

        -- * Constructing Maybe expressions
        mkNothingExpr, mkJustExpr,

        -- * Floats
        wrapFloat, wrapFloats,

        -- * Error Ids
        mkRuntimeErrorApp, mkImpossibleExpr, mkAbsentErrorApp, errorIds,
        rEC_CON_ERROR_ID,
        nON_EXHAUSTIVE_GUARDS_ERROR_ID, nO_METHOD_BINDING_ERROR_ID,
        pAT_ERROR_ID, rEC_SEL_ERROR_ID,
        tYPE_ERROR_ID, aBSENT_SUM_FIELD_ERROR_ID
    ) where

import GHC.Prelude
import GHC.Platform

import GHC.Types.Id
import GHC.Types.Var  ( visArgConstraintLike )
import GHC.Types.Id.Info
import GHC.Types.Cpr
import GHC.Types.Basic( TypeOrConstraint(..) )
import GHC.Types.Demand
import GHC.Types.Name      hiding ( varName )
import GHC.Types.Literal
import GHC.Types.Unique.Supply ( MonadUnique )

import GHC.Core
import GHC.Core.Utils ( exprType, mkSingleAltCase, bindNonRec, mkCast, mkTick )
import GHC.Core.Type
import GHC.Core.Predicate    ( scopedSort, isEqPred )
import GHC.Core.TyCo.Compare ( eqType )
import GHC.Core.Coercion     ( isCoVar, mkRepReflCo, mkForAllVisCos )
import GHC.Core.DataCon      ( DataCon, dataConWorkId, dataConWrapId )
import GHC.Core.Multiplicity

import GHC.Builtin.WiredIn.Types
import GHC.Builtin.KnownKeys
import GHC.Builtin.Modules
import GHC.Builtin.WiredIn.Prim

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic

import GHC.Data.FastString
import GHC.Data.OrdList
import GHC.Data.Maybe ( expectJust )

import Data.List        ( partition )
import Data.List.NonEmpty ( NonEmpty (..) )
import Data.Char        ( ord )
import Data.Foldable    ( foldrM )

infixl 4 `mkCoreApp`, `mkCoreApps`

{-
************************************************************************
*                                                                      *
\subsection{Basic GHC.Core construction}
*                                                                      *
************************************************************************
-}
-- | Sort the variables, putting type and covars first, in scoped order,
-- and then other Ids
--
-- It is a deterministic sort, meaning it doesn't look at the values of
-- Uniques. For explanation why it's important See Note [Unique Determinism]
-- in GHC.Types.Unique.
sortQuantVars :: [Var] -> [Var]
sortQuantVars vs = sorted_tcvs ++ ids
  where
    (tcvs, ids) = partition (isTyVar <||> isCoVar) vs
    sorted_tcvs = scopedSort tcvs

-- | Bind a binding group over an expression, using a @let@ or @case@ as
-- appropriate (see "GHC.Core#let_can_float_invariant")
mkCoreLet :: HasDebugCallStack => CoreBind -> CoreExpr -> CoreExpr
mkCoreLet (NonRec bndr rhs) body        -- See Note [Core let-can-float invariant]
  = bindNonRec bndr rhs body
mkCoreLet bind body
  = Let bind body

-- | Create a lambda where the given expression has a number of variables
-- bound over it. The leftmost binder is that bound by the outermost
-- lambda in the result
mkCoreLams :: [CoreBndr] -> CoreExpr -> CoreExpr
mkCoreLams = mkLams

-- | Create a type lambda (/\a b c. e) and apply a cast to fix up visibilities
-- if needed. See Note [Required foralls in Core]
mkCoreTyLams :: [TyVarBinder] -> CoreExpr -> CoreExpr
mkCoreTyLams binders body = mkCast lam co
  where
    lam = mkCoreLams (binderVars binders) body
    co  = mkForAllVisCos binders (mkRepReflCo (exprType body))

-- | Bind a list of binding groups over an expression. The leftmost binding
-- group becomes the outermost group in the resulting expression
mkCoreLets :: HasDebugCallStack => [CoreBind] -> CoreExpr -> CoreExpr
mkCoreLets binds body = foldr mkCoreLet body binds

-- | Construct an expression which represents the application of a number of
-- expressions to that of a data constructor expression. The leftmost expression
-- in the list is applied first
mkCoreConApps :: DataCon -> [CoreExpr] -> CoreExpr
mkCoreConApps con args = mkCoreApps (Var (dataConWorkId con)) args

-- | A variant of 'mkCoreConApps' constructs an expression which represents the
-- application of a number of expressions to that of a data constructor
-- expression using the wrapper, not the worker, of the data constructor. The
-- leftmost expression in the list is applied first
mkCoreConWrapApps :: DataCon -> [CoreExpr] -> CoreExpr
mkCoreConWrapApps con args = mkCoreApps (Var (dataConWrapId con)) args

-- | Construct an expression which represents the application of a number of
-- expressions to another. The leftmost expression in the list is applied first
-- See Note [Assertion checking in mkCoreApp]
mkCoreApps :: CoreExpr   -- ^ function
           -> [CoreExpr] -- ^ arguments
           -> CoreExpr
mkCoreApps fun args = foldl' mkCoreApp fun args

-- | Construct an expression which represents the application of one expression
-- to the other
-- See Note [Assertion checking in mkCoreApp]
mkCoreApp :: CoreExpr -- ^ function
          -> CoreExpr -- ^ argument
          -> CoreExpr
mkCoreApp fun arg = App fun arg

{- Note [Assertion checking in mkCoreApp]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
At one time we had an assertion to check that the function and argument type match up,
but that turned out to take 90% of all compile time (!) when compiling test
`unboxedsums/UbxSumUnpackedSize.hs`. The reason was an unboxed sum constructor with
hundreds of foralls.   It's most straightforward just to remove the assert, and
rely on Lint to discover any mis-constructed terms.
-}

{- *********************************************************************
*                                                                      *
              Building case expressions
*                                                                      *
********************************************************************* -}

-- | Make a /wildcard binder/. This is typically used when you need a binder
-- that you expect to use only at a *binding* site.  Do not use it at
-- occurrence sites because it has a single, fixed unique, and it's very
-- easy to get into difficulties with shadowing.  That's why it is used so little.
--
-- See Note [WildCard binders] in "GHC.Core.Opt.Simplify.Env"
mkWildValBinder :: Mult -> Type -> Id
mkWildValBinder w ty = mkLocalIdOrCoVar wildCardName w ty
  -- "OrCoVar" since a coercion can be a scrutinee with -fdefer-type-errors
  -- (e.g. see test T15695). Ticket #17291 covers fixing this problem.

wildCardName :: Name
wildCardName = mkSystemVarName wildCardKey (fsLit "wild")

-- | Make a case expression whose case binder is unused
-- The alts and res_ty should not have any occurrences of WildId
mkWildCase :: CoreExpr -- ^ scrutinee
           -> Scaled Type
           -> Type -- ^ res_ty
           -> [CoreAlt] -- ^ alts
           -> CoreExpr
mkWildCase scrut (Scaled w scrut_ty) res_ty alts
  = Case scrut (mkWildValBinder w scrut_ty) res_ty alts

mkIfThenElse :: CoreExpr -- ^ guard
             -> CoreExpr -- ^ then
             -> CoreExpr -- ^ else
             -> CoreExpr
mkIfThenElse guard then_expr else_expr
-- Not going to be refining, so okay to take the type of the "then" clause
  = mkWildCase guard (linear boolTy) (exprType then_expr)
         [ Alt (DataAlt falseDataCon) [] else_expr,       -- Increasing order of tag!
           Alt (DataAlt trueDataCon)  [] then_expr ]

castBottomExpr :: CoreExpr -> Type -> CoreExpr
-- (castBottomExpr e ty), assuming that 'e' diverges,
-- return an expression of type 'ty'
-- See Note [Empty case alternatives] in GHC.Core
castBottomExpr e res_ty
  | e_ty `eqType` res_ty = e
  | otherwise            = Case e (mkWildValBinder OneTy e_ty) res_ty []
  where
    e_ty = exprType e

mkLitRubbish :: Type -> Maybe CoreExpr
-- Make a rubbish-literal CoreExpr of the given type.
-- Fail (returning Nothing) if
--    * the RuntimeRep of the Type is not monomorphic;
--    * the type is (a ~# b), the type of coercion
--    * the type is terminating (isTerminatingType), e.g. a dictionary
-- See INVARIANT 1, 2 and 3 of item (2) in Note [Rubbish literals]
-- in GHC.Types.Literal
mkLitRubbish ty
  | not (noFreeVarsOfType rep)
  = Nothing   -- Satisfy INVARIANT 1
  | isEqPred ty
  = Nothing   -- Satisfy INVARIANT 2
  | isTerminatingType ty
  = Nothing   -- Satisfy INVARIANT 3
  | otherwise
  = Just (Lit (LitRubbish torc rep) `mkTyApps` [ty])
  where
    (torc, rep) = expectJust $ sORTKind_maybe (typeKind ty)

{-
************************************************************************
*                                                                      *
\subsection{Making literals}
*                                                                      *
************************************************************************
-}

-- | Create a 'CoreExpr' which will evaluate to the given @Int@
mkIntExpr :: Platform -> Integer -> CoreExpr        -- Result = I# i :: Int
mkIntExpr platform i = mkCoreConApps intDataCon  [mkIntLit platform i]

-- | Create a 'CoreExpr' which will evaluate to the given @Int@. Don't check
-- that the number is in the range of the target platform @Int@
mkUncheckedIntExpr :: Integer -> CoreExpr        -- Result = I# i :: Int
mkUncheckedIntExpr i = mkCoreConApps intDataCon  [Lit (mkLitIntUnchecked i)]

-- | Create a 'CoreExpr' which will evaluate to the given @Int@
mkIntExprInt :: Platform -> Int -> CoreExpr         -- Result = I# i :: Int
mkIntExprInt platform i = mkCoreConApps intDataCon  [mkIntLit platform (fromIntegral i)]

-- | Create a 'CoreExpr' which will evaluate to a @Word@ with the given value
mkWordExpr :: Platform -> Integer -> CoreExpr
mkWordExpr platform w = mkCoreConApps wordDataCon [mkWordLit platform w]

-- | Create a 'CoreExpr' which will evaluate to the given @Integer@
mkIntegerExpr  :: Platform -> Integer -> CoreExpr  -- Result :: Integer
mkIntegerExpr platform i
  | platformInIntRange platform i = mkCoreConApps integerISDataCon [mkIntLit platform i]
  | i < 0                         = mkCoreConApps integerINDataCon [Lit (mkLitBigNat (negate i))]
  | otherwise                     = mkCoreConApps integerIPDataCon [Lit (mkLitBigNat i)]

-- | Create a 'CoreExpr' which will evaluate to the given @Natural@
mkNaturalExpr  :: Platform -> Integer -> CoreExpr
mkNaturalExpr platform w
  | platformInWordRange platform w = mkCoreConApps naturalNSDataCon [mkWordLit platform w]
  | otherwise                      = mkCoreConApps naturalNBDataCon [Lit (mkLitBigNat w)]

-- | Create a 'CoreExpr' which will evaluate to a
-- (lifted) @Float@ approximating the given @Rational@
mkFloatExpr  :: Rational -> CoreExpr
mkFloatExpr  r = mkCoreConApps floatDataCon  [mkFloatLit  r]

-- | Create a 'CoreExpr' which will evaluate to a
-- (lifted) @Double@ approximating the given @Rational@
mkDoubleExpr :: Rational -> CoreExpr
mkDoubleExpr r = mkCoreConApps doubleDataCon [mkDoubleLit r]


-- | Create a 'CoreExpr' which will evaluate to the given @Char@
mkCharExpr     :: Char             -> CoreExpr      -- Result = C# c :: Int
mkCharExpr c = mkCoreConApps charDataCon [mkCharLit c]

data MkStringIds = MkStringIds
  { unpackCStringId     :: !Id
  , unpackCStringUtf8Id :: !Id
  }

getMkStringIds :: Applicative m => (KnownKey -> m Id) -> m MkStringIds
getMkStringIds lookupM = MkStringIds <$> lookupM unpackCStringIdKey <*> lookupM unpackCStringUtf8IdKey

-- | Create a 'CoreExpr' which will evaluate to the given @String@
mkStringExprWith :: MkStringIds -> String -> CoreExpr  -- Result :: String
mkStringExprWith mks = mkStringExprFSWith mks . mkFastString

mkStringExprFSWith :: MkStringIds -> FastString -> CoreExpr
mkStringExprFSWith ids str
  | nullFS str
  = mkNilExpr charTy

  | all safeChar chars
  = let !unpack_id = unpackCStringId ids
    in App (Var unpack_id) lit

  | otherwise
  = let !unpack_utf8_id = unpackCStringUtf8Id ids
    in App (Var unpack_utf8_id) lit

  where
    chars = unpackFS str
    safeChar c = ord c >= 1 && ord c <= 0x7F
    lit = Lit (LitString (bytesFS str))

{-
************************************************************************
*                                                                      *
     Creating tuples and their types for Core expressions
*                                                                      *
************************************************************************
-}

-- | The unit expression
unitExpr :: CoreExpr
unitExpr = Var unitDataConId

{- Note [Flattening one-tuples]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This family of functions creates a tuple of variables/expressions/types.
  mkCoreTup [e1,e2,e3] = (e1,e2,e3)
What if there is just one variable/expression/type in the argument?
We could do one of two things:

* Flatten it out, so that
    mkCoreTup [e1] = e1

* Build a one-tuple (see Note [One-tuples] in GHC.Builtin.WiredIn.Types)
    mkCoreTupSolo [e1] = Solo e1
  We use a suffix "Solo" to indicate this.

Usually we want the former, but occasionally the latter.

NB: The logic in tupleDataCon knows about () and Solo and (,), etc.

Note [Don't flatten tuples from HsSyn]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If we get an explicit 1-tuple from HsSyn somehow (likely: Template Haskell),
we should treat it really as a 1-tuple, without flattening. Note that a
1-tuple and a flattened value have different performance and laziness
characteristics, so should just do what we're asked.

This arose from discussions in #16881.

One-tuples that arise internally depend on the circumstance; often flattening
is a good idea. Decisions are made on a case-by-case basis.

`mkCoreBoxedTuple` and `mkBigCoreVarTup BareElements` (in GHC.Core.Make.BigTuple)
build tuples without flattening.
-}

-- | Build a small tuple holding the specified expressions.
--
-- One-tuples are *not* flattened; see Note [Flattening one-tuples] as well
-- as Note [Don't flatten tuples from HsSyn].
--
-- Arguments must have kind @Type@.
mkCoreBoxedTuple :: HasDebugCallStack => [CoreExpr] -> CoreExpr
mkCoreBoxedTuple cs
  = assertPpr (all (tcIsLiftedTypeKind . typeKind . exprType) cs) (ppr cs)
    mkCoreConApps (tupleDataCon Boxed (length cs))
                  (map (Type . exprType) cs ++ cs)


-- | Build a small unboxed tuple holding the specified expressions.
-- Do not include the RuntimeRep specifiers; this function calculates them
-- for you.
-- Does /not/ flatten one-tuples; see Note [Flattening one-tuples]
mkCoreUnboxedTuple :: [CoreExpr] -> CoreExpr
mkCoreUnboxedTuple exps
  = mkCoreConApps (tupleDataCon Unboxed (length tys))
                  (map (Type . getRuntimeRep) tys ++ map Type tys ++ exps)
  where
    tys = map exprType exps

-- | Make a core tuple of the given boxity; don't flatten 1-tuples
mkCoreTupBoxity :: Boxity -> [CoreExpr] -> CoreExpr
mkCoreTupBoxity Boxed   exps = mkCoreBoxedTuple   exps
mkCoreTupBoxity Unboxed exps = mkCoreUnboxedTuple exps

-- | Build the type of a small tuple that holds the specified variables
-- One-tuples are flattened; see Note [Flattening one-tuples]
mkCoreVarTupTy :: [Id] -> Type
mkCoreVarTupTy ids = mkBoxedTupleTy (map idType ids)

-- | Build a small tuple holding the specified expressions
-- One-tuples are flattened; see Note [Flattening one-tuples]
mkCoreTup :: [CoreExpr] -> CoreExpr
mkCoreTup [c] = c
mkCoreTup cs  = mkCoreBoxedTuple cs   -- non-1-tuples are uniform

-- | Build an unboxed sum.
--
-- Alternative number ("alt") starts from 1.
mkCoreUnboxedSum :: Int -> Int -> [Type] -> CoreExpr -> CoreExpr
mkCoreUnboxedSum arity alt tys exp
  = assert (length tys == arity) $
    assert (alt <= arity) $
    mkCoreConApps (sumDataCon alt arity)
                  (map (Type . getRuntimeRep) tys
                   ++ map Type tys
                   ++ [exp])

-- | Pattern match on a tuple built by @'mkChunkified' 'mkCoreTup'@, binding the
-- given variables in the body. Strict in the entire chunked tuple:
--
-- > mkChunkedTupleCase [a,b,c,d] body e
-- >   = case e of v { (p,q) ->
-- >     case p of p { (a,b) ->
-- >     case q of q { (c,d) ->
-- >     body }}}
--
-- (pretending 'mAX_TUPLE_SIZE' is 2).
mkChunkedTupleCase
  :: MonadUnique m
  => [Id]       -- ^ The tuple identifiers to pattern match on;
                --   bring these into scope in the body
  -> CoreExpr   -- ^ Body of the case
  -> CoreExpr   -- ^ Scrutinee
  -> m CoreExpr
mkChunkedTupleCase all_vars all_body scrut
  = go (chunkify all_vars) all_body
  where
    -- go [[a1..an], [b1..bm], ...] body
    --    case scrut of (p,q, ...) ->
    --    case p of (a1,..an) ->
    --    case q of (b1,..bm) ->
    --    ... -> body
    go [vars] body
      = do { scrut_var <- case scrut of
                            Var v -> return v
                            _     -> mkSysLocalM (fsLit "ds") ManyTy (exprType scrut)
           ; return (mkSmallTupleCase vars body scrut_var scrut) }
    go vars_s body
      = do { (vars', body') <- foldrM one_tuple_case ([], body) vars_s
           ; go (chunkify vars') body' }

    one_tuple_case chunk_vars (vs, body)
      = do { scrut_var <- mkSysLocalM (fsLit "ds") ManyTy (mkCoreVarTupTy chunk_vars)
           ; return ( scrut_var:vs
                    , mkSmallTupleCase chunk_vars body scrut_var (Var scrut_var) ) }

-- | Pattern match on a tuple of arity at most 'mAX_TUPLE_SIZE', flattening
-- one-tuples.
mkSmallTupleCase
        :: [Id]         -- ^ The tuple args
        -> CoreExpr     -- ^ Body of the case
        -> Id           -- ^ A variable of the same type as the scrutinee
        -> CoreExpr     -- ^ Scrutinee
        -> CoreExpr

mkSmallTupleCase [var] body _scrut_var scrut
  = bindNonRec var scrut body
mkSmallTupleCase vars body scrut_var scrut
  = Case scrut scrut_var (exprType body)
         [Alt (DataAlt (tupleDataCon Boxed (length vars))) vars body]

{-
************************************************************************
*                                                                      *
\subsection{Common list manipulation expressions}
*                                                                      *
************************************************************************

Call the constructor Ids when building explicit lists, so that they
interact well with rules.
-}

-- | Makes a list @[]@ for lists of the specified type
mkNilExpr :: Type -> CoreExpr
mkNilExpr ty = mkCoreConApps nilDataCon [Type ty]

-- | Makes a list @(:)@ for lists of the specified type
mkConsExpr :: Type -> CoreExpr -> CoreExpr -> CoreExpr
mkConsExpr ty hd tl = mkCoreConApps consDataCon [Type ty, hd, tl]

-- | Make a list containing the given expressions, where the list has the given type
mkListExpr :: Type -> [CoreExpr] -> CoreExpr
mkListExpr ty xs = foldr (mkConsExpr ty) (mkNilExpr ty) xs

{-
************************************************************************
*                                                                      *
             Manipulating Maybe data type
*                                                                      *
************************************************************************
-}


-- | Makes a Nothing for the specified type
mkNothingExpr :: Type -> CoreExpr
mkNothingExpr ty = mkConApp nothingDataCon [Type ty]

-- | Makes a Just from a value of the specified type
mkJustExpr :: Type -> CoreExpr -> CoreExpr
mkJustExpr ty val = mkConApp justDataCon [Type ty, val]


{-
************************************************************************
*                                                                      *
             Manipulating Floats
*                                                                      *
************************************************************************
-}

wrapFloat :: FloatBind -> CoreExpr -> CoreExpr
wrapFloat (FloatTick t)          body = mkTick t body
wrapFloat (FloatLet defns)       body = Let defns body
wrapFloat (FloatCase e b con bs) body = mkSingleAltCase e b con bs body

-- | Applies the floats from right to left. That is @wrapFloats [b1, b2, …, bn]
-- u = let b1 in let b2 in … in let bn in u@
wrapFloats :: FloatBinds -> CoreExpr -> CoreExpr
wrapFloats floats expr = foldrOL wrapFloat expr floats


{-
************************************************************************
*                                                                      *
                      Error expressions
*                                                                      *
************************************************************************
-}

mkRuntimeErrorApp
        :: Id           -- Should be of type
                        --   forall (r::RuntimeRep) (a::TYPE r). Addr# -> a
                        --      or (a :: CONSTRAINT r)
                        --      where Addr# points to a UTF8 encoded string
        -> Type         -- The type to instantiate 'a'
        -> String       -- The string to print
        -> CoreExpr

mkRuntimeErrorApp err_id res_ty err_msg
  = mkApps (Var err_id) [ Type (getRuntimeRep res_ty)
                        , Type res_ty, err_string ]
  where
    err_string = Lit (mkLitString err_msg)

{-
************************************************************************
*                                                                      *
                     Error Ids
*                                                                      *
************************************************************************

GHC randomly injects these into the code.

@patError@ is just a version of @error@ for pattern-matching
failures.  It knows various ``codes'' which expand to longer
strings---this saves space!

@absentErr@ is a thing we put in for ``absent'' arguments.  They jolly
well shouldn't be yanked on, but if one is, then you will get a
friendly message from @absentErr@ (rather than a totally random
crash).
-}

errorIds :: [Id]
errorIds
  = [ nON_EXHAUSTIVE_GUARDS_ERROR_ID,
      nO_METHOD_BINDING_ERROR_ID,
      pAT_ERROR_ID,
      rEC_CON_ERROR_ID,
      rEC_SEL_ERROR_ID,
      iMPOSSIBLE_ERROR_ID, iMPOSSIBLE_CONSTRAINT_ERROR_ID,
      aBSENT_ERROR_ID,  aBSENT_CONSTRAINT_ERROR_ID,
      aBSENT_SUM_FIELD_ERROR_ID,
      tYPE_ERROR_ID   -- Used with Opt_DeferTypeErrors, see #10284
      ]

recSelErrorName, recConErrorName, patErrorName :: Name
nonExhaustiveGuardsErrorName, noMethodBindingErrorName :: Name
typeErrorName :: Name
absentSumFieldErrorName :: Name

recSelErrorName     = err_nm "recSelError"     recSelErrorIdKey     rEC_SEL_ERROR_ID
recConErrorName     = err_nm "recConError"     recConErrorIdKey     rEC_CON_ERROR_ID
patErrorName        = err_nm "patError"        patErrorIdKey        pAT_ERROR_ID
typeErrorName       = err_nm "typeError"       typeErrorIdKey       tYPE_ERROR_ID

noMethodBindingErrorName     = err_nm "noMethodBindingError"
                                  noMethodBindingErrorIdKey nO_METHOD_BINDING_ERROR_ID
nonExhaustiveGuardsErrorName = err_nm "nonExhaustiveGuardsError"
                                  nonExhaustiveGuardsErrorIdKey nON_EXHAUSTIVE_GUARDS_ERROR_ID

err_nm :: String -> Unique -> Id -> Name
err_nm str uniq id = mkWiredInIdName gHC_INTERNAL_CONTROL_EXCEPTION_BASE (fsLit str) uniq id

rEC_SEL_ERROR_ID, rEC_CON_ERROR_ID :: Id
pAT_ERROR_ID, nO_METHOD_BINDING_ERROR_ID, nON_EXHAUSTIVE_GUARDS_ERROR_ID :: Id
tYPE_ERROR_ID, aBSENT_SUM_FIELD_ERROR_ID :: Id
rEC_SEL_ERROR_ID                = mkRuntimeErrorId TypeLike recSelErrorName
rEC_CON_ERROR_ID                = mkRuntimeErrorId TypeLike recConErrorName
pAT_ERROR_ID                    = mkRuntimeErrorId TypeLike patErrorName
nO_METHOD_BINDING_ERROR_ID      = mkRuntimeErrorId TypeLike noMethodBindingErrorName
nON_EXHAUSTIVE_GUARDS_ERROR_ID  = mkRuntimeErrorId TypeLike nonExhaustiveGuardsErrorName
tYPE_ERROR_ID                   = mkRuntimeErrorId TypeLike typeErrorName

-- Note [aBSENT_SUM_FIELD_ERROR_ID]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Unboxed sums are transformed into unboxed tuples in GHC.Stg.Unarise.mkUbxSum
-- and fields that can't be reached are filled with rubbish values.
-- For instance, consider the case of the program:
--
--     f :: (# Int | Float# #) -> Int
--     f = ...
--
--     x = f (# | 2.0## #)
--
-- Unarise will represent f's unboxed sum argument as a tuple (# Int#, Int,
-- Float# #), where Int# is a tag. Consequently, `x` will be rewritten to:
--
--     x = f (# 2#, ???, 2.0## #)
--
-- We must come up with some rubbish literal to use in place of `???`. In the
-- case of unboxed integer types this is easy: we can simply use 0 for
-- Int#/Word# and 0.0 Float#/Double#.
--
-- However, coming up with a rubbish pointer value is more delicate as the
-- value must satisfy the following requirements:
--
--    1. it needs to be a valid closure pointer for the GC (not a NULL pointer)
--
--    2. it can't take arguments because it's used in unarise and applying an
--       argument would require allocating a thunk, which is both difficult to
--       do and costly.
--
--    3. it shouldn't be CAFfy since this would make otherwise non-CAFfy
--       bindings CAFfy, incurring a cost in GC performance. Given that unboxed
--       sums are intended to be used in performance-critical code, this is to
--       We work-around this by declaring the absentSumFieldError as non-CAFfy,
--       as described in Note [Wired-in exceptions are not CAFfy].
--
--       Getting this wrong causes hard-to-debug runtime issues, see #15038.
--
--    4. it can't be defined in `base` package.  Afterall, not all code which
--       uses unboxed sums uses depends upon `base`.  Specifically, this became
--       an issue when we wanted to use unboxed sums in boot libraries used by
--       `base`, see #17791.
--
-- To fill this role we define `ghc-prim:GHC.Prim.Panic.absentSumFieldError`
-- with the type:
--
--    absentSumFieldError :: forall a. a
--
-- Note that this type is something of a lie since Unarise may use it at an
-- unlifted type. However, this lie is benign as absent sum fields are examined
-- only by the GC, which does not care about levity..
--
-- When entered, this closure calls `stg_panic#`, which immediately halts
-- execution and cannot be caught. This is in contrast to most other runtime
-- errors, which are thrown as proper Haskell exceptions. This design is
-- intentional since entering an absent sum field is an indication that
-- something has gone horribly wrong, very likely due to a compiler bug.
--

-- Note [Wired-in exceptions are not CAFfy]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- GHC has logic wiring-in a small number of exceptions, which may be thrown in
-- generated code. Specifically, these are implemented via closures (defined
-- in `GHC.Prim.Exception` in `ghc-prim`) which, when entered, raise the desired
-- exception. For instance, in the case of OverflowError we have
--
--     raiseOverflow :: forall a. a
--     raiseOverflow = runRW# (\s ->
--         case raiseOverflow# s of
--           (# _, _ #) -> let x = x in x)
--
-- where `raiseOverflow#` is defined in the rts/Exception.cmm.
--
-- Note that `raiseOverflow` and friends, being top-level thunks, are CAFs.
-- Normally, this would be reflected in their IdInfo; however, as these
-- functions are widely used and CAFfyness is transitive, we very much want to
-- avoid declaring them as CAFfy. This is especially true in especially in
-- performance-critical code like that using unboxed sums and
-- absentSumFieldError.
--
-- Consequently, `mkExceptionId` instead declares the exceptions to be
-- non-CAFfy and rather ensure in the RTS (in `initBuiltinGcRoots` in
-- rts/RtsStartup.c) that these closures remain reachable by creating a
-- StablePtr to each. Note that we are using the StablePtr mechanism not
-- because we need a StablePtr# object, but rather because the stable pointer
-- table is a source of GC roots.
--
-- At some point we could consider removing this optimisation as it is quite
-- fragile, but we do want to be careful to avoid adding undue cost. Unboxed
-- sums in particular are intended to be used in performance-critical contexts.
--
-- See #15038, #21141.

absentSumFieldErrorName
   = mkWiredInIdName
      gHC_PRIM_PANIC
      (fsLit "absentSumFieldError")
      absentSumFieldErrorIdKey
      aBSENT_SUM_FIELD_ERROR_ID

aBSENT_SUM_FIELD_ERROR_ID = mkExceptionId absentSumFieldErrorName

-- | Exception with type \"forall a. a\"
--
-- Any exceptions added via this function needs to be added to
-- the RTS's initBuiltinGcRoots() function.
mkExceptionId :: Name -> Id
mkExceptionId name
  = mkVanillaGlobalWithInfo name
      (mkSpecForAllTys [alphaTyVar] (mkTyVarTy alphaTyVar)) -- forall a . a
      (divergingIdInfo [] `setCafInfo` NoCafRefs)
         -- See Note [Wired-in exceptions are not CAFfy]

-- | An 'IdInfo' for an Id, such as 'aBSENT_ERROR_ID', that
-- throws an (imprecise) exception after being supplied one value arg for every
-- argument 'Demand' in the list. The demands end up in the demand signature.
--
-- 1. Sets the demand signature to unleash the given arg dmds 'botDiv'
-- 2. Sets the arity info so that it matches the length of arg demands
-- 3. Sets a bottoming CPR sig with the correct arity
--
-- It's important that all 3 agree on the arity, which is what this defn ensures.
divergingIdInfo :: [Demand] -> IdInfo
divergingIdInfo arg_dmds
  = vanillaIdInfo `setArityInfo` arity
                  `setDmdSigInfo` mkClosedDmdSig arg_dmds botDiv
                  `setCprSigInfo` mkCprSig arity botCpr
  where
    arity = length arg_dmds

{- Note [Error and friends have an "open-tyvar" forall]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'error' and 'undefined' have types
        error     :: forall (v :: RuntimeRep) (a :: TYPE v). String -> a
        undefined :: forall (v :: RuntimeRep) (a :: TYPE v). a
Notice the runtime-representation polymorphism. This ensures that
"error" can be instantiated at unboxed as well as boxed types.
This is OK because it never returns, so the return type is irrelevant.


************************************************************************
*                                                                      *
                     iMPOSSIBLE_ERROR_ID
*                                                                      *
************************************************************************
-}

iMPOSSIBLE_ERROR_ID, iMPOSSIBLE_CONSTRAINT_ERROR_ID :: Id
iMPOSSIBLE_ERROR_ID            = mkRuntimeErrorId TypeLike       impossibleErrorName
iMPOSSIBLE_CONSTRAINT_ERROR_ID = mkRuntimeErrorId ConstraintLike impossibleConstraintErrorName

impossibleErrorName, impossibleConstraintErrorName :: Name
impossibleErrorName           = err_nm "impossibleError"
                                impossibleErrorIdKey iMPOSSIBLE_ERROR_ID
impossibleConstraintErrorName = err_nm "impossibleConstraintError"
                                impossibleConstraintErrorIdKey iMPOSSIBLE_CONSTRAINT_ERROR_ID

mkImpossibleExpr :: Type -> String -> CoreExpr
mkImpossibleExpr res_ty str
  = mkRuntimeErrorApp err_id res_ty str
  where    -- See Note [Type vs Constraint for error ids]
    err_id = case typeTypeOrConstraint res_ty of
               TypeLike       -> iMPOSSIBLE_ERROR_ID
               ConstraintLike -> iMPOSSIBLE_CONSTRAINT_ERROR_ID

{- Note [Type vs Constraint for error ids]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need both
  iMPOSSIBLE_ERROR_ID            :: forall (r::RuntimeRep) (a::TYPE r).       Addr# -> a
  iMPOSSIBLE_CONSTRAINT_ERROR_ID :: forall (r::RuntimeRep) (a::CONSTRAINT r). Addr# -> a

because we don't have polymorphism over TYPE vs CONSTRAINT.  You
might wonder if iMPOSSIBLE_CONSTRAINT_ERROR_ID is ever needed in
practice, but it is: see #22634.  So:

* In Control.Exception.Base we have
      impossibleError           :: forall (a::Type). Addr# -> a
      impossibleConstraintError :: forall (a::Type). Addr# -> a
  This generates the code for `impossibleError`, but because they are wired in
  the interface file definitions are never looked at (indeed, they don't
  even get serialised).

* In this module GHC.Core.Make we define /wired-in/ Ids for
      iMPOSSIBLE_ERROR_ID
      iMPOSSIBLE_CONSTRAINT_ERROR_ID
   with the desired above types (i.e. runtime-rep polymorphic, and returning a
   constraint for the latter.

Much the same plan works for aBSENT_ERROR_ID and aBSENT_CONSTRAINT_ERROR_ID


************************************************************************
*                                                                      *
                     aBSENT_ERROR_ID
*                                                                      *
************************************************************************

Note [aBSENT_ERROR_ID]
~~~~~~~~~~~~~~~~~~~~~~
We use aBSENT_ERROR_ID to build absent fillers for lifted types in workers. E.g.

   f x = (case x of (a,b) -> b) + 1::Int

The demand analyser figures out that only the second component of x is
used, and does a w/w split thus

   f x = case x of (a,b) -> $wf b

   $wf b = let a = absentError "blah"
               x = (a,b)
           in <the original RHS of f>

After some simplification, the (absentError "blah") thunk normally goes away.
See also Note [Absent fillers] in GHC.Core.Opt.WorkWrap.Utils.

Historical Note
---------------
We used to have exprIsHNF respond True to absentError and *not* mark it as diverging.
Here's the reason for the former. It doesn't apply anymore because we no longer say
that `a` is absent (A). Instead it gets (head strict) demand 1A and we won't
emit the absent error:

#14285 had, roughly

   data T a = MkT a !a
   {-# INLINABLE f #-}
   f x = case x of MkT a b -> g (MkT b a)

It turned out that g didn't use the second component, and hence f doesn't use
the first.  But the stable-unfolding for f looks like
   \x. case x of MkT a b -> g ($WMkT b a)
where $WMkT is the wrapper for MkT that evaluates its arguments.  We
apply the same w/w split to this unfolding (see Note [Worker/wrapper
for INLINABLE functions] in GHC.Core.Opt.WorkWrap) so the template ends up like
   \b. let a = absentError "blah"
           x = MkT a b
        in case x of MkT a b -> g ($WMkT b a)

After doing case-of-known-constructor, and expanding $WMkT we get
   \b -> g (case absentError "blah" of a -> MkT b a)

Yikes!  That bogusly appears to evaluate the absentError!

This is extremely tiresome.  Another way to think of this is that, in
Core, it is an invariant that a strict data constructor, like MkT, must
be applied only to an argument in HNF. So (absentError "blah") had
better be non-bottom.

So the "solution" is to add a special case for absentError to exprIsHNFlike.
This allows Simplify.rebuildCase, in the Note [Case to let transformation]
branch, to convert the case on absentError into a let. We also make
absentError *not* be diverging, unlike the other error-ids, so that we
can be sure not to remove the case branches before converting the case to
a let.

If, by some bug or bizarre happenstance, we ever call absentError, we should
throw an exception.  This should never happen, of course, but we definitely
can't return anything.  e.g. if somehow we had
    case absentError "foo" of
       Nothing -> ...
       Just x  -> ...
then if we return, the case expression will select a field and continue.
Seg fault city. Better to throw an exception. (Even though we've said
it is in HNF :-)

It might seem a bit surprising that seq on absentError is simply erased

    absentError "foo" `seq` x ==> x

but that should be okay; since there's no pattern match we can't really
be relying on anything from it.
-}

-- We need two absentError Ids:
--   absentError           :: forall (a :: Type).       Addr# -> a
--   absentConstraintError :: forall (a :: Constraint). Addr# -> a
-- We don't have polymorphism over TypeOrConstraint!
-- mkAbsentErrorApp chooses which one to use, based on the kind
-- See Note [Type vs Constraint for error ids]

mkAbsentErrorApp :: Type         -- The type to instantiate 'a'
                 -> String       -- The string to print
                 -> CoreExpr

mkAbsentErrorApp res_ty err_msg
  = mkApps (Var err_id) [ Type res_ty, err_string ]
  where
    err_id = case typeTypeOrConstraint res_ty of
               TypeLike       -> aBSENT_ERROR_ID
               ConstraintLike -> aBSENT_CONSTRAINT_ERROR_ID
    err_string = Lit (mkLitString err_msg)

absentErrorName, absentConstraintErrorName :: Name
absentErrorName
   = mkWiredInIdName gHC_PRIM_PANIC (fsLit "absentError")
      absentErrorIdKey aBSENT_ERROR_ID

absentConstraintErrorName   -- See Note [Type vs Constraint for error ids]
   = mkWiredInIdName gHC_PRIM_PANIC (fsLit "absentConstraintError")
      absentConstraintErrorIdKey aBSENT_CONSTRAINT_ERROR_ID

aBSENT_ERROR_ID, aBSENT_CONSTRAINT_ERROR_ID :: Id

aBSENT_ERROR_ID -- See Note [aBSENT_ERROR_ID]
 = mk_runtime_error_id absentErrorName absent_ty
 where
   -- absentError :: forall (a :: Type). Addr# -> a
   absent_ty = mkSpecForAllTys [alphaTyVar] $
               mkVisFunTyMany addrPrimTy (mkTyVarTy alphaTyVar)
   -- Not runtime-rep polymorphic. aBSENT_ERROR_ID is only used for
   -- lifted-type things; see Note [Absent fillers] in GHC.Core.Opt.WorkWrap.Utils

aBSENT_CONSTRAINT_ERROR_ID -- See Note [aBSENT_ERROR_ID]
 = mk_runtime_error_id absentConstraintErrorName absent_ty
   -- See Note [Type vs Constraint for error ids]
 where
   -- absentConstraintError :: forall (a :: Constraint). Addr# -> a
   absent_ty = mkSpecForAllTys [alphaConstraintTyVar] $
               mkFunTy visArgConstraintLike ManyTy
                       addrPrimTy (mkTyVarTy alphaConstraintTyVar)


{-
************************************************************************
*                                                                      *
                     mkRuntimeErrorId
*                                                                      *
************************************************************************
-}

mkRuntimeErrorId :: TypeOrConstraint -> Name -> Id
-- Error function
--   with type:  forall (r::RuntimeRep) (a::TYPE r). Addr# -> a
--   with arity: 1
-- which diverges after being given one argument
-- The Addr# is expected to be the address of
--   a UTF8-encoded error string
mkRuntimeErrorId torc name = mk_runtime_error_id name (mkRuntimeErrorTy torc)


mk_runtime_error_id :: Name -> Type -> Id
mk_runtime_error_id name ty
 = mkVanillaGlobalWithInfo name ty (divergingIdInfo [evalDmd])
     -- Do *not* mark them as NoCafRefs, because they can indeed have
     -- CAF refs.  For example, pAT_ERROR_ID calls GHC.Err.untangle,
     -- which has some CAFs
     -- In due course we may arrange that these error-y things are
     -- regarded by the GC as permanently live, in which case we
     -- can give them NoCaf info.  As it is, any function that calls
     -- any pc_bottoming_Id will itself have CafRefs, which bloats
     -- SRTs.

mkRuntimeErrorTy :: TypeOrConstraint -> Type
-- forall (rr :: RuntimeRep) (a :: rr). Addr# -> a
--   See Note [Error and friends have an "open-tyvar" forall]
mkRuntimeErrorTy torc = mkSpecForAllTys [runtimeRep1TyVar, tyvar] $
                        mkFunctionType ManyTy addrPrimTy (mkTyVarTy tyvar)
  where
    tyvar:|_ = expectNonEmpty $ mkTemplateTyVars [kind]
    kind = case torc of
              TypeLike       -> mkTYPEapp       runtimeRep1Ty
              ConstraintLike -> mkCONSTRAINTapp runtimeRep1Ty
