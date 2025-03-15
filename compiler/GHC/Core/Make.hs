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
        mkCharExpr, mkStringExpr, mkStringExprFS, mkStringExprFSWith,
        MkStringIds (..), getMkStringIds,

        -- * Floats
        FloatBind(..), wrapFloat, wrapFloats, floatBindings,

        -- * Constructing small tuples
        mkCoreVarTupTy, mkCoreTup, mkCoreUnboxedTuple, mkCoreUnboxedSum,
        mkCoreTupBoxity, unitExpr,

        -- * Constructing big tuples
        mkChunkified, chunkify,
        mkBigCoreVarTup, mkBigCoreVarTupSolo,
        mkBigCoreVarTupTy, mkBigCoreTupTy,
        mkBigCoreTup,

          -- * Deconstructing big tuples
        mkBigTupleSelector, mkBigTupleSelectorSolo, mkBigTupleCase,

        -- * Constructing list expressions
        mkNilExpr, mkConsExpr, mkListExpr,
        mkFoldrExpr, mkBuildExpr,

        -- * Constructing Maybe expressions
        mkNothingExpr, mkJustExpr,

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
import GHC.Types.Var  ( setTyVarUnique, visArgConstraintLike )
import GHC.Types.TyThing
import GHC.Types.Id.Info
import GHC.Types.Cpr
import GHC.Types.Basic( TypeOrConstraint(..) )
import GHC.Types.Demand
import GHC.Types.Name      hiding ( varName )
import GHC.Types.Literal
import GHC.Types.Unique.Supply

import GHC.Core
import GHC.Core.Utils ( exprType, mkSingleAltCase, bindNonRec, mkCast )
import GHC.Core.Type
import GHC.Core.Predicate    ( scopedSort, isCoVarType )
import GHC.Core.TyCo.Compare ( eqType )
import GHC.Core.Coercion     ( isCoVar, mkRepReflCo, mkForAllVisCos )
import GHC.Core.DataCon      ( DataCon, dataConWorkId, dataConWrapId )
import GHC.Core.Multiplicity

import GHC.Builtin.Types
import GHC.Builtin.Names
import GHC.Builtin.Types.Prim

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic

import GHC.Settings.Constants( mAX_TUPLE_SIZE )
import GHC.Data.FastString
import GHC.Data.Maybe ( expectJust )

import Data.List        ( partition )
import Data.List.NonEmpty ( NonEmpty (..) )
import Data.Char        ( ord )

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
mkCoreLet :: CoreBind -> CoreExpr -> CoreExpr
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
mkCoreLets :: [CoreBind] -> CoreExpr -> CoreExpr
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
mkCoreApps :: CoreExpr -- ^ function
           -> [CoreExpr] -- ^ arguments
           -> CoreExpr
mkCoreApps fun args
  = fst $
    foldl' (mkCoreAppTyped doc_string) (fun, fun_ty) args
  where
    doc_string = ppr fun_ty $$ ppr fun $$ ppr args
    fun_ty = exprType fun

-- | Construct an expression which represents the application of one expression
-- to the other
mkCoreApp :: SDoc
          -> CoreExpr -- ^ function
          -> CoreExpr -- ^ argument
          -> CoreExpr
mkCoreApp s fun arg
  = fst $ mkCoreAppTyped s (fun, exprType fun) arg

-- | Construct an expression which represents the application of one expression
-- paired with its type to an argument. The result is paired with its type. This
-- function is not exported and used in the definition of 'mkCoreApp' and
-- 'mkCoreApps'.
mkCoreAppTyped :: SDoc -> (CoreExpr, Type) -> CoreExpr -> (CoreExpr, Type)
mkCoreAppTyped _ (fun, fun_ty) (Type ty)
  = (App fun (Type ty), piResultTy fun_ty ty)
mkCoreAppTyped _ (fun, fun_ty) (Coercion co)
  = (App fun (Coercion co), funResultTy fun_ty)
mkCoreAppTyped d (fun, fun_ty) arg
  = assertPpr (isFunTy fun_ty) (ppr fun $$ ppr arg $$ d)
    (App fun arg, funResultTy fun_ty)

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
-- See INVARIANT 1 and 2 of item (2) in Note [Rubbish literals]
-- in GHC.Types.Literal
mkLitRubbish ty
  | not (noFreeVarsOfType rep)
  = Nothing   -- Satisfy INVARIANT 1
  | isCoVarType ty
  = Nothing   -- Satisfy INVARIANT 2
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

-- | Create a 'CoreExpr' which will evaluate to the given @Float@
mkFloatExpr :: Float -> CoreExpr
mkFloatExpr f = mkCoreConApps floatDataCon [mkFloatLitFloat f]

-- | Create a 'CoreExpr' which will evaluate to the given @Double@
mkDoubleExpr :: Double -> CoreExpr
mkDoubleExpr d = mkCoreConApps doubleDataCon [mkDoubleLitDouble d]


-- | Create a 'CoreExpr' which will evaluate to the given @Char@
mkCharExpr     :: Char             -> CoreExpr      -- Result = C# c :: Int
mkCharExpr c = mkCoreConApps charDataCon [mkCharLit c]

-- | Create a 'CoreExpr' which will evaluate to the given @String@
mkStringExpr   :: MonadThings m => String     -> m CoreExpr  -- Result :: String
mkStringExpr str = mkStringExprFS (mkFastString str)

-- | Create a 'CoreExpr' which will evaluate to a string morally equivalent to the given @FastString@
mkStringExprFS :: MonadThings m => FastString -> m CoreExpr  -- Result :: String
mkStringExprFS = mkStringExprFSLookup lookupId

mkStringExprFSLookup :: Monad m => (Name -> m Id) -> FastString -> m CoreExpr
mkStringExprFSLookup lookupM str = do
  mk <- getMkStringIds lookupM
  pure (mkStringExprFSWith mk str)

getMkStringIds :: Applicative m => (Name -> m Id) -> m MkStringIds
getMkStringIds lookupM = MkStringIds <$> lookupM unpackCStringName <*> lookupM unpackCStringUtf8Name

data MkStringIds = MkStringIds
  { unpackCStringId     :: !Id
  , unpackCStringUtf8Id :: !Id
  }

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

{- Note [Flattening one-tuples]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This family of functions creates a tuple of variables/expressions/types.
  mkCoreTup [e1,e2,e3] = (e1,e2,e3)
What if there is just one variable/expression/type in the argument?
We could do one of two things:

* Flatten it out, so that
    mkCoreTup [e1] = e1

* Build a one-tuple (see Note [One-tuples] in GHC.Builtin.Types)
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

'mkCoreBoxedTuple` and `mkBigCoreVarTupSolo` build tuples without flattening.
-}

-- | Build a small tuple holding the specified expressions
-- One-tuples are *not* flattened; see Note [Flattening one-tuples]
-- See also Note [Don't flatten tuples from HsSyn]
-- Arguments must have kind Type
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

{- Note [Big tuples]
~~~~~~~~~~~~~~~~~~~~
"Big" tuples (`mkBigCoreTup` and friends) are more general than "small"
ones (`mkCoreTup` and friends) in two ways.

1. GHCs built-in tuples can only go up to 'mAX_TUPLE_SIZE' in arity, but
   we might conceivably want to build such a massive tuple as part of the
   output of a desugaring stage (notably that for list comprehensions).

   `mkBigCoreTup` encodes such big tuples by creating and pattern
   matching on /nested/ small tuples that are directly expressible by
   GHC.

   Nesting policy: it's better to have a 2-tuple of 10-tuples (3 objects)
   than a 10-tuple of 2-tuples (11 objects), so we want the leaves of any
   construction to be big.

2. When desugaring arrows we gather up a tuple of free variables, which
   may include dictionaries (of kind Constraint) and unboxed values.

   These can't live in a tuple. `mkBigCoreTup` encodes such tuples by
   boxing up the offending arguments: see Note [Boxing constructors]
   in GHC.Builtin.Types.

If you just use the 'mkBigCoreTup', 'mkBigCoreVarTupTy', 'mkBigTupleSelector'
and 'mkBigTupleCase' functions to do all your work with tuples you should be
fine, and not have to worry about the arity limitation, or kind limitation at
all.

The "big" tuple operations flatten 1-tuples just like "small" tuples.
But see Note [Don't flatten tuples from HsSyn]
-}

mkBigCoreVarTupSolo :: [Id] -> CoreExpr
-- Same as mkBigCoreVarTup, but:
--   - one-tuples are not flattened
--     see Note [Flattening one-tuples]
--   - arguments should have kind Type
mkBigCoreVarTupSolo [id] = mkCoreBoxedTuple [Var id]
mkBigCoreVarTupSolo ids  = mkChunkified mkCoreTup (map Var ids)

-- | Build a big tuple holding the specified variables
-- One-tuples are flattened; see Note [Flattening one-tuples]
-- Arguments don't have to have kind Type
mkBigCoreVarTup :: [Id] -> CoreExpr
mkBigCoreVarTup ids = mkBigCoreTup (map Var ids)

-- | Build a "big" tuple holding the specified expressions
-- One-tuples are flattened; see Note [Flattening one-tuples]
-- Arguments don't have to have kind Type; ones that do not are boxed
-- This function crashes (in wrapBox) if given a non-Type
-- argument that it doesn't know how to box.
mkBigCoreTup :: [CoreExpr] -> CoreExpr
mkBigCoreTup exprs = mkChunkified mkCoreTup (map wrapBox exprs)

-- | Build the type of a big tuple that holds the specified variables
-- One-tuples are flattened; see Note [Flattening one-tuples]
mkBigCoreVarTupTy :: HasDebugCallStack => [Id] -> Type
mkBigCoreVarTupTy ids = mkBigCoreTupTy (map idType ids)

-- | Build the type of a big tuple that holds the specified type of thing
-- One-tuples are flattened; see Note [Flattening one-tuples]
mkBigCoreTupTy :: HasDebugCallStack => [Type] -> Type
mkBigCoreTupTy tys = mkChunkified mkBoxedTupleTy $
                     map boxTy tys

-- | The unit expression
unitExpr :: CoreExpr
unitExpr = Var unitDataConId

--------------------------------------------------------------
wrapBox :: CoreExpr -> CoreExpr
-- ^ If (e :: ty) and (ty :: Type), wrapBox is a no-op
-- But if (ty :: ki), and ki is not Type, wrapBox returns (K @ty e)
--     which has kind Type
-- where K is the boxing data constructor for ki
-- See Note [Boxing constructors] in GHC.Builtin.Types
-- Panics if there /is/ no boxing data con
wrapBox e
  = case boxingDataCon e_ty of
      BI_NoBoxNeeded                       -> e
      BI_Box { bi_inst_con = boxing_expr } -> App boxing_expr e
      BI_NoBoxAvailable -> pprPanic "wrapBox" (ppr e $$ ppr (exprType e))
                           -- We should do better than panicing: #22336
  where
    e_ty = exprType e

boxTy :: HasDebugCallStack => Type -> Type
-- ^ `boxTy ty` is the boxed version of `ty`. That is,
-- if `e :: ty`, then `wrapBox e :: boxTy ty`.
-- Note that if `ty :: Type`, `boxTy ty` just returns `ty`.
-- Panics if it is not possible to box `ty`, like `wrapBox` (#22336)
-- See Note [Boxing constructors] in GHC.Builtin.Types
boxTy ty
  = case boxingDataCon ty of
      BI_NoBoxNeeded -> ty
      BI_Box { bi_boxed_type = box_ty } -> box_ty
      BI_NoBoxAvailable -> pprPanic "boxTy" (ppr ty)
                           -- We should do better than panicing: #22336

unwrapBox :: UniqSupply -> Id -> CoreExpr
                 -> (UniqSupply, Id, CoreExpr)
-- If v's type required boxing (i.e it is unlifted or a constraint)
-- then (unwrapBox us v body) returns
--          (case box_v of MkDict v -> body)
--          together with box_v
--      where box_v is a fresh variable
-- Otherwise unwrapBox is a no-op
-- Panics if no box is available (#22336)
unwrapBox us var body
  = case boxingDataCon var_ty of
      BI_NoBoxNeeded    -> (us, var, body)
      BI_NoBoxAvailable -> pprPanic "unwrapBox" (ppr var $$ ppr var_ty)
                           -- We should do better than panicing: #22336
      BI_Box { bi_data_con = box_con, bi_boxed_type = box_ty }
         -> (us', var', body')
         where
           var'  = mkSysLocal (fsLit "uc") uniq ManyTy box_ty
           body' = Case (Var var') var' (exprType body)
                        [Alt (DataAlt box_con) [var] body]
  where
    var_ty      = idType var
    (uniq, us') = takeUniqFromSupply us

-- | Lifts a \"small\" constructor into a \"big\" constructor by recursive decomposition
mkChunkified :: ([a] -> a)      -- ^ \"Small\" constructor function, of maximum input arity 'mAX_TUPLE_SIZE'
             -> [a]             -- ^ Possible \"big\" list of things to construct from
             -> a               -- ^ Constructed thing made possible by recursive decomposition
mkChunkified small_tuple as = mk_big_tuple (chunkify as)
  where
        -- Each sub-list is short enough to fit in a tuple
    mk_big_tuple [as] = small_tuple as
    mk_big_tuple as_s = mk_big_tuple (chunkify (map small_tuple as_s))

chunkify :: [a] -> [[a]]
-- ^ Split a list into lists that are small enough to have a corresponding
-- tuple arity. The sub-lists of the result all have length <= 'mAX_TUPLE_SIZE'
-- But there may be more than 'mAX_TUPLE_SIZE' sub-lists
chunkify xs
  | n_xs <= mAX_TUPLE_SIZE = [xs]
  | otherwise              = split xs
  where
    n_xs     = length xs
    split [] = []
    split xs = let (as, bs) = splitAt mAX_TUPLE_SIZE xs
               in as : split bs


{-
************************************************************************
*                                                                      *
\subsection{Tuple destructors}
*                                                                      *
************************************************************************
-}

-- | Builds a selector which scrutinises the given
-- expression and extracts the one name from the list given.
-- If you want the no-shadowing rule to apply, the caller
-- is responsible for making sure that none of these names
-- are in scope.
--
-- If there is just one 'Id' in the tuple, then the selector is
-- just the identity.
--
-- If necessary, we pattern match on a \"big\" tuple.
--
-- A tuple selector is not linear in its argument. Consequently, the case
-- expression built by `mkBigTupleSelector` must consume its scrutinee 'Many'
-- times. And all the argument variables must have multiplicity 'Many'.
mkBigTupleSelector, mkBigTupleSelectorSolo
    :: [Id]         -- ^ The 'Id's to pattern match the tuple against
    -> Id           -- ^ The 'Id' to select
    -> Id           -- ^ A variable of the same type as the scrutinee
    -> CoreExpr     -- ^ Scrutinee
    -> CoreExpr     -- ^ Selector expression

-- mkBigTupleSelector [a,b,c,d] b v e
--          = case e of v {
--                (p,q) -> case p of p {
--                           (a,b) -> b }}
-- We use 'tpl' vars for the p,q, since shadowing does not matter.
--
-- In fact, it's more convenient to generate it innermost first, getting
--
--        case (case e of v
--                (p,q) -> p) of p
--          (a,b) -> b
mkBigTupleSelector vars the_var scrut_var scrut
  = mk_tup_sel (chunkify vars) the_var
  where
    mk_tup_sel [vars] the_var = mkSmallTupleSelector vars the_var scrut_var scrut
    mk_tup_sel vars_s the_var = mkSmallTupleSelector group the_var tpl_v $
                                mk_tup_sel (chunkify tpl_vs) tpl_v
        where
          tpl_tys = [mkBoxedTupleTy (map idType gp) | gp <- vars_s]
          tpl_vs  = mkTemplateLocals tpl_tys
          (tpl_v, group) = case
            [ (tpl,gp)
            | (tpl,gp) <- zipEqual tpl_vs vars_s
            , the_var `elem` gp
            ] of
              [x] -> x
              _ -> panic "mkBigTupleSelector"
-- ^ 'mkBigTupleSelectorSolo' is like 'mkBigTupleSelector'
-- but one-tuples are NOT flattened (see Note [Flattening one-tuples])
mkBigTupleSelectorSolo vars the_var scrut_var scrut
  | [_] <- vars
  = mkSmallTupleSelector1 vars the_var scrut_var scrut
  | otherwise
  = mkBigTupleSelector vars the_var scrut_var scrut

-- | `mkSmallTupleSelector` is like 'mkBigTupleSelector', but for tuples that
-- are guaranteed never to be "big".  Also does not unwrap boxed types.
--
-- > mkSmallTupleSelector [x] x v e = [| e |]
-- > mkSmallTupleSelector [x,y,z] x v e = [| case e of v { (x,y,z) -> x } |]
mkSmallTupleSelector, mkSmallTupleSelector1
          :: [Id]        -- The tuple args
          -> Id          -- The selected one
          -> Id          -- A variable of the same type as the scrutinee
          -> CoreExpr    -- Scrutinee
          -> CoreExpr
mkSmallTupleSelector [var] should_be_the_same_var _ scrut
  = assert (var == should_be_the_same_var) $
    scrut  -- Special case for 1-tuples
mkSmallTupleSelector vars the_var scrut_var scrut
  = mkSmallTupleSelector1 vars the_var scrut_var scrut

-- ^ 'mkSmallTupleSelector1' is like 'mkSmallTupleSelector'
-- but one-tuples are NOT flattened (see Note [Flattening one-tuples])
mkSmallTupleSelector1 vars the_var scrut_var scrut
  = assert (notNull vars) $
    Case scrut scrut_var (idType the_var)
         [Alt (DataAlt (tupleDataCon Boxed (length vars))) vars (Var the_var)]

-- | A generalization of 'mkBigTupleSelector', allowing the body
-- of the case to be an arbitrary expression.
--
-- To avoid shadowing, we use uniques to invent new variables.
--
-- If necessary we pattern match on a "big" tuple.
mkBigTupleCase :: MonadUnique m    --   For inventing names of intermediate variables
               => [Id]             -- ^ The tuple identifiers to pattern match on;
                                   --   Bring these into scope in the body
               -> CoreExpr         -- ^ Body of the case
               -> CoreExpr         -- ^ Scrutinee
               -> m CoreExpr
-- ToDo: eliminate cases where none of the variables are needed.
--
--         mkBigTupleCase uniqs [a,b,c,d] body v e
--           = case e of v { (p,q) ->
--             case p of p { (a,b) ->
--             case q of q { (c,d) ->
--             body }}}
mkBigTupleCase vars body scrut
  = do us <- getUniqueSupplyM
       let (wrapped_us, wrapped_vars, wrapped_body) = foldr unwrap (us,[],body) vars
       return $ mk_tuple_case wrapped_us (chunkify wrapped_vars) wrapped_body
  where
    scrut_ty = exprType scrut

    unwrap var (us,vars,body)
      = (us', var':vars, body')
      where
        (us', var', body') = unwrapBox us var body

    mk_tuple_case :: UniqSupply -> [[Id]] -> CoreExpr -> CoreExpr
    -- mk_tuple_case [[a1..an], [b1..bm], ...] body
    --    case scrut of (p,q, ...) ->
    --    case p of (a1,..an) ->
    --    case q of (b1,..bm) ->
    --    ... -> body
    -- This is the case where don't need any nesting
    mk_tuple_case us [vars] body
      = mkSmallTupleCase vars body scrut_var scrut
      where
        scrut_var = case scrut of
                       Var v -> v
                       _ -> snd (new_var us scrut_ty)

    -- This is the case where we must nest tuples at least once
    mk_tuple_case us vars_s body
      = mk_tuple_case us' (chunkify vars') body'
      where
        (us', vars', body') = foldr one_tuple_case (us, [], body) vars_s

    one_tuple_case chunk_vars (us, vs, body)
      = (us', scrut_var:vs, body')
      where
        tup_ty           = mkBoxedTupleTy (map idType chunk_vars)
        (us', scrut_var) = new_var us tup_ty
        body' = mkSmallTupleCase chunk_vars body scrut_var (Var scrut_var)

    new_var :: UniqSupply -> Type -> (UniqSupply, Id)
    new_var us ty = (us', id)
       where
         (uniq, us') = takeUniqFromSupply us
         id = mkSysLocal (fsLit "ds") uniq ManyTy ty

-- | As 'mkBigTupleCase', but for a tuple that is small enough to be guaranteed
-- not to need nesting.
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
                Floats
*                                                                      *
************************************************************************
-}

data FloatBind
  = FloatLet  CoreBind
  | FloatCase CoreExpr Id AltCon [Var]
      -- case e of y { C ys -> ... }
      -- See Note [Floating single-alternative cases] in GHC.Core.Opt.SetLevels

instance Outputable FloatBind where
  ppr (FloatLet b) = text "LET" <+> ppr b
  ppr (FloatCase e b c bs) = hang (text "CASE" <+> ppr e <+> text "of" <+> ppr b)
                                2 (ppr c <+> ppr bs)

wrapFloat :: FloatBind -> CoreExpr -> CoreExpr
wrapFloat (FloatLet defns)       body = Let defns body
wrapFloat (FloatCase e b con bs) body = mkSingleAltCase e b con bs body

-- | Applies the floats from right to left. That is @wrapFloats [b1, b2, …, bn]
-- u = let b1 in let b2 in … in let bn in u@
wrapFloats :: [FloatBind] -> CoreExpr -> CoreExpr
wrapFloats floats expr = foldr wrapFloat expr floats

bindBindings :: CoreBind -> [Var]
bindBindings (NonRec b _) = [b]
bindBindings (Rec bnds) = map fst bnds

floatBindings :: FloatBind -> [Var]
floatBindings (FloatLet bnd) = bindBindings bnd
floatBindings (FloatCase _ b _ bs) = b:bs

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

-- | Make a fully applied 'foldr' expression
mkFoldrExpr :: MonadThings m
            => Type             -- ^ Element type of the list
            -> Type             -- ^ Fold result type
            -> CoreExpr         -- ^ "Cons" function expression for the fold
            -> CoreExpr         -- ^ "Nil" expression for the fold
            -> CoreExpr         -- ^ List expression being folded acress
            -> m CoreExpr
mkFoldrExpr elt_ty result_ty c n list = do
    foldr_id <- lookupId foldrName
    return (Var foldr_id `App` Type elt_ty
           `App` Type result_ty
           `App` c
           `App` n
           `App` list)

-- | Make a 'build' expression applied to a locally-bound worker function
mkBuildExpr :: (MonadFail m, MonadThings m, MonadUnique m)
            => Type                                     -- ^ Type of list elements to be built
            -> ((Id, Type) -> (Id, Type) -> m CoreExpr) -- ^ Function that, given information about the 'Id's
                                                        -- of the binders for the build worker function, returns
                                                        -- the body of that worker
            -> m CoreExpr
mkBuildExpr elt_ty mk_build_inside = do
    n_tyvar <- newTyVar alphaTyVar
    let n_ty = mkTyVarTy n_tyvar
        c_ty = mkVisFunTysMany [elt_ty, n_ty] n_ty
    [c, n] <- sequence [mkSysLocalM (fsLit "c") ManyTy c_ty, mkSysLocalM (fsLit "n") ManyTy n_ty]

    build_inside <- mk_build_inside (c, c_ty) (n, n_ty)

    build_id <- lookupId buildName
    return $ Var build_id `App` Type elt_ty `App` mkLams [n_tyvar, c, n] build_inside
  where
    newTyVar tyvar_tmpl = do
      uniq <- getUniqueM
      return (setTyVarUnique tyvar_tmpl uniq)

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

