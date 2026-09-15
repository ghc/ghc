-- | Building and deconstructing \"big\" tuples in Core, which may exceed the
-- maximum tuple arity and may hold unlifted values and constraints.
--
-- See Note [Big tuples].
module GHC.Core.Make.BigTuple (
        -- * Constructing big tuples
        BigTupleLayout(..),
        mkBigCoreVarTup,
        mkBigCoreVarTupTy, mkBigCoreTupTy,
        mkBigCoreTup,

        -- * Deconstructing big tuples
        mkBigTupleSelector, mkBigTupleCase,
    ) where

import GHC.Prelude

import GHC.Types.Id
import GHC.Types.Basic ( Boxity(..) )
import GHC.Types.Unique.Supply

import GHC.Core
import GHC.Core.Make     ( mkCoreTup, mkCoreBoxedTuple, mkChunkedTupleCase )
import GHC.Core.Make.Box ( boxTy, wrapBox, unwrapBox, mkUnbox )
import GHC.Core.Utils    ( exprType )
import GHC.Core.Type

import GHC.Builtin.WiredIn.Types

import GHC.Utils.Outputable
import GHC.Utils.Misc
import GHC.Utils.Panic

import GHC.Data.FastString

import Data.Foldable ( foldrM )


{- Note [Big tuples]
~~~~~~~~~~~~~~~~~~~~
"Big" tuples (`mkBigCoreTup` and friends) are more general than "small"
ones (`mkCoreTup` and friends) in two ways.

1. GHCs built-in tuples can only go up to 'mAX_TUPLE_SIZE' in arity, but
   we might conceivably want to build such a massive tuple as part of the
   output of a desugaring stage (notably that for list comprehensions).

   `mkBigCoreTup` encodes such big tuples by creating and pattern
   matching on /nested/ small tuples that are directly expressible by
   GHC, as per 'mkChunkified'.

2. When desugaring arrows we gather up a tuple of free variables, which
   may include dictionaries (of kind Constraint) and unboxed values.

   These can't live in a tuple. With the 'BoxedElements' layout, `mkBigCoreTup`
   encodes such tuples by boxing up every element: see Note [Boxing constructors]
   in GHC.Builtin.WiredIn.Types.Box and Note [Boxing big tuple elements] in
   GHC.HsToCore.Utils.

If you just use the 'mkBigCoreTup', 'mkBigCoreVarTupTy', 'mkBigTupleSelector'
and 'mkBigTupleCase' functions to do all your work with tuples you should be
fine, and not have to worry about the arity limitation, or kind limitation at
all.

The "big" tuple operations flatten 1-tuples just like "small" tuples.
But see Note [Don't flatten tuples from HsSyn] in GHC.Core.Make.

  Relationship with 'Box'

    The 'BoxedElements' big tuple layout boxes the components of the tuple,
    using the boxing machinery of GHC.Core.Make.Box.

    Moreover, a big tuple has the same representation as a boxed unboxed tuple,
    i.e.

      mkBigCoreTupTy [t1, ..., tn] ~R# Box (# t1, ..., tn #)

    This suggests that the "big tuple" machinery is just a special case of the
    boxing machinery, but it isn't quite:

      - Big tuples may contain dictionaries, which can't be components of an
        unboxed tuple.
      - 'mkBigTupleSelector' only unboxes the component it selects, whereas
        'unbox' unboxes all components.
      - The 'BareElements' layout doesn't box its elements at all.
-}

-- | The layout of a "big tuple"; see Note [Big tuples].
--
-- Construction and deconstruction of a big tuple must use the same layout.
--
-- See Note [Boxing big tuple elements] in GHC.HsToCore.Utils.
data BigTupleLayout
  -- | All tuple elements are individually boxed, which allows unlifted elements.
  --
  -- All one-tuples are flattened, as per Note [Flattening one-tuples] in GHC.Core.Make.
  = BoxedElements
  -- | All elements are kept as they are (which requires them to be lifted).
  --
  -- Preserves all one-tuples (as 'Solo').
  | BareElements

-- | Build a big tuple holding the specified variables.
mkBigCoreVarTup :: MonadUnique m => BigTupleLayout -> [Id] -> m CoreExpr
mkBigCoreVarTup BoxedElements ids =
  -- With the 'BoxedElements' layout each element is boxed. We need the
  -- 'MonadUnique' to desugar this boxing in-place.
  -- See Note [Desugaring box & unbox] in GHC.Core.Make.Box.
  mkBigCoreTup (map Var ids)
mkBigCoreVarTup BareElements [id] = return $ mkCoreBoxedTuple [Var id]  -- preserve 1-tuples
mkBigCoreVarTup BareElements ids  = return $ mkChunkified mkCoreTup (map Var ids)

-- | Build a "big tuple" holding the specified expressions.
--
-- Uses the 'BoxedElements' big tuple layout: each element is boxed.
-- See Note [Desugaring box & unbox] in GHC.Core.Make.Box.
mkBigCoreTup :: MonadUnique m => [CoreExpr] -> m CoreExpr
mkBigCoreTup exprs
  -- See Note [Laziness in big-tuple machinery]
  = do { us <- getUniqueSupplyM
       ; return $ initUs_ us $
           do { boxed <- mapM (\ e -> wrapBox (exprType e) e) exprs
              ; return (mkChunkified mkCoreTup boxed) } }

{- Note [Laziness in big-tuple machinery]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'mkBigCoreTup' and 'mkBigTupleCase' must both be lazy in the list of
components/binders they deal with, as some of their consumers (such as the
desugaring of arrows in 'GHC.HsToCore.Arrows') rely on this for knot-tying.

To ensure these functions are lazy, we return a lazy computation by getting the
UniqSupply first and then returning a pure thunk, instead of using the ambient
monad to sequence all operations (as this monad may be strict). This ensures
that the result is a thunk that forces the components only when the built Core
is itself demanded.
-}

-- | Build the type of a "big tuple" holding the specified variables.
--
-- Uses the 'BoxedElements' big tuple layout.
mkBigCoreVarTupTy :: [Id] -> Type
mkBigCoreVarTupTy ids = mkBigCoreTupTy (map idType ids)

-- | Build the type of a "big tuple" that holds elements of the specified types.
--
-- Uses the 'BoxedElements' big tuple layout.
mkBigCoreTupTy :: [Type] -> Type
mkBigCoreTupTy tys = mkChunkified mkBoxedTupleTy $
                     map boxTy tys

{-
************************************************************************
*                                                                      *
\subsection{Tuple destructors}
*                                                                      *
************************************************************************
-}

-- | Builds a selector which scrutinises the given expression and extracts the
-- one named element from the list. Does not force the other elements.
-- See also 'mkBigTupleCase'.
--
-- If you want the no-shadowing rule to apply, the caller is responsible for
-- making sure that none of these names are in scope.
--
-- A tuple selector is not linear in its argument. Consequently, the case
-- expression built by 'mkBigTupleSelector' must consume its scrutinee 'Many'
-- times. And all the argument variables must have multiplicity 'Many'.
mkBigTupleSelector
  :: MonadUnique m
  => BigTupleLayout
  -> [Id]         -- ^ The 'Id's to pattern match the tuple against
  -> Id           -- ^ The 'Id' to select
  -> Id           -- ^ A variable of the same type as the scrutinee
  -> CoreExpr     -- ^ Scrutinee
  -> m CoreExpr   -- ^ Selector expression
mkBigTupleSelector BareElements vars the_var scrut_var scrut
  | [_] <- vars = return (mkSmallTupleSelector1 vars the_var scrut_var scrut) -- preserve Solo
  | otherwise   = return (mkChunkedTupleSelector vars the_var scrut_var scrut)
mkBigTupleSelector BoxedElements vars the_var scrut_var scrut
  -- Make a fresh boxed binder for each element, select the wanted one, and
  -- unbox it. The other elements are bound lazily as tuple components, so they
  -- are not forced.
  = do { boxed_vars <- mapM (\ v -> mkSysLocalM (fsLit "bsel") ManyTy (boxTy (idType v))) vars
       ; let boxed_the_var =
               case [ bv | (v, bv) <- zipEqual vars boxed_vars, v == the_var ] of
                 bv : _ -> bv
                 []     -> pprPanic "mkBigTupleSelector" (ppr the_var $$ ppr vars)
       ; mkUnbox (getRuntimeRep (idType the_var)) (idType the_var)
              (mkChunkedTupleSelector boxed_vars boxed_the_var scrut_var scrut) }

-- | Select one element from a "big" (chunked) tuple, with no (un)boxing and
-- flattening one-tuples. The chunking matches 'mkChunkified'.
--
-- > mkChunkedTupleSelector [a,b,c,d] b v e
-- >    = case e of v { (p,q) -> case p of p { (a,b) -> b }}
--
-- We use 'tpl' vars for the p,q, since shadowing does not matter. In fact, it's
-- more convenient to generate it innermost first, getting:
--
-- > case (case e of v { (p,q) -> p }) of p { (a,b) -> b }
mkChunkedTupleSelector :: [Id] -> Id -> Id -> CoreExpr -> CoreExpr
mkChunkedTupleSelector vars the_var scrut_var scrut
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
              _ -> panic "mkChunkedTupleSelector"

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
-- but one-tuples are NOT flattened (see Note [Flattening one-tuples] in GHC.Core.Make)
mkSmallTupleSelector1 vars the_var scrut_var scrut
  = assert (notNull vars) $
    Case scrut scrut_var (idType the_var)
         [Alt (DataAlt (tupleDataCon Boxed (length vars))) vars (Var the_var)]

-- | A variant on 'mkBigTupleSelector' which:
--
--  - always uses the 'BoxedElements' big tuple layout,
--  - allows the body of the case to be an arbitrary expression,
--  - is strict in the entire big tuple.
mkBigTupleCase :: MonadUnique m    --   For inventing names of intermediate variables
               => [Id]             -- ^ The tuple identifiers to pattern match on;
                                   --   Bring these into scope in the body
               -> CoreExpr         -- ^ Body of the case
               -> CoreExpr         -- ^ Scrutinee
               -> m CoreExpr
-- ToDo: eliminate cases where none of the variables are needed.
mkBigTupleCase vars body scrut
  -- NB: lazy in 'vars'/'body'; see Note [Laziness in big-tuple machinery]
  = do { us <- getUniqueSupplyM
       ; return $ initUs_ us $
           do { (wrapped_vars, wrapped_body) <- foldrM unwrap ([], body) vars
              ; mkChunkedTupleCase wrapped_vars wrapped_body scrut } }
  where
    body_ty = exprType body

    -- Make a fresh boxed binder for each tuple component and unbox it in the
    -- body. See Note [Desugaring box & unbox] in GHC.Core.Make.Box.
    unwrap var (vars, body)
      = do { (var', body') <- unwrapBox var body_ty body
           ; return (var':vars, body') }
