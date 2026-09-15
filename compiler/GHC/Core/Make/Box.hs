-- | Building Core that boxes values of any representation into lifted values.
--
-- See Note [Desugaring box & unbox].
module GHC.Core.Make.Box (
        -- * Boxing unlifted values and constraints
        boxTy, wrapBox, unwrapBox,
        mkBox, mkUnbox, mkCanonicalCo,
        boxDictTy, liftConstraint, unliftConstraintExpr,
    ) where

import GHC.Prelude

import GHC.Types.Id
import GHC.Types.Var ( visArgConstraintLike )
import GHC.Types.Basic
  ( Boxity(..), TypeOrConstraint(..), UnboxedTupleOrSum(..) )
import GHC.Types.Unique.Supply

import GHC.Core
import GHC.Core.Make
  ( mkWildCase, mkWildValBinder, mkImpossibleExpr
  , mkCoreTup, mkCoreUnboxedTuple, mkCoreUnboxedSum
  , mkChunkedTupleCase )
import GHC.Core.Utils ( mkCast, mkCastMCo )
import GHC.Core.Type
import GHC.Core.TyCon ( newTyConCo )
import GHC.Core.TyCo.Rep ( UnivCoProvenance(CanonicalProv) )
import GHC.Core.Coercion
  ( Coercion, CoercionN, Role(..)
  , MCoercion(..), MCoercionR, mkSymMCo
  , isReflCo, mkUnivCo, mkSymCo, mkUnbranchedAxInstCo
  , mkAxiomCo, mkNomReflCo, mkSubCo, mkTyConAppCo )

import GHC.Builtin.WiredIn.Types
import GHC.Builtin.WiredIn.Types.Box

import GHC.Data.FastString

import GHC.Utils.Misc ( HasDebugCallStack )

import Control.Monad ( zipWithM )


{-**********************************************************************
*                                                                      *
                    Boxing and unboxing
*                                                                      *
**********************************************************************-}

{- Note [Desugaring box & unbox]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The wired-in Ids 'box' and 'unbox' (see GHC.Builtin.WiredIn.Ids) allow
boxing/unboxing arbitrary types:

  box   :: forall (r :: RuntimeRep) (a :: TYPE r). a -> Box @r a
  unbox :: forall (r :: RuntimeRep) (a :: TYPE r). Box @r a -> a

These are representation polymorphic, so they must be inlined at every occurrence.
Unusually however, their unfoldings depend on the representation, so we can't
provide a single compulsory unfolding (like we do for 'coerce' for example).
Instead, we inline a custom unfolding generated from the type, in
'mkBox'/'mkUnbox'. This relies on the fact that the RuntimeRep
is always concrete (and not e.g. a skolem type variable): like all other
representation-polymorphic 'Id's, the typechecker guarantees that 'box'/'unbox'
are always instantiated at a concrete RuntimeRep, as explained in
Note [Representation-polymorphism checking built-ins] in GHC.Tc.Utils.Concrete.
After zonking, we can thus desugar any occurrence of 'box @r'/'unbox @r' by
dispatching on the concrete RuntimeRep 'r'.

The lack of unfolding for 'box'/unbox' means we must guarantee that they do not
occur in Core (nor in any later pipeline). Core Lint checks this (GHC.Core.Lint.lintIdOcc).
So we make sure to desugar occurrences of 'box'/'unbox' rather eagerly:

  - Occurrences of 'box'/'unbox' coming from typechecked source are desugared
    in the 'ds_app_var' case of GHC.HsToCore.Expr.

  - The big-tuple machinery of Note [Big tuples] in GHC.Core.Make.BigTuple,
    which is used by the desugarer, boxes/unboxes its components directly via
    'mkBox'/'mkUnbox' as it builds each tuple.

  - GHC.Core.Opt.SetLevels boxes unlifted values as it floats them out.
    It only needs some lifted type to wrap the value in, not the user-facing
    'Box' type, so it applies the boxing data constructor directly.
    See Note [Floating MFEs of unlifted type] in GHC.Core.Opt.SetLevels.

Recall the setup described in Note [Boxing constructors] in
GHC.Builtin.WiredIn.Types.Box. Defined in GHC.Internal.Box, we have:

  - A fixed family of /boxing data constructors/:

      data BoxInt  = BoxInt  Int#
      data BoxWord = BoxWord Word#
      data BoxAddr = BoxAddr Addr#
      ...

  - A type family:

      type family BoxTF r where
        BoxTF IntRep  = BoxInt
        BoxTF WordRep = BoxWord
        BoxTF AddrRep = BoxAddr
        ...

  - The user-facing 'Box' type, as a wrapper around 'BoxTF'

      newtype Box (a :: TYPE r) = MkBox (BoxTF r)
      type role Box representational

The desugarer then does the following:

  box @IntRep @Int# (e :: Int#)
    ==>
      BoxInt e |> ( sym tf_co ; sym nt_co )

  unbox @IntRep @Int# (e :: Box Int#)
    ==>
      case ( e |> nt_co ; tf_co ) of
        BoxInt x -> x

    where
      nt_co :: Box   Int#   ~R# BoxTF IntRep   -- 'Box' newtype coercion axiom
      tf_co :: BoxTF IntRep ~#  BoxInt         -- 'BoxTF' type family axiom

This handles simple representations such as 'Int# :: TYPE IntRep'. However,
for 'TupleRep'/'SumRep', things are a bit more complex. The subtlety is that:

  - We box unboxed sums and unboxed tuples by recursively boxing their components.
  - We might need to box a type such as (F Bool :: TYPE (TupleRep [IntRep, FloatRep])),
    where 'F' is a type family or an unlifted newtype.
    That is, the type may not be an unboxed tuple/unboxed sum, even though its
    RuntimeRep is a TupleRep/SumRep.

The implementation of 'box'/'unbox', which allows boxing
(e :: F Bool :: TYPE (TupleRep [IntRep, FloatRep])), thus proceeds by first
casting to the canonical type, also removing the outer 'MkBox' newtype layer:

  box @r @(ty :: TYPE r) (e :: ty)
    ==> (mkBox)
      boxCanon @r (e |> can_co) |> sym nt_co
        :: Box ty

  unbox @r @(ty :: TYPE r) (e :: Box ty)
    ==> (mkUnbox)
      unboxCanon @r (e |> nt_co) |> sym can_co
        :: ty

For representations that have an associated boxing data constructor (which is
every representation except TupleRep/SumRep which are boxed recursively, also
excepting BoxedRep which doesn't need boxing in the first place), 'boxCanon'
and 'unboxCanon' straightforwardly use that data constructor:

  boxCanon @r (e :: canonicalTypeOfRep r)
    ==> (mkBox_canon)
      K e |> sym tf_co
        :: BoxTF r

  unboxCanon @r (e :: BoxTF r)
    ==> (mkUnbox_canon)
      case e |> tf_co of
        K x ->
          x :: canonicalTypeOfRep r

    where
      K :: canonicalTypeOfRep r -> T   -- the boxing DataCon and TyCon at representation 'r',
                                       -- e.g. BoxInt :: Int# -> BoxInt
      nt_co  :: Box @r ty ~R# BoxTF r
      tf_co  :: BoxTF r ~# T
      can_co :: ty ~R# canonicalTypeOfRep r

For unboxed tuples and unboxed sums, 'boxCanon'/'unboxCanon' operate purely on
canonical types, and so when dealing with TupleRep/SumRep they operate on
genuine unboxed tuples/unboxed sums. Writing 't_i' for 'canonicalTypeOfRep r_i':

  boxCanon @(TupleRep [r1, r2]) (e :: (# t1, t2 #))
    ==> (mkBoxTuple_canon)
      ( case e of (# x1, x2 #) -> ( boxCanon @r1 x1, boxCanon @r2 x2 ) )
        |> sym tf_co
          :: BoxTF (TupleRep [r1, r2])

  unboxCanon @(TupleRep [r1, r2]) (e :: BoxTF (TupleRep [r1, r2]))
    ==> (mkUnboxTuple_canon)
      case e |> tf_co of
        (b1, b2) ->
          (# unboxCanon @r1 b1, unboxCanon @r2 b2 #)

    where
      tf_co :: BoxTF (TupleRep [r1, r2]) ~# (BoxTF r1, BoxTF r2)

  boxCanon @(SumRep [r1, r2]) (e :: (# t1 | t2 #))
    ==> (mkBoxSum_canon)
      ( case e of
          (# x1 | #) -> MkBoxSum0 (boxCanon @r1 x1 |> sym co_0)
          (# | x2 #) -> MkBoxSum1 (boxCanon @r2 x2 |> sym co_1) )
        |> sym tf_co
        :: BoxTF (SumRep [r1, r2])

  unboxCanon @(SumRep [r1, r2]) (e :: BoxTF (SumRep [r1, r2]))
    ==> (mkUnboxSum_canon)
      case e |> tf_co of
        MkBoxSum0 b1 -> (# unboxCanon @r1 (b1 |> co_0) | #)
        MkBoxSum1 b2 -> (# | unboxCanon @r2 (b2 |> co_1) #)
        DEFAULT      -> impossible

    where
      tf_co :: BoxTF (SumRep [r1, r2]) ~# BoxSum [r1, r2]
      co_k  :: BoxTF ([r1, r2] !! k) ~# BoxTF r_k  -- type family coercion axiom for (!!)

Note that the components are recursively boxed with 'boxCanon' (not 'box'),
so no 'MkBox' newtype coercion appears below the top level. The key point
here is that we deal with the 'MkBox' newtype wrapping and with
canonicalisation once at the very top, and then everything recurs using
only canonical types (because canonicalising e.g. (# F Int, (# G Bool, H Char #) #)
canonicalises it all the way down).

Note [The canonical type of a RuntimeRep]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For every 'r :: RuntimeRep', we define the canonical type 'canon r :: TYPE r'.
Given any type 'ty :: TYPE r', we define a coercion 'ty ~R# canon r', using
the 'CanonicalProv' 'UnivCoProvenance'.

This is implemented in 'canonicalTypeOfRep':

  -- Can be thought of as a wired-in type family (but we don't expose it)
  -- type Canonical :: forall (r :: RuntimeRep) -> TYPE r

  canonicalTypeOfRep :: RuntimeRepType -> Type
  canonicalTypeOfRep IntRep                     = Int#
  canonicalTypeOfRep WordRep                    = Word#
  canonicalTypeOfRep FloatRep                   = Float#
  canonicalTypeOfRep (VecRep Vec16 Int8ElemRep) = Int8X16#
  canonicalTypeOfRep (BoxedRep Lifted)          = Any @Type
  canonicalTypeOfRep (BoxedRep Unlifted)        = Any @UnliftedType
  canonicalTypeOfRep (TupleRep '[r1, ..., rn])
    = (# canonicalTypeOfRep r1, ..., canonicalTypeOfRep rn #)
  canonicalTypeOfRep (SumRep   '[r1, ..., rn])
    = (# canonicalTypeOfRep r1 | ... | canonicalTypeOfRep rn #)

The entire reason this concept exists is to handle the boxing of unboxed
tuples and unboxed sums, as described in Note [Desugaring box & unbox].
-}

-- | `boxTy ty` is a boxed version of the type `ty`.
--
-- See Note [Boxing constructors] in GHC.Builtin.WiredIn.Types.Box
boxTy :: HasDebugCallStack => Type -> Type
boxTy ty =
  case typeTypeOrConstraint ty of
    TypeLike ->
      mkTyConApp boxTyCon [kindRep $ typeKind ty, ty]
    ConstraintLike ->
      boxDictTy ty

-- | @wrapBox ty e@ boxes @e :: ty@ into a value of kind @Type@.
--
-- See Note [Boxing constructors] in GHC.Builtin.WiredIn.Types.Box.
wrapBox :: MonadUnique m => Type -> CoreExpr -> m CoreExpr
wrapBox ty e =
  case typeTypeOrConstraint ty of
    TypeLike ->
      mkBox (getRuntimeRep ty) ty e
    ConstraintLike ->
      -- -=> trick; see [Boxing constraints] in GHC.Builtin.WiredIn.Types.Box
      let ev = Lam (mkWildValBinder ManyTy unboxedUnitTy) e
      in return $ mkConApp mkDictBoxDataCon [Type (liftConstraint ty), ev]

-- | @unwrapBox var body_ty body@ builds the fresh boxed binder @var'@
-- and the expression @let var = unbox var' in body@ (using a @case@ when @var@
-- is unlifted), producing fully desugared Core (never an @unbox@ application).
-- It returns @var'@ so that the caller can bind it (e.g. as a tuple component).
--
-- See Note [Desugaring box & unbox].
unwrapBox :: MonadUnique m
          => Id          -- ^ @var@: binder to recover from the box
          -> Type        -- ^ type of @body@
          -> CoreExpr    -- ^ @body@
          -> m (Id, CoreExpr)
unwrapBox var body_ty body =
  case typeTypeOrConstraint var_ty of
    TypeLike ->
      do { var'    <- mkSysLocalM (fsLit "uc") ManyTy box_ty
           -- let-or-case var = unbox var' in body
         ; unboxed <- mkUnbox (getRuntimeRep var_ty) var_ty (Var var')
         ; return (var', let_or_case unboxed) }
    ConstraintLike ->
      do { var'  <- mkSysLocalM (fsLit "uc") ManyTy box_ty
         ; d_var <- mkSysLocalM (fsLit "uc") ManyTy (liftConstraint var_ty)
           -- case var' of MkDictBox d -> let-or-case var = d (# #) in body
         ; return ( var'
                  , Case (Var var') var' body_ty
                       [Alt (DataAlt mkDictBoxDataCon) [d_var]
                            (let_or_case (unliftConstraintExpr (Var d_var)))] ) }
  where
    var_ty = idType var
    box_ty = boxTy var_ty

    -- Bind @var = rhs@ in @body@, using @let@ or @case@ as appropriate
    -- depending on the levity of @var@.
    let_or_case rhs
      | mightBeUnliftedType var_ty
      = Case rhs var body_ty [Alt DEFAULT [] body]
      | otherwise
      = Let (NonRec var rhs) body

-- | @unwrapBoxCo rep ty :: Box \@rep ty ~R# BoxTF rep@: unwrap the 'Box' newtype.
unwrapBoxCo :: RuntimeRepType -> Type -> Coercion
unwrapBoxCo rep ty =
  mkUnbranchedAxInstCo Representational (newTyConCo boxTyCon) [rep, ty] []

-- | @boxTFCo rep :: BoxTF rep ~# boxTF (repBoxingInfo rep)@: reduce the
-- 'BoxTF' type family at a concrete representation.
boxTFCo :: RuntimeRepType -> CoercionN
boxTFCo rep = mkAxiomCo boxCoAxiomRule [mkNomReflCo rep]

-- | Box an expression of an arbitrary type.
--
-- @mkBox :: ty -> Box ty@
--
-- See Note [Desugaring box & unbox].
mkBox :: MonadUnique m => RuntimeRepType -> Type -> CoreExpr -> m CoreExpr
mkBox rep ty e =
  do { boxed <- mkBox_canon rep (mkCastMCo e (mkCanonicalCo ty))
     ; return $ mkCast boxed (mkSymCo (unwrapBoxCo rep ty)) }

-- | Unbox an expression created by 'mkBox'.
--
-- @mkUnbox :: Box ty -> Box ty@
--
-- See Note [Desugaring box & unbox].
mkUnbox :: MonadUnique m => RuntimeRepType -> Type -> CoreExpr -> m CoreExpr
mkUnbox rep ty e =
  do { unboxed <- mkUnbox_canon rep (mkCast e (unwrapBoxCo rep ty))
     ; return $ mkCastMCo unboxed (mkSymMCo (mkCanonicalCo ty)) }

-- | Box an expression whose type is canonical.
--
-- @mkBox_canon :: canonicalTypeOfRep rep -> BoxTF rep@
--
-- See Note [Desugaring box & unbox].
mkBox_canon :: MonadUnique m => RuntimeRepType -> CoreExpr -> m CoreExpr
mkBox_canon rep e =
  do { reduct <-
         case repBoxingInfo rep of
           BoxLifted                          -> return e
           BoxWithDataCon box_dc _            -> return $ mkConApp box_dc [e]
           BoxComponents UnboxedTupleType rs  -> mkBoxTuple_canon rs e
           BoxComponents UnboxedSumType   rs  -> mkBoxSum_canon   rs e
     ; return $ mkCast reduct (mkSymCo (mkSubCo (boxTFCo rep))) }

-- | Unbox an expression created by 'mkBox_canon'.
--
-- @mkUnbox_canon :: BoxTF rep -> canonicalTypeOfRep rep@
--
-- See Note [Desugaring box & unbox].
mkUnbox_canon :: MonadUnique m => RuntimeRepType -> CoreExpr -> m CoreExpr
mkUnbox_canon rep e =
  case boxing_info of
    BoxLifted ->
      return reduct
    BoxWithDataCon box_dc canon ->
      do { x <- mkSysLocalM (fsLit "ub") ManyTy canon
         ; return $ mkWildCase reduct (unrestricted (boxTF boxing_info)) canon
                      [Alt (DataAlt box_dc) [x] (Var x)] }
    BoxComponents UnboxedTupleType rs -> mkUnboxTuple_canon rs reduct
    BoxComponents UnboxedSumType   rs -> mkUnboxSum_canon   rs reduct
  where
    boxing_info = repBoxingInfo rep
    reduct = mkCast e (mkSubCo (boxTFCo rep))

-- | The implementation of 'mkBox_canon' for unboxed tuples:
--
-- > mkBoxTuple_canon e = case e of (# x_1, .., x_n #) -> (mkBox_canon x_1, .., mkBox_canon x_n)
--
-- where the boxed tuple is chunked as in 'boxTupleTy'.
--
-- See Note [Desugaring box & unbox].
mkBoxTuple_canon :: MonadUnique m => [RuntimeRepType] -> CoreExpr -> m CoreExpr
mkBoxTuple_canon elt_reps e =
  do { elt_bndrs <- mapM (mkSysLocalM (fsLit "bx") ManyTy . canonicalTypeOfRep) elt_reps
     ; boxes <- zipWithM (\ r x -> mkBox_canon r (Var x)) elt_reps elt_bndrs
     ; return $
         mkWildCase e (unrestricted (mkTupleTy1 Unboxed (map canonicalTypeOfRep elt_reps)))
                    (boxTupleTy elt_reps)
           [Alt (DataAlt (tupleDataCon Unboxed (length elt_reps))) elt_bndrs
                (mkChunkified mkCoreTup boxes)] }

-- | The implementation of 'mkUnbox_canon' for unboxed tuples:
--
-- > mkUnboxTuple_canon e = case e of (b_1, .., b_n) -> (# mkUnbox_canon b_1, .., mkUnbox_canon b_n #)
--
-- where the boxed tuple is chunked as in 'boxTupleTy'.
--
-- See Note [Desugaring box & unbox].
mkUnboxTuple_canon :: MonadUnique m => [RuntimeRepType] -> CoreExpr -> m CoreExpr
mkUnboxTuple_canon elt_reps e =
  do { box_bndrs <- mapM (mkSysLocalM (fsLit "ub") ManyTy . boxTFTy) elt_reps
     ; us <- zipWithM (\ r b -> mkUnbox_canon r (Var b)) elt_reps box_bndrs
     ; mkChunkedTupleCase box_bndrs (mkCoreUnboxedTuple us) e }

-- | The implementation of 'mkBox_canon' for unboxed sums:
--
-- > mkBoxSum_canon e = case e of { (# x | #) -> MkBoxSum0 (mkBox_canon x) ; (# | y #) -> MkBoxSum1 (mkBox_canon y) }
--
-- See Note [Desugaring box & unbox].
mkBoxSum_canon :: MonadUnique m => [RuntimeRepType] -> CoreExpr -> m CoreExpr
mkBoxSum_canon elt_reps e =
  do { alts <- zipWithM mk_alt [0 ..] elt_reps
     ; return $ mkWildCase e (unrestricted (mkSumTy elt_tys)) (boxSumTy elt_reps) alts }
  where
    elt_tys = map canonicalTypeOfRep elt_reps
    rs_ty   = mkPromotedListTy runtimeRepTy elt_reps

    mk_alt k comp_rep =
      do { x   <- mkSysLocalM (fsLit "bx") ManyTy (canonicalTypeOfRep comp_rep)
         ; box <- mkBox_canon comp_rep (Var x)
         ; let -- box |> (BoxTF r_k ~R# BoxTF (rs !! k))
               comp = mkCast box (mkSymCo (mkSubCo (boxTFIndexCo rs_ty k)))
         ; return $ Alt (DataAlt (sumDataCon (k + 1) (length elt_reps))) [x]
                        (mkConApp (boxSumDataCon k) [Type rs_ty, comp]) }

-- | The implementation of 'mkUnbox_canon' for unboxed sums:
--
-- > mkUnboxSum_canon e = case e of { MkBoxSum0 x -> (# mkUnbox_canon x | #) ; MkBoxSum1 y -> (# | mkUnbox_canon y #) }
--
-- See Note [Desugaring box & unbox].
mkUnboxSum_canon :: MonadUnique m => [RuntimeRepType] -> CoreExpr -> m CoreExpr
mkUnboxSum_canon elt_reps e =
  do { alts <- zipWithM mk_alt [0 ..] elt_reps
     ; return $
         mkWildCase e (unrestricted (boxSumTy elt_reps)) canon_ty
           ( Alt DEFAULT [] (mkImpossibleExpr canon_ty "mkUnboxSum_canon: alternative out of range")
           : alts ) }
  where
    arity    = length elt_reps
    elt_tys  = map canonicalTypeOfRep elt_reps
    canon_ty = mkSumTy elt_tys
    rs_ty    = mkPromotedListTy runtimeRepTy elt_reps

    mk_alt k comp_rep =
      do { x <- mkSysLocalM (fsLit "ub") ManyTy (boxTFTy (listIndexTy rs_ty k))
           -- x |> (BoxTF (rs !! k) ~R# BoxTF r_k)
         ; u <- mkUnbox_canon comp_rep (mkCast (Var x) (mkSubCo (boxTFIndexCo rs_ty k)))
         ; return $ Alt (DataAlt (boxSumDataCon k)) [x]
                        (mkCoreUnboxedSum arity (k + 1) elt_tys u) }

-- | @boxTFIndexCo rs k :: BoxTF (rs !! k) ~# BoxTF r_k@.
boxTFIndexCo :: Type -> Int -> CoercionN
boxTFIndexCo rs_ty k =
  mkTyConAppCo Nominal boxTFTyCon
    [ mkAxiomCo listIndexCoAxiomRule
        [ mkNomReflCo runtimeRepTy
        , mkNomReflCo rs_ty
        , mkNomReflCo (mkNumLitTy (toInteger k))
        ]
    ]

-- | @mkCanonicalCo ty@ coerces @ty :: TYPE r@ to the canonical type of @r@.
-- This is 'MRefl' when @ty@ is already canonical.
--
-- See Note [The canonical type of a RuntimeRep].
mkCanonicalCo :: Type -> MCoercionR
mkCanonicalCo ty
  | isReflCo co = MRefl
  | otherwise   = MCo co
  where
    -- NB: mkUnivCo returns 'Refl' when the LHS and RHS types are equal.
    co    = mkUnivCo CanonicalProv [] Representational ty canon
    rep   = kindRep $ typeKind ty
    canon = canonicalTypeOfRep rep

-- | The boxed type of a constraint @c@: @DictBox ((##) -=> c)@.
--
-- See [Boxing constraints] in GHC.Builtin.WiredIn.Types.Box.
boxDictTy :: Type -> Type
boxDictTy c = mkTyConApp dictBoxTyCon [liftConstraint c]

-- | Make a constraint /lifted/ by wrapping it in @(##) -=>@.
--
-- See [Boxing constraints] in GHC.Builtin.WiredIn.Types.Box.
liftConstraint :: Type -> Type
liftConstraint c = mkFunTy visArgConstraintLike ManyTy unboxedUnitTy c

-- | Recover the original constraint from one made lifted via 'liftConstraint',
-- by applying it to @(##)@.
--
-- See [Boxing constraints] in GHC.Builtin.WiredIn.Types.Box.
unliftConstraintExpr :: CoreExpr -> CoreExpr
unliftConstraintExpr e = App e (mkConApp (tupleDataCon Unboxed 0) []) -- e (# #)
