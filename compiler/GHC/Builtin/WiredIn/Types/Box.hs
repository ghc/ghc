{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Wired-in types and type families for boxing values of any representation.
--
-- See Note [Boxing constructors].
module GHC.Builtin.WiredIn.Types.Box (
        -- * All wired-in boxing TyCons
        boxWiredInTyCons,

        -- * Box
        boxTyCon, boxTFTyCon, boxCoAxiomRule,
        RuntimeRepBoxingInfo(..), repBoxingInfo, boxTF, boxTFTy, boxTupleTy, boxSumTy,
        boxSumTyCon, boxSumDataCon, listIndexTy,
        listIndexTyCon, listIndexCoAxiomRule,

        -- * Boxing constraints
        dictBoxTyCon, mkDictBoxDataCon,

        -- * The canonical type of a RuntimeRep
        canonicalTypeOfRep,
    ) where

import GHC.Prelude

-- friends:
import GHC.Builtin.KnownKeys
import GHC.Builtin.Modules ( gHC_INTERNAL_BOX, gHC_INTERNAL_TYPENATS )
import GHC.Builtin.WiredIn.Prim
import GHC.Builtin.WiredIn.Types
import GHC.Builtin.Uniques ( mkBoxingTyConUnique, boxingDataConUnique )

-- others:
import GHC.Core.Coercion.Axiom
import GHC.Core.Type
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.Map.Type  ( TypeMap, emptyTypeMap, extendTypeMap, lookupTypeMap )

import GHC.Types.TyThing
import GHC.Types.Name
import GHC.Types.Basic

import GHC.Settings.Constants ( mAX_SUM_SIZE )

import GHC.Data.FastString
import GHC.Data.Pair ( Pair(..) )

import GHC.Utils.Outputable
import GHC.Utils.Misc  ( HasDebugCallStack, equalLength )
import GHC.Utils.Panic

-- | All the wired-in boxing 'TyCon's.
boxWiredInTyCons :: [TyCon]
boxWiredInTyCons = boxTyCon : boxSumTyCon : dictBoxTyCon : boxingTyCons

{- *********************************************************************
*                                                                      *
              Boxing data constructors
*                                                                      *
********************************************************************* -}

{- Note [Boxing constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sometimes, we have an unboxed type but the rest of the code requires working
with boxed types:

  * In desugaring, when we need to package up a bunch of values into a tuple,
    for example when desugaring arrows, recursive do blocks, or list/monad
    comprehensions. See Note [Big tuples] in GHC.Core.Make.BigTuple and
    Note [Boxing big tuple elements] in GHC.HsToCore.Utils.

  * In let-floating, when we want to float an unlifted sub-expression.
    See Note [Floating MFEs of unlifted type] in GHC.Core.Opt.SetLevels

To resolve the mismatch, we want to take the unboxed value and "box it up",
turning it into a boxed value. For example, given a value of type
(# Int#, Float# #), we would like (roughly) to box it into a value of type
(Int, Float). More precisely:

  (BOX1)   Ticket: #22336

    Boxing should be supported at /every/ representation (every RuntimeRep).
    In particular, this includes historically unsupported representations such as:

      - TupleRep and SumRep
      - AddrRep
      - VecRep, for all supported combinations of vector size and element type,
        e.g. 'VecRep Vec2 DoubleElemRep', 'VecRep Vec8 Int32ElemRep', ...

    Moreover, boxing should work at every type, not just built-in ones.
    For example, suppose we have "F Int :: TYPE (TupleRep '[IntRep, FloatRep])",
    where 'F' might be a type family or an unlifted newtype. Then we should be
    able to box a value (v :: F Int) based on the /RuntimeRep/ of the type,
    namely "TupleRep '[IntRep, FloatRep]".

  (BOX2)   Tickets: #25065, #20864

    During typechecking, we might have a type (ty :: TYPE kappa) in which the
    representation, kappa, is a yet unfilled metavariable.
    We need a way to refer to the associated boxed type before 'kappa' is
    filled in.

    Specific examples:
      - the treatment of parallel list comprehensions and recursive do
        in GHC.Tc.Gen.Match.tcMcStmt
      - the treatment of transform list comprehensions
        in GHC.Tc.Gen.Match.{tcLcStmt,tcMcStmt}

  (BOX3)   Ticket: #22473

    We would like to make the boxing machinery available to the programmer,
    in the form of the following interface:

      type Box :: forall (r :: RuntimeRep). TYPE r -> Type
      box :: forall {r :: RuntimeRep} (a :: TYPE r). a -> Box a
      unbox :: forall {r :: RuntimeRep} (a :: TYPE r). Box a -> a

Here is how the boxing machinery works:

* In GHC.Internal.Box, we define:
  - A fixed family of /boxing data constructors/, one per RuntimeRep
    (not including BoxedRep, TupleRep and SumRep):
      data BoxInt  = BoxInt  Int#
      data BoxWord = BoxWord Word#
      data BoxAddr = BoxAddr Addr#
      ...
     All these data types are actually wired-in. See GHC.Builtin.WiredIn.Types.Box
     and Wrinkle [Wiring-in boxing data constructors].

  - A type family BoxTF:
      type family BoxTF r where
        BoxTF IntRep  = BoxInt
        BoxTF WordRep = BoxWord
        BoxTF AddrRep = BoxAddr
        ...
   The most important point here is that the boxing data constructor is determined
   purely by the runtime representation, not by the type being boxed. In other
   words, the 'BoxTF' type family takes only a 'RuntimeRep' argument, not the full
   type being boxed. See Wrinkle [BoxTF instances] below.

  - A user-facing 'Box' type, as a wrapper around 'BoxTF'
      newtype Box (a :: TYPE r) = MkBox (BoxTF r)
      type role Box representational

    Why a newtype 'Box' that wraps 'BoxTF'? See Wrinkle [The Box newtype] below.

* The desugarer rewrites calls (box @rr) and (unbox @rr), dispatching on the
   RuntimeRep 'rr' (always concrete). This desugaring is done by
   GHC.HsToCore.Expr.ds_box_unbox, which calls GHC.Core.Make.{mkBox,mkUnbox}
   to do the heavy lifting.
   See Note [Desugaring box & unbox] in GHC.Core.Make.Box.

* When the let-floater wants to box something, notably in GHC.Core.Opt.SetLevels.lvlMFE,
  it directly applies the appropriate boxing data constructor; see 'box_and_float'.
  (lvlMFE does not directly call 'mkBox', as that works on CoreExpr and not
  LevelledExpr; making 'mkBox' polymorphic over 'Expr b' is more hassle than
  it's worth.)

And that's it!  But there are wrinkles of course:

Wrinkle [The Box newtype]

  The 'Box' newtype achieves several things compared to 'type Box (a :: TYPE r) = BoxTF r':
    - The role annotation on 'Box' avoids the type parameter 'a' being phantom;
      that is, 'Box' keeps track of the original type of the value being boxed.
    - It allows 'Box' to be partially applied (generally useful to users).
    - Hiding the constructor avoids exposing the internal 'BoxTF' type family
      and its reduction behaviour to users.

Wrinkle [BoxTF instances]

  'BoxTF' is a /wired-in/ type family (the tuple instances require infinitely
  many equations); see GHC.Builtin.WiredIn.Types.Box.boxTF.

Wrinkle [Boxing Int, Float etc]

  We could re-use e.g. 'Int' instead of 'BoxInt', but it's simpler not to:
    - it's convenient to have all the boxing data constructors in one place,
    - some types such as 'Int8', 'Word8' are not wired-in,
    - there is no 'Addr' type, just 'data Ptr a = Ptr Addr#',
    - there are no standard boxed VecRep types we could use.

Wrinkle [Boxing boxed types]

  A lifted type is already boxed, so defining 'BoxTF LiftedRep = Any @Type'
  avoids introducing a pointless extra indirection.

Wrinkle [Boxing constraints]

  Constraints (kind @CONSTRAINT r@) cannot be boxed with 'Box', as it takes an
  argument of kind @TYPE r@. Instead, a constraint @c@ is boxed with:

    data DictBox (c :: Constraint) where MkDictBox :: c => DictBox c

  'DictBox' only takes a /lifted/ constraint (kind @Constraint@). A non-lifted
  constraint @c :: CONSTRAINT r@ is first made lifted by wrapping it in a function
  arrow: @(##) -=> c@ is a lifted constraint regardless of @r@. So a single
  'DictBox' suffices for constraints of any representation.

  See Note [Evidence terms from Unsatisfiable Givens] in GHC.Tc.Solver.Default,
  which introduced this trick.

Wrinkle [Boxing TupleRep and SumRep]

  The key decision we make is to box values of TupleRep/SumRep representations
  by recursively boxing their component(s). This avoids a combinatorial
  explosion in which we would need separate boxing data constructors for every
  combination of RuntimeReps, e.g.

    - one boxing DataCon for TupleRep '[ IntRep  , WordRep ]
    -            another for TupleRep '[ FloatRep, IntRep ]
    - ...

  The BoxTF type family equations for tuples are thus of the form:

    BoxTF (TupleRep '[r1, r2, r3]) = (BoxTF r1, BoxTF r2, BoxTF r3)
      -- boxed tuple of recursively boxed components

  Boxed tuples only go up to 'mAX_TUPLE_SIZE' in arity, so for larger arities
  the boxed tuple is built out of nested tuples using 'mkChunkified'.

  For SumRep, we don't have n-ary boxed sums, so we define the following:

    BoxTF (SumRep rs) = BoxSum rs

    type BoxSum :: [RuntimeRep] -> Type
    data BoxSum rs
      = MkBoxSum0 (BoxTF (rs !! 0))
      | MkBoxSum1 (BoxTF (rs !! 1))
      | ...

  Each alternative gets its own constructor (rather than storing a tag alongside
  a payload), so that the type of the payload is fixed by the constructor alone.

Wrinkle [Boxing VecRep]

  A bit of special care is required to ensure we can compile the VecRep
  boxing data constructors on all CPUs.
  See Note [No implicit binds for Box constructors] in GHC.CoreToStg.AddImplicitBinds.

Wrinkle [Boxing IntRep/WordRep]

  The RTS has special logic for sharing small Int#/Char# values; see
  Note [Precomputed static closures] in GHC.StgToCmm.DataCon as well as
  Note [CHARLIKE and INTLIKE closures] in the RTS.
  We extend the INTLIKE logic to 'BoxInt' in GHC.Builtin.maybeIntLikeCon and
  in the garbage collector (rts/sm/Evac.c).
  We can't easily do the same for 'BoxWord' as we would need different
  constructors to distinguish boxing 'Char#' from boxing 'Word#'.

Wrinkle [Wiring-in boxing data constructors]

  All the boxing data constructors are wired in (and so are their parent TyCons).
  There are two main reasons for this:

    (1) There are a lot of boxing data constructors. Wiring them in ensures
        they don't bloat interface files.
    (2) Both the desugarer and the float-out pass (GHC.Core.Opt.SetLevels) need
        to construct the boxing 'DataCon' given a concrete RuntimeRep, as
        explained in Note [Desugaring box & unbox]. It's much simpler to wire-in
        the data constructors, as this avoids having to thread through type
        environments (both to the big tuple machinery described in
        Note [Big tuples] in GHC.Core.Make.BigTuple and to the Core optimisation passes).

Wrinkle [Why not make Box a data family?]

  It would be possible to define 'Box' as a data family. However, all the data
  family instances would need to be wired in. Wiring in data family instances,
  while doable, would introduce a fair amount of avoidable complexity (in
  particular in instance lookup functions). It's much simpler to use a wired-in
  closed type family.
-}

boxTyConName :: Name
boxTyConName =
  mkWiredInTyConName UserSyntax gHC_INTERNAL_BOX (fsLit "Box") boxTyConKey boxTyCon

boxTFTyConName :: Name
boxTFTyConName =
  mkWiredInTyConName UserSyntax gHC_INTERNAL_BOX (fsLit "BoxTF") boxTFTyConKey boxTFTyCon

mkBoxDataConName :: Name
mkBoxDataConName =
  mkWiredInDataConName UserSyntax gHC_INTERNAL_BOX (fsLit "MkBox") mkBoxDataConKey mkBoxDataCon

-- | Build a wired-in newtype, together with its data constructor and its
-- (implicit, representational) unwrapping coercion axiom.
mkWiredInNewTyCon :: HasDebugCallStack
                  => Name          -- ^ 'TyCon' name
                  -> Name          -- ^ 'DataCon' name
                  -> Unique        -- ^ 'Unique' of the coercion axiom
                  -> [TyConBinder] -- ^ binders of the newtype
                  -> [Role]        -- ^ roles of the binders
                  -> Type          -- ^ the newtype field type
                  -> (TyCon, DataCon)
mkWiredInNewTyCon tc_name dc_name nt_co_uniq bndrs roles nt_fld_ty =
  assertPpr (equalLength bndrs roles) (ppr tc_name $$ ppr bndrs $$ ppr roles)
  (tc, dc)
  where
    tvs = binderVars bndrs

    tc = mkAlgTyCon tc_name (mkTyConKind bndrs liftedTypeKind)
           bndrs 0 liftedTypeKind
           roles
           Nothing   -- no CType
           []        -- no stupid theta
           (NewTyCon { data_con     = dc
                     , nt_rhs       = nt_fld_ty
                     , nt_etad_rhs  = (tvs, nt_fld_ty)
                     , nt_co        = nt_co
                     , nt_fixed_rep = True })
           (VanillaAlgTyCon (mkPrelTyConRepName tc_name))
           False     -- not declared with GADT syntax

    dc = pcDataCon dc_name tvs [nt_fld_ty] tc

    nt_co_name = mkWiredInName (nameModule tc_name) (mkNewTyCoOcc (nameOccName tc_name)) nt_co_uniq
                   (ACoAxiom (toBranchedAxiom nt_co)) UserSyntax
    nt_co = CoAxiom { co_ax_unique   = nt_co_uniq
                    , co_ax_name     = nt_co_name
                    , co_ax_tc       = tc
                    , co_ax_role     = Representational
                    , co_ax_implicit = True
                    , co_ax_branches = unbranched branch }
    branch = CoAxBranch { cab_loc     = getSrcSpan nt_co_name
                        , cab_tvs     = tvs
                        , cab_eta_tvs = []
                        , cab_cvs     = []
                        , cab_roles   = roles
                        , cab_lhs     = mkTyVarTys tvs
                        , cab_rhs     = nt_fld_ty
                        , cab_overlaps = []
                        , cab_incomps  = [] }

-- | The wired-in @Box@ newtype:
--
-- > newtype Box @r (a :: TYPE r) = MkBox (BoxTF r)
-- > type role Box representational
--
-- See Wrinkle [The Box newtype] in Note [Boxing constructors].
boxTyCon :: TyCon
mkBoxDataCon :: DataCon
(boxTyCon, mkBoxDataCon) =
  mkWiredInNewTyCon boxTyConName mkBoxDataConName boxCoKey bndrs roles rep_ty
  where
    r = runtimeRep1TyVar
    a = openAlphaTyVar

    -- type Box :: forall (r :: RuntimeRep). TYPE r -> Type
    bndrs = [ mkNamedTyConBinder Specified r
            , mkAnonTyConBinder a ]
    roles = [ Nominal, Representational ]

    rep_ty = mkTyConApp boxTFTyCon [ mkTyVarTy r ]  -- BoxTF r

-- | The internal @BoxTF@ type family that implements the boxing scheme
-- described in Note [Boxing constructors].
boxTFTyCon :: TyCon
boxTFTyCon =
  mkFamilyTyCon
    boxTFTyConName kind binders 0 liftedTypeKind
    Nothing
    (ClosedTypeFamilyTyCon $ CTF_BuiltIn ops)
    Nothing
    NotInjective
  where
    binders = [ mkAnonTyConBinder runtimeRep1TyVar ]
    kind    = mkTyConKind binders liftedTypeKind
    ops     = trivialBuiltInClosedTyFam { bctfMatchFam = [axBox] }

-- | @DictBox@ boxes a (lifted) constraint into a value of kind @Type@:
--
-- > data DictBox c where MkDictBox :: c => DictBox c
--
-- See Note [Boxing constructors].
dictBoxTyCon :: TyCon
mkDictBoxDataCon :: DataCon
(dictBoxTyCon, mkDictBoxDataCon) = (tc, dc)
  where
    tc = pcTyCon dictBoxTyConName Nothing [c_tv] [dc]
    dc = pcDataConConstraint mkDictBoxDataConName [c_tv] [mkTyVarTy c_tv] tc
    c_tv = head (mkTemplateTyVars [constraintKind])  -- the constraint c :: Constraint

dictBoxTyConName :: Name
dictBoxTyConName =
    mkWiredInTyConName UserSyntax gHC_INTERNAL_BOX (fsLit "DictBox") dictBoxTyConKey dictBoxTyCon

mkDictBoxDataConName :: Name
mkDictBoxDataConName =
    mkWiredInDataConName UserSyntax gHC_INTERNAL_BOX (fsLit "MkDictBox") mkDictBoxDataConKey mkDictBoxDataCon

-- | Build a wired-in boxing data type, e.g.
--
-- > data BoxInt = BoxInt Int#
--
-- See Note [Boxing constructors].
mkBoxDataTyCon :: Int         -- ^ boxing 'Unique' index
               -> FastString  -- ^ constructor name (e.g. @BoxInt@)
               -> Type        -- ^ field type (e.g. @Int#@);
               -> TyCon
mkBoxDataTyCon i con canon = tc
  where
    tc_uniq = mkBoxingTyConUnique i
    dc_uniq = boxingDataConUnique tc_uniq
    tc_name = mkWiredInTyConName   UserSyntax gHC_INTERNAL_BOX con tc_uniq tc
    dc_name = mkWiredInDataConName UserSyntax gHC_INTERNAL_BOX con dc_uniq dc
    dc      = pcDataCon dc_name [] [canon] tc
    tc      = pcTyCon tc_name Nothing [] [dc]

-- | The boxing data types of Note [Boxing constructors], each with the
-- 'RuntimeRep' it boxes and the canonical type of that representation
-- (see Note [The canonical type of a RuntimeRep] in GHC.Core.Make.Box).
boxingDataTyCons :: [(RuntimeRepType, TyCon, Type)]
boxingDataTyCons =
  [ (rep, mkBoxDataTyCon i (fsLit con) canon, canon)
  | (i, (rep, con, canon)) <- zip [1..] (scalar_reps ++ vec_reps)
  ]
  where
    scalar_reps :: [(RuntimeRepType, String, Type)]
    scalar_reps =
      [ (    intRepDataConTy,      "BoxInt",    intPrimTy )
      , (   int8RepDataConTy,     "BoxInt8",   int8PrimTy )
      , (  int16RepDataConTy,    "BoxInt16",  int16PrimTy )
      , (  int32RepDataConTy,    "BoxInt32",  int32PrimTy )
      , (  int64RepDataConTy,    "BoxInt64",  int64PrimTy )
      , (   wordRepDataConTy,     "BoxWord",   wordPrimTy )
      , (  word8RepDataConTy,    "BoxWord8",  word8PrimTy )
      , ( word16RepDataConTy,   "BoxWord16", word16PrimTy )
      , ( word32RepDataConTy,   "BoxWord32", word32PrimTy )
      , ( word64RepDataConTy,   "BoxWord64", word64PrimTy )
      , (   addrRepDataConTy,     "BoxAddr",   addrPrimTy )
      , (  floatRepDataConTy,    "BoxFloat",  floatPrimTy )
      , ( doubleRepDataConTy,   "BoxDouble", doublePrimTy )
      , (      unliftedRepTy, "BoxUnlifted", anyTypeOfKind unliftedTypeKind )
      ]

    vec_reps :: [(RuntimeRepType, String, Type)]
    vec_reps =
      [ (rep, con_str, prim_vec_ty rep)
      | (elem_ty, elem_nm, w) <- vec_elems
      , (count, count_ty)     <- vec_counts
      , (count * w) `elem` [128, 256, 512]
      , let rep = mkTyConApp vecRepDataConTyCon [count_ty, elem_ty]
            con_str = "BoxVec" ++ show count ++ elem_nm
      ]
    vec_elems :: [(Type, String, Int)]
    vec_elems =
      [ (int8ElemRepDataConTy  , "Int8"  ,  8)
      , (int16ElemRepDataConTy , "Int16" , 16)
      , (int32ElemRepDataConTy , "Int32" , 32)
      , (int64ElemRepDataConTy , "Int64" , 64)
      , (word8ElemRepDataConTy , "Word8" , 8 )
      , (word16ElemRepDataConTy, "Word16", 16)
      , (word32ElemRepDataConTy, "Word32", 32)
      , (word64ElemRepDataConTy, "Word64", 64)
      , (floatElemRepDataConTy , "Float" , 32)
      , (doubleElemRepDataConTy, "Double", 64)
      ]
    vec_counts :: [(Int, Type)]
    vec_counts =
      [ ( 2, vec2DataConTy )
      , ( 4, vec4DataConTy )
      , ( 8, vec8DataConTy )
      , (16, vec16DataConTy)
      , (32, vec32DataConTy)
      , (64, vec64DataConTy)
      ]

    prim_vec_ty :: RuntimeRepType -> Type
    prim_vec_ty rep = case lookupTypeMap prim_vec_tys rep of
      Just ty -> ty
      Nothing -> pprPanic "boxingDataTyCons: no primitive vector type" (ppr rep)

    prim_vec_tys :: TypeMap Type
    prim_vec_tys = foldl add emptyTypeMap primTyCons
      where
        add m tc
          | Just rep <- kindRep_maybe (tyConKind tc)
          , Just (rep_tc, _) <- splitTyConApp_maybe rep
          , rep_tc == vecRepDataConTyCon
          = extendTypeMap m rep (mkTyConTy tc)
          | otherwise
          = m

-- | The 'TyCon's of the boxing 'DataCon's of Note [Boxing constructors].
boxingTyCons :: [TyCon]
boxingTyCons = [ tc | (_, tc, _) <- boxingDataTyCons ]

listIndexTyConName :: Name
listIndexTyConName =
  mkWiredInTyConName UserSyntax gHC_INTERNAL_TYPENATS (fsLit "!!") listIndexTyConKey listIndexTyCon

-- | The built-in type-level list-indexing type family @(!!)@.
listIndexTyCon :: TyCon
listIndexTyCon =
  mkFamilyTyCon
    listIndexTyConName kind binders 0 res_kind
    Nothing
    (ClosedTypeFamilyTyCon $ CTF_BuiltIn ops)
    Nothing
    NotInjective
  where
    binders  = mkTemplateTyConBinders [liftedTypeKind]
                 (\ ~[k] -> [mkListTy k, naturalTy])
    res_kind = mkTyVarTy (head (binderVars binders))  -- k
    kind     = mkTyConKind binders res_kind
    ops      = trivialBuiltInClosedTyFam { bctfMatchFam = [axListIndex] }

-- | The type family reduction rule of the built-in @(!!)@ type family.
axListIndex :: BuiltInFamRewrite
axListIndex = bif
  where
    bif = BIF_Rewrite
      { bifrw_name   = fsLit "ListIndexDef"
      , bifrw_axr    = BuiltInFamRew bif
      , bifrw_fam_tc = listIndexTyCon
      , bifrw_arity  = 3
      , bifrw_match  = \ ts ->
          do { [_k, xs, n] <- return ts
             ; elt <- list_index xs n
             ; return (ts, elt) }
      , bifrw_proves = \ cs ->
          do { [Pair k1 _k2, Pair xs1 xs2, Pair n1 n2] <- return cs
             ; elt <- list_index xs2 n2
             ; return (Pair (mkTyConApp listIndexTyCon [k1, xs1, n1]) elt) } }

    list_index xs n =
      do { i <- isNumLitTy n
         ; if i < 0 then Nothing else go i xs }
      where
        go i list_ty
          | Just (tc, [_k, elt, rest]) <- splitTyConApp_maybe list_ty
          , tc `hasKey` consDataConKey
          = if i == 0 then Just elt else go (i - 1) rest
          | otherwise
          = Nothing

-- | The 'CoAxiomRule' for the type family reduction of @(!!)@.
listIndexCoAxiomRule :: CoAxiomRule
listIndexCoAxiomRule = bifrw_axr axListIndex

boxSumTyConName :: Name
boxSumTyConName =
  mkWiredInTyConName UserSyntax gHC_INTERNAL_BOX (fsLit "BoxSum") boxSumTyConKey boxSumTyCon

-- | The wired-in 'BoxSum' data type:
--
-- > type BoxSum :: [RuntimeRep] -> Type
-- > data BoxSum rs
-- >   = MkBoxSum0 (BoxTF (rs !! 0))
-- >   | MkBoxSum1 (BoxTF (rs !! 1))
-- >   | ...                          -- 'mAX_SUM_SIZE' constructors in total
--
-- See [Boxing TupleRep and SumRep].
boxSumTyCon :: TyCon
boxSumTyCon = tc
  where
    bndrs = mkTemplateAnonTyConBinders [mkListTy runtimeRepTy]  -- rs :: [RuntimeRep]
    [rs]  = binderVars bndrs
    rs_ty = mkTyVarTy rs

    tc = mkAlgTyCon boxSumTyConName
                    (mkTyConKind bndrs liftedTypeKind) bndrs 0 liftedTypeKind
                    [Nominal]  -- rs occurs under the type family (!!)
                    Nothing    -- no CType
                    []         -- no stupid theta
                    (mkDataTyConRhs (map mk_dc [0 .. mAX_SUM_SIZE - 1]))
                    (VanillaAlgTyCon (mkPrelTyConRepName boxSumTyConName))
                    False      -- not declared with GADT syntax

    -- MkBoxSum_k :: forall rs. BoxTF (rs !! k) -> BoxSum rs
    mk_dc :: Int -> DataCon
    mk_dc k = dc
      where
        dc      = pcDataCon dc_name [rs] [boxTFTy (listIndexTy rs_ty k)] tc
        dc_name = mkWiredInDataConName UserSyntax gHC_INTERNAL_BOX
                    (fsLit ("MkBoxSum" ++ show k)) (boxSumDataConKey k) dc

-- | @boxSumDataCon k@ is the constructor @MkBoxSum_k@ of 'boxSumTyCon', which
-- stores alternative @k@ (counting from 0) of an unboxed sum.
boxSumDataCon :: Int -> DataCon
boxSumDataCon k =
  case drop k (tyConDataCons boxSumTyCon) of
    dc : _ | k >= 0 -> dc
    _ -> pprPanic "boxSumDataCon: alternative out of range" (ppr k)

-- | @listIndexTy rs k@ is the type @rs !! k@.
listIndexTy :: Type -> Int -> Type
listIndexTy rs_ty k = mkTyConApp listIndexTyCon [runtimeRepTy, rs_ty, mkNumLitTy (toInteger k)]

-- | How to box a value of a given 'RuntimeRep' (one per 'RuntimeRep').
--
-- See Note [Boxing constructors] and [Boxing TupleRep and SumRep].
data RuntimeRepBoxingInfo
  -- | The type is already boxed: no additional boxing needed.
  --
  -- @BoxTF LiftedRep = Any \@Type@.
  = BoxLifted
  -- | Box using a specific data constructor, e.g. box @e :: ty :: TYPE IntRep@
  -- using @BoxInt@.
  | BoxWithDataCon
      DataCon -- ^ the boxing data constructor
      Type    -- ^ the canonical type of this representation, e.g. @Int#@ for @IntRep@.
              --
              -- see Note [The canonical type of a RuntimeRep] in GHC.Core.Make.Box
  -- | @TupleRep rs@ or @SumRep rs@: box each component.
  | BoxComponents
      UnboxedTupleOrSum
      [RuntimeRepType] -- ^ the component representations

-- | A map of all boxing data constructors, keyed by 'RuntimeRep'.
--
-- Example: @IntRep ↦ BoxWithDataCon BoxInt Int#@.
dataConBoxingInfos :: TypeMap RuntimeRepBoxingInfo
dataConBoxingInfos =
  foldl add emptyTypeMap boxingDataTyCons
  where
    add m (rep, tc, canon) =
      extendTypeMap m rep (BoxWithDataCon (tyConSingleDataCon tc) canon)

-- | How to box a value of a given 'RuntimeRep'.
repBoxingInfo_maybe :: RuntimeRepType -> Maybe RuntimeRepBoxingInfo
repBoxingInfo_maybe rep
  | isLiftedRuntimeRep rep
  = Just BoxLifted
  | Just (tc, [rs]) <- splitTyConApp_maybe rep
  , Just tup_or_sum <- tupleOrSum_maybe tc
  = BoxComponents tup_or_sum <$> extractPromotedList_maybe rs
  | otherwise
  = lookupTypeMap dataConBoxingInfos rep
  where
    tupleOrSum_maybe tc
      | tc == tupleRepDataConTyCon = Just UnboxedTupleType
      | tc == sumRepDataConTyCon   = Just UnboxedSumType
      | otherwise                  = Nothing

-- | How to box a value of a given concrete 'RuntimeRep'.
--
-- Panics on a non-concrete 'RuntimeRep'.
repBoxingInfo :: HasDebugCallStack => RuntimeRepType -> RuntimeRepBoxingInfo
repBoxingInfo rep =
  case repBoxingInfo_maybe rep of
    Just nfo -> nfo
    Nothing  -> pprPanic "repBoxingInfo: non-concrete RuntimeRep" (ppr rep)

-- | Reduces @BoxTF rep@ at a specific @RuntimeRep@.
boxTF :: RuntimeRepBoxingInfo -> Type
boxTF BoxLifted                           = anyTypeOfKind liftedTypeKind
boxTF (BoxWithDataCon dc _)               = mkTyConTy (dataConTyCon dc)
boxTF (BoxComponents UnboxedTupleType rs) = boxTupleTy rs
boxTF (BoxComponents UnboxedSumType   rs) = boxSumTy   rs

-- | The (unreduced) type family application @BoxTF r@.
boxTFTy :: RuntimeRepType -> Type
boxTFTy r = mkTyConApp boxTFTyCon [r]

-- | Reduces @BoxTF (TupleRep rs)@:
-- @boxTupleTy [r1, ..., r_n] = (BoxTF r_1, ..., BoxTF r_n)@,
-- chunked with 'mkChunkified' and flattening 1-tuples.
--
-- See [Boxing TupleRep and SumRep].
boxTupleTy :: [RuntimeRepType] -> Type
boxTupleTy rs = mkChunkified mkBoxedTupleTy (map boxTFTy rs)

-- | Reduces @BoxTF (SumRep rs)@: @boxSumTy rs = BoxSum rs@.
--
-- See [Boxing TupleRep and SumRep].
boxSumTy :: [RuntimeRepType] -> Type
boxSumTy rs = mkTyConApp boxSumTyCon [mkPromotedListTy runtimeRepTy rs]

-- | The canonical type of a concrete 'RuntimeRep'.
--
-- Panics on a non-concrete 'RuntimeRep'.
--
-- See Note [The canonical type of a RuntimeRep] in GHC.Core.Make.Box.
canonicalTypeOfRep :: HasDebugCallStack => RuntimeRepType -> Type
canonicalTypeOfRep rep =
  case repBoxingInfo rep of
    BoxLifted                          -> anyTypeOfKind liftedTypeKind
    BoxWithDataCon _ canon             -> canon
    BoxComponents UnboxedTupleType rs  -> mkTupleTy1 Unboxed (map canonicalTypeOfRep rs)
    BoxComponents UnboxedSumType   rs  -> mkSumTy (map canonicalTypeOfRep rs)

-- | The type family reduction rule of the @BoxTF@ built-in closed type family.
axBox :: BuiltInFamRewrite
axBox = bif
  where
    bif = BIF_Rewrite
      { bifrw_name   = fsLit "BoxTFDef"
      , bifrw_axr    = BuiltInFamRew bif
      , bifrw_fam_tc = boxTFTyCon
      , bifrw_arity  = 1
      , bifrw_match  = \ ts ->
          do { [rep] <- return ts
             ; nfo <- repBoxingInfo_maybe rep
             ; return (ts, boxTF nfo) }
      , bifrw_proves = \ cs ->
          do { [Pair rep1 rep2] <- return cs
             ; nfo <- repBoxingInfo_maybe rep2
             ; return (Pair (boxTFTy rep1) (boxTF nfo)) } }

-- | The 'CoAxiomRule' for the type family reduction of 'BoxTF'.
boxCoAxiomRule :: CoAxiomRule
boxCoAxiomRule = bifrw_axr axBox
