

-----------------------------------------------------------------------------
--
-- Stg to C--: code generation for constructors
--
-- This module provides the support code for StgToCmm to deal with
-- constructors on the RHSs of let(rec)s.
--
-- (c) The University of Glasgow 2004-2006
--
-----------------------------------------------------------------------------

module GHC.StgToCmm.DataCon (
        cgTopRhsCon, buildDynCon, bindConArgs, bindFakeConArg
    ) where

import GHC.Prelude

import GHC.Platform

import GHC.Stg.Utils (allowTopLevelConApp)
import GHC.Stg.Syntax
import GHC.Core  ( AltCon(..) )

import GHC.StgToCmm.Monad
import GHC.StgToCmm.Env
import GHC.StgToCmm.Heap
import GHC.StgToCmm.Layout
import GHC.StgToCmm.Utils
import GHC.StgToCmm.Closure

import GHC.Cmm.Expr
import GHC.Cmm.Utils
import GHC.Cmm.CLabel
import GHC.Cmm.Graph
import GHC.Runtime.Heap.Layout
import GHC.Types.CostCentre
import GHC.Unit
import GHC.Core.DataCon
import GHC.Data.FastString
import GHC.Types.Id
import GHC.Types.Id.Info( CafInfo( NoCafRefs ) )
import GHC.Types.Name (isInternalName)
import GHC.Types.RepType (countConRepArgs)
import GHC.Types.Literal
import GHC.Builtin.Utils
import GHC.Utils.Panic
import GHC.Utils.Misc
import GHC.Utils.Monad (mapMaybeM)

import Data.Char
import GHC.StgToCmm.Config (stgToCmmPlatform)
import GHC.StgToCmm.TagCheck (checkConArgsStatic, checkConArgsDyn)
import GHC.Utils.Outputable
import GHC.Builtin.Types (justDataConName, maybeTyConName, maybeTyCon)
import GHC.Core.TyCon
import GHC.Core.Type
import GHC.Tc.Utils.TcType (isTyFamFree)

---------------------------------------------------------------
--      Top-level constructors
---------------------------------------------------------------

cgTopRhsCon :: StgToCmmConfig
            -> Id               -- Name of thing bound to this RHS
            -> DataCon          -- Id
            -> ConstructorNumber
            -> [NonVoid StgArg] -- Args
            -> (CgIdInfo, FCode ())
cgTopRhsCon cfg id con mn args
  | Just static_info <- precomputedStaticConInfo_maybe cfg id con args
  , let static_code | isInternalName name = pure ()
                    | otherwise           = gen_code
  = -- There is a pre-allocated static closure available; use it
    -- See Note [Precomputed static closures].
    -- For External bindings we must keep the binding,
    -- since importing modules will refer to it by name;
    -- but for Internal ones we can drop it altogether
    -- See Note [About the NameSorts] in "GHC.Types.Name" for Internal/External
    (static_info, static_code)

  -- Otherwise generate a closure for the constructor.
  | otherwise
  = (id_Info, gen_code)

  where
   platform      = stgToCmmPlatform cfg
   id_Info       = litIdInfo platform id (mkConLFInfo con) (CmmLabel closure_label)
   name          = idName id
   caffy         = idCafInfo id -- any stgArgHasCafRefs args
   closure_label = mkClosureLabel name caffy

   gen_code =
     do { profile <- getProfile
        ; this_mod <- getModuleName
        ; massert (allowTopLevelConApp platform (stgToCmmExtDynRefs cfg) this_mod con (map fromNonVoid args))
        ; massert (args `lengthIs` countConRepArgs con )
        ; checkConArgsStatic (text "TagCheck failed - Top level con") con (map fromNonVoid args)
        -- LAY IT OUT
        ; let
            (tot_wds, --  #ptr_wds + #nonptr_wds
             ptr_wds, --  #ptr_wds
             nv_args_w_offsets) =
                 mkVirtHeapOffsetsWithPadding profile StdHeader (addArgReps args)

        ; let
            -- Decompose padding into units of length 8, 4, 2, or 1 bytes to
            -- allow the implementation of mk_payload to use widthFromBytes,
            -- which only handles these cases.
            fix_padding (x@(Padding n off) : rest)
              | n == 0                 = fix_padding rest
              | n `elem` [1,2,4,8]     = x : fix_padding rest
              | n > 8                  = add_pad 8
              | n > 4                  = add_pad 4
              | n > 2                  = add_pad 2
              | otherwise              = add_pad 1
              where add_pad m = Padding m off : fix_padding (Padding (n-m) (off+m) : rest)
            fix_padding (x : rest)     = x : fix_padding rest
            fix_padding []             = []

            mk_payload (Padding len _) = return (CmmInt 0 (widthFromBytes len))
            mk_payload (FieldOff arg _) = do
                amode <- getArgAmode arg
                case amode of
                  CmmLit lit -> return lit
                  _          -> panic "GHC.StgToCmm.DataCon.cgTopRhsCon"

            nonptr_wds = tot_wds - ptr_wds

             -- we're not really going to emit an info table, so having
             -- to make a CmmInfoTable is a bit overkill, but mkStaticClosureFields
             -- needs to poke around inside it.
            info_tbl = mkDataConInfoTable profile con (addModuleLoc this_mod mn) True ptr_wds nonptr_wds


        ; payload <- mapM mk_payload (fix_padding nv_args_w_offsets)
                -- NB1: nv_args_w_offsets is sorted into ptrs then non-ptrs
                -- NB2: all the amodes should be Lits!
                --      TODO (osa): Why?

                -- BUILD THE OBJECT
                --
            -- We're generating info tables, so we don't know and care about
            -- what the actual arguments are. Using () here as the place holder.

        ; emitDataCon closure_label info_tbl dontCareCCS payload }

addModuleLoc :: Module -> ConstructorNumber -> ConInfoTableLocation
addModuleLoc this_mod mn = do
  case mn of
    NoNumber -> DefinitionSite
    Numbered n -> UsageSite this_mod n

---------------------------------------------------------------
--      Lay out and allocate non-top-level constructors
---------------------------------------------------------------

buildDynCon :: Id                 -- Name of the thing to which this constr will
                                  -- be bound
            -> ConstructorNumber
            -> Bool               -- is it genuinely bound to that name, or just
                                  -- for profiling?
            -> CostCentreStack    -- Where to grab cost centre from;
                                  -- current CCS if currentOrSubsumedCCS
            -> DataCon            -- The data constructor
            -> [NonVoid StgArg]   -- Its args
            -> FCode (CgIdInfo, FCode CmmAGraph)
               -- Return details about how to find it and initialization code
buildDynCon binder mn actually_bound cc con args
    = do cfg <- getStgToCmmConfig
         platform <- getPlatform
         --   pprTrace "noCodeLocal:" (ppr (binder,con,args,cgInfo)) True
         case precomputedStaticConInfo_maybe cfg binder con args of
           Just cgInfo -> return (cgInfo, return mkNop)
           Nothing     -> buildDynCon' platform cfg binder mn actually_bound cc con args

buildDynCon' :: Platform
             -> StgToCmmConfig
             -> Id
             -> ConstructorNumber
             -> Bool
             -> CostCentreStack
             -> DataCon
             -> [NonVoid StgArg]
             -> FCode (CgIdInfo, FCode CmmAGraph)
{- We used to pass a boolean indicating whether all the
args were of size zero, so we could use a static
constructor; but I concluded that it just isn't worth it.
Now I/O uses unboxed tuples there just aren't any constructors
with all size-zero args.

The reason for having a separate argument, rather than looking at
the addr modes of the args is that we may be in a "knot", and
premature looking at the args will cause the compiler to black-hole!
-}
-------- buildDynCon': the JUST JUICING case -----------
buildDynCon' platform conf binder _mn  _actually_bound _ccs con args
  | stgToCmmFastMaybe conf
  , dataConName con == justDataConName
  , [arg] <- args
  , NonVoid (StgVarArg just_payload) <- arg
  , mAX_PTR_TAG platform >= 3
  , isFakeablePayload just_payload
  , pprTrace "Found a JUST!" (ppr (con,args,stgArgType (fromNonVoid arg))) True
  = do  { let
              -- We represent the just as a pointer to *something* so we can't
              -- us LFCon
              lf_info = mkLFArgument just_payload

              gen_code reg
                = do  { modu <- getModuleName
                      ; cfg  <- getStgToCmmConfig
                      ; let platform = stgToCmmPlatform cfg

                      ; arg_code <- getArgAmode arg
                      ; let fake_just_code = mkFakeJust platform reg arg_code
                      ; return fake_just_code }
                  where

          ; (id_info, reg) <- rhsIdInfo binder lf_info
          ; return (id_info, gen_code reg)
        }
 where
  -- We cant fake a Just around a Nothing, so we only fake the just
  -- if the payload is sure not to be a Maybe a
  isFakeablePayload just_payload =
    let ty = idType just_payload
        tc_mb = tyConAppTyCon_maybe ty
    in
    isFunTy ty ||
    isAlgType ty &&
    not (tc_mb == Just maybeTyCon)



-------- buildDynCon': the general case -----------
buildDynCon' platform _conf binder mn actually_bound ccs con args
  = do  { (id_info, reg) <- rhsIdInfo binder lf_info
        ; return (id_info, gen_code reg)
        }
 where
  lf_info = mkConLFInfo con

  gen_code reg
    = do  { modu <- getModuleName
          ; cfg  <- getStgToCmmConfig
          ; let profile  = stgToCmmProfile  cfg
                (tot_wds, ptr_wds, args_w_offsets)
                   = mkVirtConstrOffsets profile (addArgReps args)
                nonptr_wds = tot_wds - ptr_wds
                info_tbl = mkDataConInfoTable profile con (addModuleLoc modu mn) False
                                ptr_wds nonptr_wds
          ; let ticky_name | actually_bound = Just binder
                           | otherwise = Nothing

          ; checkConArgsDyn (hang (text "TagCheck failed on constructor application.") 4 $
                                   text "On binder:" <> ppr binder $$ text "Constructor:" <> ppr con) con (map fromNonVoid args)
          ; hp_plus_n <- allocDynClosure ticky_name info_tbl lf_info
                                          (use_cc platform) (blame_cc platform)
                                          args_w_offsets
          ; return (mkRhsInit platform reg lf_info hp_plus_n) }
    where
      use_cc platform     -- cost-centre to stick in the object
        | isCurrentCCS ccs = cccsExpr platform
        | otherwise        = panic "buildDynCon: non-current CCS not implemented"

      blame_cc = use_cc -- cost-centre on which to blame the alloc (same)


{- Note [Precomputed static closures]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For Char/Int closures there are some value closures
built into the RTS. This is the case for all values in
the range mINT_INTLIKE .. mAX_INTLIKE (or CHARLIKE).
See Note [CHARLIKE and INTLIKE closures] in the RTS code.

Similarly zero-arity constructors have a closure
in their defining Module we can use.

If possible we prefer to refer to those existing
closure instead of building new ones.

This is true at compile time where we do this replacement
in this module.
But also at runtime where the GC does the same (but only for
INT/CHAR closures).

`precomputedStaticConInfo_maybe` checks if a given constructor application
can be replaced with a reference to a existing static closure.

If so the code will reference the existing closure when accessing
the binding.
Unless the binding is visible to other modules we also generate
no code for the binding itself. We can do this since then we can
always reference the existing closure.

See Note [About the NameSorts] for the definition of external names.
For external bindings we must still generate a closure,
but won't use it inside this module.
This can sometimes reduce cache pressure. Since:
* If somebody uses the exported binding:
  + This module will reference the existing closure.
  + GC will reference the existing closure.
  + The importing module will reference the built closure.
* If nobody uses the exported binding:
  + This module will reference the RTS closures.
  + GC references the RTS closures

In the later case we avoided loading the built closure into the cache which
is what we optimize for here.

Consider this example using Ints.

    module M(externalInt, foo, bar) where

    externalInt = 1 :: Int
    internalInt = 1 :: Int
    { -# NOINLINE foo #- }
    foo = Just internalInt :: Maybe Int
    bar = Just externalInt

    ==================== STG: ====================
    externalInt = I#! [1#];

    bar = Just! [externalInt];

    internalInt_rc = I#! [2#];

    foo = Just! [internalInt_rc];

For externally visible bindings we must generate closures
since those may be referenced by their symbol `<name>_closure`
when imported.

`externalInt` is visible to other modules so we generate a closure:

    [section ""data" . M.externalInt_closure" {
        M.externalInt_closure:
            const GHC.Types.I#_con_info;
            const 1;
    }]

It will be referenced inside this module via `M.externalInt_closure+1`

`internalInt` is however a internal name. As such we generate no code for
it. References to it are replaced with references to the static closure as
we can see in the closure built for `foo`:

    [section ""data" . M.foo_closure" {
        M.foo_closure:
            const GHC.Maybe.Just_con_info;
            const stg_INTLIKE_closure+289; // == I# 2
            const 3;
    }]

This holds for both local and top level bindings.

We don't support this optimization when compiling into Windows DLLs yet
because they don't support cross package data references well.
-}

-- (precomputedStaticConInfo_maybe cfg id con args)
--     returns (Just cg_id_info)
-- if there is a precomputed static closure for (con args).
-- In that case, cg_id_info addresses it.
-- See Note [Precomputed static closures]
precomputedStaticConInfo_maybe :: StgToCmmConfig -> Id -> DataCon -> [NonVoid StgArg] -> Maybe CgIdInfo
precomputedStaticConInfo_maybe cfg binder con []
-- Nullary constructors
  | isNullaryRepDataCon con
  = Just $ litIdInfo (stgToCmmPlatform cfg) binder (mkConLFInfo con)
                (CmmLabel (mkClosureLabel (dataConName con) NoCafRefs))
precomputedStaticConInfo_maybe cfg binder con [arg]
  -- Int/Char values with existing closures in the RTS
  | intClosure || charClosure
  , platformOS platform /= OSMinGW32 || not (stgToCmmPIE cfg || stgToCmmPIC cfg)
  , Just val <- getClosurePayload arg
  , inRange val
  = let intlike_lbl   = mkCmmClosureLabel rtsUnitId label
        val_int = fromIntegral val :: Int
        offsetW = (val_int - fromIntegral min_static_range) * (fixedHdrSizeW profile + 1)
                -- INTLIKE/CHARLIKE closures consist of a header and one word payload
        static_amode = cmmLabelOffW platform intlike_lbl offsetW
    in Just $ litIdInfo platform binder (mkConLFInfo con) static_amode
  where
    profile     = stgToCmmProfile  cfg
    platform    = stgToCmmPlatform cfg
    intClosure  = maybeIntLikeCon  con
    charClosure = maybeCharLikeCon con
    getClosurePayload (NonVoid (StgLitArg (LitNumber LitNumInt val))) = Just val
    getClosurePayload (NonVoid (StgLitArg (LitChar val))) = Just (fromIntegral . ord $ val)
    getClosurePayload _ = Nothing
    -- Avoid over/underflow by comparisons at type Integer!
    inRange :: Integer -> Bool
    inRange val
      = val >= min_static_range && val <= max_static_range

    constants = platformConstants platform

    min_static_range :: Integer
    min_static_range
      | intClosure = fromIntegral (pc_MIN_INTLIKE constants)
      | charClosure = fromIntegral (pc_MIN_CHARLIKE constants)
      | otherwise = panic "precomputedStaticConInfo_maybe: Unknown closure type"
    max_static_range
      | intClosure = fromIntegral (pc_MAX_INTLIKE constants)
      | charClosure = fromIntegral (pc_MAX_CHARLIKE constants)
      | otherwise = panic "precomputedStaticConInfo_maybe: Unknown closure type"
    label
      | intClosure = fsLit "stg_INTLIKE"
      | charClosure = fsLit "stg_CHARLIKE"
      | otherwise = panic "precomputedStaticConInfo_maybe: Unknown closure type"

precomputedStaticConInfo_maybe _ _ _ _ = Nothing

---------------------------------------------------------------
--      Binding constructor arguments
---------------------------------------------------------------

bindConArgs :: AltCon -> LocalReg -> [NonVoid Id] -> FCode [LocalReg]
-- bindConArgs is called from cgAlt of a case
-- (bindConArgs con args) augments the environment with bindings for the
-- binders args, assuming that we have just returned from a 'case' which
-- found a con
bindConArgs (DataAlt con) base args
  = assert (not (isUnboxedTupleDataCon con)) $
    do profile <- getProfile
       platform <- getPlatform
       let (_, _, args_w_offsets) = mkVirtConstrOffsets profile (addIdReps args)
           tag = tagForCon platform con

           -- The binding below forces the masking out of the tag bits
           -- when accessing the constructor field.
           bind_arg :: (NonVoid Id, ByteOff) -> FCode (Maybe LocalReg)
           bind_arg (arg@(NonVoid b), offset)
             | isDeadBinder b  -- See Note [Dead-binder optimisation] in GHC.StgToCmm.Expr
             = return Nothing
             | otherwise
             = do { emit $ mkTaggedObjectLoad platform (idToReg platform arg)
                                              base offset tag
                  ; Just <$> bindArgToReg arg }

       mapMaybeM bind_arg args_w_offsets

bindConArgs _other_con _base args
  = assert (null args ) return []

bindFakeConArg :: LocalReg -> NonVoid Id -> FCode (Maybe LocalReg)
bindFakeConArg base arg
  | NonVoid b <- arg
  , isDeadBinder b
  = return Nothing
  | otherwise
  = do profile <- getProfile
       platform <- getPlatform
       let
           -- The binding below forces the masking out of the tag bits
           -- when accessing the constructor field.
           bind_arg :: (NonVoid Id) -> FCode (Maybe LocalReg)
           bind_arg arg@(NonVoid b)
             | isDeadBinder b  -- See Note [Dead-binder optimisation] in GHC.StgToCmm.Expr
             = return Nothing
             | otherwise
             = do { target_reg <- bindArgToReg arg
                  ; emitCommentAlways $ fsLit "bindFakeJust"
                  ; emit $ mkAssign (CmmLocal target_reg)
                                    (cmmUntag platform (CmmReg $ CmmLocal base))
                  ; return $ Just target_reg
                  }

       bind_arg arg