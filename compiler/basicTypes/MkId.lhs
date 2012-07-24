%
% (c) The University of Glasgow 2006
% (c) The AQUA Project, Glasgow University, 1998
%

This module contains definitions for the IdInfo for things that
have a standard form, namely:

- data constructors
- record selectors
- method and superclass selectors
- primitive operations

\begin{code}
{-# OPTIONS -fno-warn-tabs #-}
-- The above warning supression flag is a temporary kludge.
-- While working on this module you are encouraged to remove it and
-- detab the module (please do the detabbing in a separate patch). See
--     http://hackage.haskell.org/trac/ghc/wiki/Commentary/CodingStyle#TabsvsSpaces
-- for details

module MkId (
        mkDictFunId, mkDictFunTy, mkDictSelId,

        mkDataConIds, mkPrimOpId, mkFCallId,

        mkReboxingAlt, wrapNewTypeBody, unwrapNewTypeBody,
        wrapFamInstBody, unwrapFamInstScrut,
        wrapTypeFamInstBody, unwrapTypeFamInstScrut,
        mkUnpackCase, mkProductBox,

        -- And some particular Ids; see below for why they are wired in
        wiredInIds, ghcPrimIds,
        unsafeCoerceName, unsafeCoerceId, realWorldPrimId, 
        voidArgId, nullAddrId, seqId, lazyId, lazyIdKey,
        coercionTokenId,

	-- Re-export error Ids
	module PrelRules
    ) where

#include "HsVersions.h"

import Rules
import TysPrim
import TysWiredIn
import PrelRules
import Type
import Coercion
import TcType
import MkCore
import CoreUtils	( exprType, mkCast )
import CoreUnfold
import Literal
import TyCon
import Class
import VarSet
import Name
import PrimOp
import ForeignCall
import DataCon
import Id
import Var              ( mkExportedLocalVar )
import IdInfo
import Demand
import CoreSyn
import Unique
import PrelNames
import BasicTypes       hiding ( SuccessFlag(..) )
import Util
import Pair
import DynFlags
import Outputable
import FastString
import ListSetOps

import Data.Maybe       ( maybeToList )
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Wired in Ids}
%*                                                                      *
%************************************************************************

Note [Wired-in Ids]
~~~~~~~~~~~~~~~~~~~
There are several reasons why an Id might appear in the wiredInIds:

(1) The ghcPrimIds are wired in because they can't be defined in
    Haskell at all, although the can be defined in Core.  They have
    compulsory unfoldings, so they are always inlined and they  have
    no definition site.  Their home module is GHC.Prim, so they
    also have a description in primops.txt.pp, where they are called
    'pseudoops'.

(2) The 'error' function, eRROR_ID, is wired in because we don't yet have
    a way to express in an interface file that the result type variable
    is 'open'; that is can be unified with an unboxed type

    [The interface file format now carry such information, but there's
    no way yet of expressing at the definition site for these 
    error-reporting functions that they have an 'open' 
    result type. -- sof 1/99]

(3) Other error functions (rUNTIME_ERROR_ID) are wired in (a) because
    the desugarer generates code that mentiones them directly, and
    (b) for the same reason as eRROR_ID

(4) lazyId is wired in because the wired-in version overrides the
    strictness of the version defined in GHC.Base

In cases (2-4), the function has a definition in a library module, and
can be called; but the wired-in version means that the details are 
never read from that module's interface file; instead, the full definition
is right here.

\begin{code}
wiredInIds :: [Id]
wiredInIds
  =  [lazyId]
  ++ errorIds		-- Defined in MkCore
  ++ ghcPrimIds

-- These Ids are exported from GHC.Prim
ghcPrimIds :: [Id]
ghcPrimIds
  = [   -- These can't be defined in Haskell, but they have
        -- perfectly reasonable unfoldings in Core
    realWorldPrimId,
    unsafeCoerceId,
    nullAddrId,
    seqId
    ]
\end{code}

%************************************************************************
%*                                                                      *
\subsection{Data constructors}
%*                                                                      *
%************************************************************************

The wrapper for a constructor is an ordinary top-level binding that evaluates
any strict args, unboxes any args that are going to be flattened, and calls
the worker.

We're going to build a constructor that looks like:

        data (Data a, C b) =>  T a b = T1 !a !Int b

        T1 = /\ a b -> 
             \d1::Data a, d2::C b ->
             \p q r -> case p of { p ->
                       case q of { q ->
                       Con T1 [a,b] [p,q,r]}}

Notice that

* d2 is thrown away --- a context in a data decl is used to make sure
  one *could* construct dictionaries at the site the constructor
  is used, but the dictionary isn't actually used.

* We have to check that we can construct Data dictionaries for
  the types a and Int.  Once we've done that we can throw d1 away too.

* We use (case p of q -> ...) to evaluate p, rather than "seq" because
  all that matters is that the arguments are evaluated.  "seq" is 
  very careful to preserve evaluation order, which we don't need
  to be here.

  You might think that we could simply give constructors some strictness
  info, like PrimOps, and let CoreToStg do the let-to-case transformation.
  But we don't do that because in the case of primops and functions strictness
  is a *property* not a *requirement*.  In the case of constructors we need to
  do something active to evaluate the argument.

  Making an explicit case expression allows the simplifier to eliminate
  it in the (common) case where the constructor arg is already evaluated.

Note [Wrappers for data instance tycons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the case of data instances, the wrapper also applies the coercion turning
the representation type into the family instance type to cast the result of
the wrapper.  For example, consider the declarations

  data family Map k :: * -> *
  data instance Map (a, b) v = MapPair (Map a (Pair b v))

The tycon to which the datacon MapPair belongs gets a unique internal
name of the form :R123Map, and we call it the representation tycon.
In contrast, Map is the family tycon (accessible via
tyConFamInst_maybe). A coercion allows you to move between
representation and family type.  It is accessible from :R123Map via
tyConFamilyCoercion_maybe and has kind

  Co123Map a b v :: {Map (a, b) v ~ :R123Map a b v}

The wrapper and worker of MapPair get the types

        -- Wrapper
  $WMapPair :: forall a b v. Map a (Map a b v) -> Map (a, b) v
  $WMapPair a b v = MapPair a b v `cast` sym (Co123Map a b v)

        -- Worker
  MapPair :: forall a b v. Map a (Map a b v) -> :R123Map a b v

This coercion is conditionally applied by wrapFamInstBody.

It's a bit more complicated if the data instance is a GADT as well!

   data instance T [a] where
        T1 :: forall b. b -> T [Maybe b]

Hence we translate to

        -- Wrapper
  $WT1 :: forall b. b -> T [Maybe b]
  $WT1 b v = T1 (Maybe b) b (Maybe b) v
                        `cast` sym (Co7T (Maybe b))

        -- Worker
  T1 :: forall c b. (c ~ Maybe b) => b -> :R7T c

        -- Coercion from family type to representation type
  Co7T a :: T [a] ~ :R7T a

\begin{code}
mkDataConIds :: Name -> Name -> DataCon -> DataConIds
mkDataConIds wrap_name wkr_name data_con
  | isNewTyCon tycon                    -- Newtype, only has a worker
  = DCIds Nothing nt_work_id                 

  | any isBanged all_strict_marks      -- Algebraic, needs wrapper
    || not (null eq_spec)              -- NB: LoadIface.ifaceDeclImplicitBndrs
    || isFamInstTyCon tycon            --     depends on this test
  = DCIds (Just alg_wrap_id) wrk_id

  | otherwise                                -- Algebraic, no wrapper
  = DCIds Nothing wrk_id
  where
    (univ_tvs, ex_tvs, eq_spec, 
     other_theta, orig_arg_tys, res_ty) = dataConFullSig data_con
    tycon = dataConTyCon data_con       -- The representation TyCon (not family)

        ----------- Worker (algebraic data types only) --------------
        -- The *worker* for the data constructor is the function that
        -- takes the representation arguments and builds the constructor.
    wrk_id = mkGlobalId (DataConWorkId data_con) wkr_name
                        (dataConRepType data_con) wkr_info

    wkr_arity = dataConRepArity data_con
    wkr_info  = noCafIdInfo
                `setArityInfo`       wkr_arity
                `setStrictnessInfo`  Just wkr_sig
                `setUnfoldingInfo`   evaldUnfolding  -- Record that it's evaluated,
                                                        -- even if arity = 0

    wkr_sig = mkStrictSig (mkTopDmdType (replicate wkr_arity topDmd) cpr_info)
        --      Note [Data-con worker strictness]
        -- Notice that we do *not* say the worker is strict
        -- even if the data constructor is declared strict
        --      e.g.    data T = MkT !(Int,Int)
        -- Why?  Because the *wrapper* is strict (and its unfolding has case
        -- expresssions that do the evals) but the *worker* itself is not.
        -- If we pretend it is strict then when we see
        --      case x of y -> $wMkT y
        -- the simplifier thinks that y is "sure to be evaluated" (because
        --  $wMkT is strict) and drops the case.  No, $wMkT is not strict.
        --
        -- When the simplifer sees a pattern 
        --      case e of MkT x -> ...
        -- it uses the dataConRepStrictness of MkT to mark x as evaluated;
        -- but that's fine... dataConRepStrictness comes from the data con
        -- not from the worker Id.

    cpr_info | isProductTyCon tycon && 
               isDataTyCon tycon    &&
               wkr_arity > 0        &&
               wkr_arity <= mAX_CPR_SIZE        = retCPR
             | otherwise                        = TopRes
        -- RetCPR is only true for products that are real data types;
        -- that is, not unboxed tuples or [non-recursive] newtypes

        ----------- Workers for newtypes --------------
    nt_work_id   = mkGlobalId (DataConWrapId data_con) wkr_name wrap_ty nt_work_info
    nt_work_info = noCafIdInfo          -- The NoCaf-ness is set by noCafIdInfo
                  `setArityInfo` 1      -- Arity 1
                  `setInlinePragInfo`    alwaysInlinePragma
                  `setUnfoldingInfo`     newtype_unf
    id_arg1      = mkTemplateLocal 1 (head orig_arg_tys)
    newtype_unf  = ASSERT2( isVanillaDataCon data_con &&
                            isSingleton orig_arg_tys, ppr data_con  )
			      -- Note [Newtype datacons]
                   mkCompulsoryUnfolding $ 
                   mkLams wrap_tvs $ Lam id_arg1 $ 
                   wrapNewTypeBody tycon res_ty_args (Var id_arg1)


        ----------- Wrapper --------------
        -- We used to include the stupid theta in the wrapper's args
        -- but now we don't.  Instead the type checker just injects these
        -- extra constraints where necessary.
    wrap_tvs    = (univ_tvs `minusList` map fst eq_spec) ++ ex_tvs
    res_ty_args = substTyVars (mkTopTvSubst eq_spec) univ_tvs
    ev_tys      = other_theta
    wrap_ty     = mkForAllTys wrap_tvs $ 
                  mkFunTys ev_tys $
                  mkFunTys orig_arg_tys $ res_ty

        ----------- Wrappers for algebraic data types -------------- 
    alg_wrap_id = mkGlobalId (DataConWrapId data_con) wrap_name wrap_ty alg_wrap_info
    alg_wrap_info = noCafIdInfo
                    `setArityInfo`         wrap_arity
                        -- It's important to specify the arity, so that partial
                        -- applications are treated as values
		    `setInlinePragInfo`    alwaysInlinePragma
                    `setUnfoldingInfo`     wrap_unf
                    `setStrictnessInfo` Just wrap_sig
                        -- We need to get the CAF info right here because TidyPgm
                        -- does not tidy the IdInfo of implicit bindings (like the wrapper)
                        -- so it not make sure that the CAF info is sane

    all_strict_marks = dataConExStricts data_con ++ dataConStrictMarks data_con
    wrap_sig = mkStrictSig (mkTopDmdType wrap_arg_dmds cpr_info)
    wrap_stricts = dropList eq_spec all_strict_marks
    wrap_arg_dmds = map mk_dmd wrap_stricts
    mk_dmd str | isBanged str = evalDmd
               | otherwise    = lazyDmd
        -- The Cpr info can be important inside INLINE rhss, where the
        -- wrapper constructor isn't inlined.
        -- And the argument strictness can be important too; we
        -- may not inline a contructor when it is partially applied.
        -- For example:
        --      data W = C !Int !Int !Int
        --      ...(let w = C x in ...(w p q)...)...
        -- we want to see that w is strict in its two arguments

    wrap_unf = mkInlineUnfolding (Just (length ev_args + length id_args)) wrap_rhs
    wrap_rhs = mkLams wrap_tvs $ 
               mkLams ev_args $
               mkLams id_args $
               foldr mk_case con_app 
                     (zip (ev_args ++ id_args) wrap_stricts)
                     i3 []
	     -- The ev_args is the evidence arguments *other than* the eq_spec
	     -- Because we are going to apply the eq_spec args manually in the
	     -- wrapper

    con_app _ rep_ids = wrapFamInstBody tycon res_ty_args $
                          Var wrk_id `mkTyApps`  res_ty_args
                                     `mkVarApps` ex_tvs                 
                                     `mkCoApps`  map (mkReflCo . snd) eq_spec
                                     `mkVarApps` reverse rep_ids
                            -- Dont box the eq_spec coercions since they are
                            -- marked as HsUnpack by mk_dict_strict_mark

    (ev_args,i2) = mkLocals 1  ev_tys
    (id_args,i3) = mkLocals i2 orig_arg_tys
    wrap_arity   = i3-1

    mk_case 
           :: (Id, HsBang)      -- Arg, strictness
           -> (Int -> [Id] -> CoreExpr) -- Body
           -> Int                       -- Next rep arg id
           -> [Id]                      -- Rep args so far, reversed
           -> CoreExpr
    mk_case (arg,strict) body i rep_args
          = case strict of
                HsNoBang -> body i (arg:rep_args)
                HsUnpack -> unboxProduct i (Var arg) (idType arg) the_body 
                      where
                        the_body i con_args = body i (reverse con_args ++ rep_args)
                _other  -- HsUnpackFailed and HsStrict
                   | isUnLiftedType (idType arg) -> body i (arg:rep_args)
                   | otherwise -> Case (Var arg) arg res_ty 
                                       [(DEFAULT,[], body i (arg:rep_args))]

mAX_CPR_SIZE :: Arity
mAX_CPR_SIZE = 10
-- We do not treat very big tuples as CPR-ish:
--      a) for a start we get into trouble because there aren't 
--         "enough" unboxed tuple types (a tiresome restriction, 
--         but hard to fix), 
--      b) more importantly, big unboxed tuples get returned mainly
--         on the stack, and are often then allocated in the heap
--         by the caller.  So doing CPR for them may in fact make
--         things worse.

mkLocals :: Int -> [Type] -> ([Id], Int)
mkLocals i tys = (zipWith mkTemplateLocal [i..i+n-1] tys, i+n)
               where
                 n = length tys
\end{code}

Note [Newtype datacons]
~~~~~~~~~~~~~~~~~~~~~~~
The "data constructor" for a newtype should always be vanilla.  At one
point this wasn't true, because the newtype arising from
     class C a => D a
looked like
       newtype T:D a = D:D (C a)
so the data constructor for T:C had a single argument, namely the
predicate (C a).  But now we treat that as an ordinary argument, not
part of the theta-type, so all is well.


%************************************************************************
%*                                                                      *
\subsection{Dictionary selectors}
%*                                                                      *
%************************************************************************

Selecting a field for a dictionary.  If there is just one field, then
there's nothing to do.  

Dictionary selectors may get nested forall-types.  Thus:

        class Foo a where
          op :: forall b. Ord b => a -> b -> b

Then the top-level type for op is

        op :: forall a. Foo a => 
              forall b. Ord b => 
              a -> b -> b

This is unlike ordinary record selectors, which have all the for-alls
at the outside.  When dealing with classes it's very convenient to
recover the original type signature from the class op selector.

\begin{code}
mkDictSelId :: Bool	     -- True <=> don't include the unfolding
			     -- Little point on imports without -O, because the
			     -- dictionary itself won't be visible
 	    -> Name	     -- Name of one of the *value* selectors 
	       		     -- (dictionary superclass or method)
            -> Class -> Id
mkDictSelId no_unf name clas
  = mkGlobalId (ClassOpId clas) name sel_ty info
  where
    sel_ty = mkForAllTys tyvars (mkFunTy (idType dict_id) (idType the_arg_id))
        -- We can't just say (exprType rhs), because that would give a type
        --      C a -> C a
        -- for a single-op class (after all, the selector is the identity)
        -- But it's type must expose the representation of the dictionary
        -- to get (say)         C a -> (a -> a)

    base_info = noCafIdInfo
                `setArityInfo`      1
                `setStrictnessInfo` Just strict_sig
                `setUnfoldingInfo`  (if no_unf then noUnfolding
	                             else mkImplicitUnfolding rhs)
		   -- In module where class op is defined, we must add
		   -- the unfolding, even though it'll never be inlined
		   -- becuase we use that to generate a top-level binding
		   -- for the ClassOp

    info | new_tycon = base_info `setInlinePragInfo` alwaysInlinePragma
    	   	   -- See Note [Single-method classes] in TcInstDcls
		   -- for why alwaysInlinePragma
         | otherwise = base_info  `setSpecInfo`       mkSpecInfo [rule]
		       		  `setInlinePragInfo` neverInlinePragma
		   -- Add a magic BuiltinRule, and never inline it
		   -- so that the rule is always available to fire.
		   -- See Note [ClassOp/DFun selection] in TcInstDcls

    n_ty_args = length tyvars

    -- This is the built-in rule that goes
    -- 	    op (dfT d1 d2) --->  opT d1 d2
    rule = BuiltinRule { ru_name = fsLit "Class op " `appendFS` 
    	   	       	 	     occNameFS (getOccName name)
                       , ru_fn    = name
    	               , ru_nargs = n_ty_args + 1
                       , ru_try   = dictSelRule val_index n_ty_args }

        -- The strictness signature is of the form U(AAAVAAAA) -> T
        -- where the V depends on which item we are selecting
        -- It's worth giving one, so that absence info etc is generated
        -- even if the selector isn't inlined
    strict_sig = mkStrictSig (mkTopDmdType [arg_dmd] TopRes)
    arg_dmd | new_tycon = evalDmd
            | otherwise = Eval (Prod [ if the_arg_id == id then evalDmd else Abs
                                     | id <- arg_ids ])

    tycon      	   = classTyCon clas
    new_tycon  	   = isNewTyCon tycon
    [data_con] 	   = tyConDataCons tycon
    tyvars     	   = dataConUnivTyVars data_con
    arg_tys    	   = dataConRepArgTys data_con	-- Includes the dictionary superclasses

    -- 'index' is a 0-index into the *value* arguments of the dictionary
    val_index      = assoc "MkId.mkDictSelId" sel_index_prs name
    sel_index_prs  = map idName (classAllSelIds clas) `zip` [0..]

    the_arg_id     = arg_ids !! val_index
    pred       	   = mkClassPred clas (mkTyVarTys tyvars)
    dict_id    	   = mkTemplateLocal 1 pred
    arg_ids    	   = mkTemplateLocalsNum 2 arg_tys

    rhs = mkLams tyvars  (Lam dict_id   rhs_body)
    rhs_body | new_tycon = unwrapNewTypeBody tycon (map mkTyVarTy tyvars) (Var dict_id)
             | otherwise = Case (Var dict_id) dict_id (idType the_arg_id)
                                [(DataAlt data_con, arg_ids, varToCoreExpr the_arg_id)]
				-- varToCoreExpr needed for equality superclass selectors
				--   sel a b d = case x of { MkC _ (g:a~b) _ -> CO g }

dictSelRule :: Int -> Arity 
            -> Id -> IdUnfoldingFun -> [CoreExpr] -> Maybe CoreExpr
-- Tries to persuade the argument to look like a constructor
-- application, using exprIsConApp_maybe, and then selects
-- from it
--       sel_i t1..tk (D t1..tk op1 ... opm) = opi
--
dictSelRule val_index n_ty_args _ id_unf args
  | (dict_arg : _) <- drop n_ty_args args
  , Just (_, _, con_args) <- exprIsConApp_maybe id_unf dict_arg
  = Just (con_args !! val_index)
  | otherwise
  = Nothing
\end{code}


%************************************************************************
%*                                                                      *
        Boxing and unboxing
%*                                                                      *
%************************************************************************

\begin{code}
-- unbox a product type...
-- we will recurse into newtypes, casting along the way, and unbox at the
-- first product data constructor we find. e.g.
--  
--   data PairInt = PairInt Int Int
--   newtype S = MkS PairInt
--   newtype T = MkT S
--
-- If we have e = MkT (MkS (PairInt 0 1)) and some body expecting a list of
-- ids, we get (modulo int passing)
--
--   case (e `cast` CoT) `cast` CoS of
--     PairInt a b -> body [a,b]
--
-- The Ints passed around are just for creating fresh locals
unboxProduct :: Int -> CoreExpr -> Type -> (Int -> [Id] -> CoreExpr) -> CoreExpr
unboxProduct i arg arg_ty body
  = result
  where 
    result = mkUnpackCase the_id arg con_args boxing_con rhs
    (_tycon, _tycon_args, boxing_con, tys) = deepSplitProductType "unboxProduct" arg_ty
    ([the_id], i') = mkLocals i [arg_ty]
    (con_args, i'') = mkLocals i' tys
    rhs = body i'' con_args

mkUnpackCase ::  Id -> CoreExpr -> [Id] -> DataCon -> CoreExpr -> CoreExpr
-- (mkUnpackCase x e args Con body)
--      returns
-- case (e `cast` ...) of bndr { Con args -> body }
-- 
-- the type of the bndr passed in is irrelevent
mkUnpackCase bndr arg unpk_args boxing_con body
  = Case cast_arg (setIdType bndr bndr_ty) (exprType body) [(DataAlt boxing_con, unpk_args, body)]
  where
  (cast_arg, bndr_ty) = go (idType bndr) arg
  go ty arg 
    | (tycon, tycon_args, _, _)  <- splitProductType "mkUnpackCase" ty
    , isNewTyCon tycon && not (isRecursiveTyCon tycon)
    = go (newTyConInstRhs tycon tycon_args) 
         (unwrapNewTypeBody tycon tycon_args arg)
    | otherwise = (arg, ty)

-- ...and the dual
reboxProduct :: [Unique]     -- uniques to create new local binders
             -> Type         -- type of product to box
             -> ([Unique],   -- remaining uniques
                 CoreExpr,   -- boxed product
                 [Id])       -- Ids being boxed into product
reboxProduct us ty
  = let 
        (_tycon, _tycon_args, _pack_con, con_arg_tys) = deepSplitProductType "reboxProduct" ty
 
        us' = dropList con_arg_tys us

        arg_ids  = zipWith (mkSysLocal (fsLit "rb")) us con_arg_tys

        bind_rhs = mkProductBox arg_ids ty

    in
      (us', bind_rhs, arg_ids)

mkProductBox :: [Id] -> Type -> CoreExpr
mkProductBox arg_ids ty 
  = result_expr
  where 
    (tycon, tycon_args, pack_con, _con_arg_tys) = splitProductType "mkProductBox" ty

    result_expr
      | isNewTyCon tycon && not (isRecursiveTyCon tycon) 
      = wrap (mkProductBox arg_ids (newTyConInstRhs tycon tycon_args))
      | otherwise = mkConApp pack_con (map Type tycon_args ++ varsToCoreExprs arg_ids)

    wrap expr = wrapNewTypeBody tycon tycon_args expr


-- (mkReboxingAlt us con xs rhs) basically constructs the case
-- alternative (con, xs, rhs)
-- but it does the reboxing necessary to construct the *source* 
-- arguments, xs, from the representation arguments ys.
-- For example:
--      data T = MkT !(Int,Int) Bool
--
-- mkReboxingAlt MkT [x,b] r 
--      = (DataAlt MkT, [y::Int,z::Int,b], let x = (y,z) in r)
--
-- mkDataAlt should really be in DataCon, but it can't because
-- it manipulates CoreSyn.

mkReboxingAlt
  :: [Unique] -- Uniques for the new Ids
  -> DataCon
  -> [Var]    -- Source-level args, *including* all evidence vars 
  -> CoreExpr -- RHS
  -> CoreAlt

mkReboxingAlt us con args rhs
  | not (any isMarkedUnboxed stricts)
  = (DataAlt con, args, rhs)

  | otherwise
  = let
        (binds, args') = go args stricts us
    in
    (DataAlt con, args', mkLets binds rhs)

  where
    stricts = dataConExStricts con ++ dataConStrictMarks con

    go [] _stricts _us = ([], [])

    -- Type variable case
    go (arg:args) stricts us 
      | isTyVar arg
      = let (binds, args') = go args stricts us
        in  (binds, arg:args')

        -- Term variable case
    go (arg:args) (str:stricts) us
      | isMarkedUnboxed str
      = let (binds, unpacked_args')        = go args stricts us'
            (us', bind_rhs, unpacked_args) = reboxProduct us (idType arg)
        in
            (NonRec arg bind_rhs : binds, unpacked_args ++ unpacked_args')
      | otherwise
      = let (binds, args') = go args stricts us
        in  (binds, arg:args')
    go (_ : _) [] _ = panic "mkReboxingAlt"
\end{code}


%************************************************************************
%*                                                                      *
        Wrapping and unwrapping newtypes and type families
%*                                                                      *
%************************************************************************

\begin{code}
wrapNewTypeBody :: TyCon -> [Type] -> CoreExpr -> CoreExpr
-- The wrapper for the data constructor for a newtype looks like this:
--      newtype T a = MkT (a,Int)
--      MkT :: forall a. (a,Int) -> T a
--      MkT = /\a. \(x:(a,Int)). x `cast` sym (CoT a)
-- where CoT is the coercion TyCon assoicated with the newtype
--
-- The call (wrapNewTypeBody T [a] e) returns the
-- body of the wrapper, namely
--      e `cast` (CoT [a])
--
-- If a coercion constructor is provided in the newtype, then we use
-- it, otherwise the wrap/unwrap are both no-ops 
--
-- If the we are dealing with a newtype *instance*, we have a second coercion
-- identifying the family instance with the constructor of the newtype
-- instance.  This coercion is applied in any case (ie, composed with the
-- coercion constructor of the newtype or applied by itself).

wrapNewTypeBody tycon args result_expr
  = ASSERT( isNewTyCon tycon )
    wrapFamInstBody tycon args $
    mkCast result_expr (mkSymCo co)
  where
    co = mkAxInstCo (newTyConCo tycon) args

-- When unwrapping, we do *not* apply any family coercion, because this will
-- be done via a CoPat by the type checker.  We have to do it this way as
-- computing the right type arguments for the coercion requires more than just
-- a spliting operation (cf, TcPat.tcConPat).

unwrapNewTypeBody :: TyCon -> [Type] -> CoreExpr -> CoreExpr
unwrapNewTypeBody tycon args result_expr
  = ASSERT( isNewTyCon tycon )
    mkCast result_expr (mkAxInstCo (newTyConCo tycon) args)

-- If the type constructor is a representation type of a data instance, wrap
-- the expression into a cast adjusting the expression type, which is an
-- instance of the representation type, to the corresponding instance of the
-- family instance type.
-- See Note [Wrappers for data instance tycons]
wrapFamInstBody :: TyCon -> [Type] -> CoreExpr -> CoreExpr
wrapFamInstBody tycon args body
  | Just co_con <- tyConFamilyCoercion_maybe tycon
  = mkCast body (mkSymCo (mkAxInstCo co_con args))
  | otherwise
  = body

-- Same as `wrapFamInstBody`, but for type family instances, which are
-- represented by a `CoAxiom`, and not a `TyCon`
wrapTypeFamInstBody :: CoAxiom -> [Type] -> CoreExpr -> CoreExpr
wrapTypeFamInstBody axiom args body
  = mkCast body (mkSymCo (mkAxInstCo axiom args))

unwrapFamInstScrut :: TyCon -> [Type] -> CoreExpr -> CoreExpr
unwrapFamInstScrut tycon args scrut
  | Just co_con <- tyConFamilyCoercion_maybe tycon
  = mkCast scrut (mkAxInstCo co_con args)
  | otherwise
  = scrut

unwrapTypeFamInstScrut :: CoAxiom -> [Type] -> CoreExpr -> CoreExpr
unwrapTypeFamInstScrut axiom args scrut
  = mkCast scrut (mkAxInstCo axiom args)
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Primitive operations}
%*                                                                      *
%************************************************************************

\begin{code}
mkPrimOpId :: PrimOp -> Id
mkPrimOpId prim_op 
  = id
  where
    (tyvars,arg_tys,res_ty, arity, strict_sig) = primOpSig prim_op
    ty   = mkForAllTys tyvars (mkFunTys arg_tys res_ty)
    name = mkWiredInName gHC_PRIM (primOpOcc prim_op) 
                         (mkPrimOpIdUnique (primOpTag prim_op))
                         (AnId id) UserSyntax
    id   = mkGlobalId (PrimOpId prim_op) name ty info
                
    info = noCafIdInfo
           `setSpecInfo`          mkSpecInfo (maybeToList $ primOpRules name prim_op)
           `setArityInfo`         arity
           `setStrictnessInfo` Just strict_sig

-- For each ccall we manufacture a separate CCallOpId, giving it
-- a fresh unique, a type that is correct for this particular ccall,
-- and a CCall structure that gives the correct details about calling
-- convention etc.  
--
-- The *name* of this Id is a local name whose OccName gives the full
-- details of the ccall, type and all.  This means that the interface 
-- file reader can reconstruct a suitable Id

mkFCallId :: DynFlags -> Unique -> ForeignCall -> Type -> Id
mkFCallId dflags uniq fcall ty
  = ASSERT( isEmptyVarSet (tyVarsOfType ty) )
    -- A CCallOpId should have no free type variables; 
    -- when doing substitutions won't substitute over it
    mkGlobalId (FCallId fcall) name ty info
  where
    occ_str = showSDoc dflags (braces (ppr fcall <+> ppr ty))
    -- The "occurrence name" of a ccall is the full info about the
    -- ccall; it is encoded, but may have embedded spaces etc!

    name = mkFCallName uniq occ_str

    info = noCafIdInfo
           `setArityInfo`         arity
           `setStrictnessInfo` Just strict_sig

    (_, tau)     = tcSplitForAllTys ty
    (arg_tys, _) = tcSplitFunTys tau
    arity        = length arg_tys
    strict_sig   = mkStrictSig (mkTopDmdType (replicate arity evalDmd) TopRes)
\end{code}


%************************************************************************
%*                                                                      *
\subsection{DictFuns and default methods}
%*                                                                      *
%************************************************************************

Important notes about dict funs and default methods
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Dict funs and default methods are *not* ImplicitIds.  Their definition
involves user-written code, so we can't figure out their strictness etc
based on fixed info, as we can for constructors and record selectors (say).

We build them as LocalIds, but with External Names.  This ensures that
they are taken to account by free-variable finding and dependency
analysis (e.g. CoreFVs.exprFreeVars).

Why shouldn't they be bound as GlobalIds?  Because, in particular, if
they are globals, the specialiser floats dict uses above their defns,
which prevents good simplifications happening.  Also the strictness
analyser treats a occurrence of a GlobalId as imported and assumes it
contains strictness in its IdInfo, which isn't true if the thing is
bound in the same module as the occurrence.

It's OK for dfuns to be LocalIds, because we form the instance-env to
pass on to the next module (md_insts) in CoreTidy, afer tidying
and globalising the top-level Ids.

BUT make sure they are *exported* LocalIds (mkExportedLocalId) so 
that they aren't discarded by the occurrence analyser.

\begin{code}
mkDictFunId :: Name      -- Name to use for the dict fun;
            -> [TyVar]
            -> ThetaType
            -> Class 
            -> [Type]
            -> Id
-- Implements the DFun Superclass Invariant (see TcInstDcls)

mkDictFunId dfun_name tvs theta clas tys
  = mkExportedLocalVar (DFunId n_silent is_nt)
                       dfun_name
                       dfun_ty
                       vanillaIdInfo
  where
    is_nt = isNewTyCon (classTyCon clas)
    (n_silent, dfun_ty) = mkDictFunTy tvs theta clas tys

mkDictFunTy :: [TyVar] -> ThetaType -> Class -> [Type] -> (Int, Type)
mkDictFunTy tvs theta clas tys
  = (length silent_theta, dfun_ty)
  where
    dfun_ty = mkSigmaTy tvs (silent_theta ++ theta) (mkClassPred clas tys)
    silent_theta 
      | null tvs, null theta 
      = []
      | otherwise
      = filterOut discard $
        substTheta (zipTopTvSubst (classTyVars clas) tys)
                   (classSCTheta clas)
                   -- See Note [Silent Superclass Arguments]
    discard pred = any (`eqPred` pred) theta
                 -- See the DFun Superclass Invariant in TcInstDcls
\end{code}


%************************************************************************
%*                                                                      *
\subsection{Un-definable}
%*                                                                      *
%************************************************************************

These Ids can't be defined in Haskell.  They could be defined in
unfoldings in the wired-in GHC.Prim interface file, but we'd have to
ensure that they were definitely, definitely inlined, because there is
no curried identifier for them.  That's what mkCompulsoryUnfolding
does.  If we had a way to get a compulsory unfolding from an interface
file, we could do that, but we don't right now.

unsafeCoerce# isn't so much a PrimOp as a phantom identifier, that
just gets expanded into a type coercion wherever it occurs.  Hence we
add it as a built-in Id with an unfolding here.

The type variables we use here are "open" type variables: this means
they can unify with both unlifted and lifted types.  Hence we provide
another gun with which to shoot yourself in the foot.

\begin{code}
lazyIdName, unsafeCoerceName, nullAddrName, seqName, realWorldName, coercionTokenName :: Name
unsafeCoerceName  = mkWiredInIdName gHC_PRIM (fsLit "unsafeCoerce#") unsafeCoerceIdKey  unsafeCoerceId
nullAddrName      = mkWiredInIdName gHC_PRIM (fsLit "nullAddr#")     nullAddrIdKey      nullAddrId
seqName           = mkWiredInIdName gHC_PRIM (fsLit "seq")           seqIdKey           seqId
realWorldName     = mkWiredInIdName gHC_PRIM (fsLit "realWorld#")    realWorldPrimIdKey realWorldPrimId
lazyIdName        = mkWiredInIdName gHC_BASE (fsLit "lazy")         lazyIdKey           lazyId
coercionTokenName = mkWiredInIdName gHC_PRIM (fsLit "coercionToken#") coercionTokenIdKey coercionTokenId
\end{code}

\begin{code}
------------------------------------------------
-- unsafeCoerce# :: forall a b. a -> b
unsafeCoerceId :: Id
unsafeCoerceId
  = pcMiscPrelId unsafeCoerceName ty info
  where
    info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                       `setUnfoldingInfo`  mkCompulsoryUnfolding rhs
           

    ty  = mkForAllTys [openAlphaTyVar,openBetaTyVar]
                      (mkFunTy openAlphaTy openBetaTy)
    [x] = mkTemplateLocals [openAlphaTy]
    rhs = mkLams [openAlphaTyVar,openBetaTyVar,x] $
          Cast (Var x) (mkUnsafeCo openAlphaTy openBetaTy)

------------------------------------------------
nullAddrId :: Id
-- nullAddr# :: Addr#
-- The reason is is here is because we don't provide 
-- a way to write this literal in Haskell.
nullAddrId = pcMiscPrelId nullAddrName addrPrimTy info
  where
    info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                       `setUnfoldingInfo`  mkCompulsoryUnfolding (Lit nullAddrLit)

------------------------------------------------
seqId :: Id	-- See Note [seqId magic]
seqId = pcMiscPrelId seqName ty info
  where
    info = noCafIdInfo `setInlinePragInfo` alwaysInlinePragma
                       `setUnfoldingInfo`  mkCompulsoryUnfolding rhs
                       `setSpecInfo`       mkSpecInfo [seq_cast_rule]
           

    ty  = mkForAllTys [alphaTyVar,betaTyVar]
                      (mkFunTy alphaTy (mkFunTy betaTy betaTy))
              -- NB argBetaTyVar; see Note [seqId magic]

    [x,y] = mkTemplateLocals [alphaTy, betaTy]
    rhs = mkLams [alphaTyVar,betaTyVar,x,y] (Case (Var x) x betaTy [(DEFAULT, [], Var y)])

    -- See Note [Built-in RULES for seq]
    seq_cast_rule = BuiltinRule { ru_name  = fsLit "seq of cast"
                                , ru_fn    = seqName
                                , ru_nargs = 4
                                , ru_try   = match_seq_of_cast
                                }

match_seq_of_cast :: Id -> IdUnfoldingFun -> [CoreExpr] -> Maybe CoreExpr
    -- See Note [Built-in RULES for seq]
match_seq_of_cast _ _ [Type _, Type res_ty, Cast scrut co, expr]
  = Just (Var seqId `mkApps` [Type (pFst (coercionKind co)), Type res_ty,
                              scrut, expr])
match_seq_of_cast _ _ _ = Nothing

------------------------------------------------
lazyId :: Id	-- See Note [lazyId magic]
lazyId = pcMiscPrelId lazyIdName ty info
  where
    info = noCafIdInfo
    ty  = mkForAllTys [alphaTyVar] (mkFunTy alphaTy alphaTy)
\end{code}

Note [Unsafe coerce magic]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We define a *primitive*
   GHC.Prim.unsafeCoerce#
and then in the base library we define the ordinary function
   Unsafe.Coerce.unsafeCoerce :: forall (a:*) (b:*). a -> b
   unsafeCoerce x = unsafeCoerce# x

Notice that unsafeCoerce has a civilized (albeit still dangerous)
polymorphic type, whose type args have kind *.  So you can't use it on
unboxed values (unsafeCoerce 3#).

In contrast unsafeCoerce# is even more dangerous because you *can* use
it on unboxed things, (unsafeCoerce# 3#) :: Int. Its type is
   forall (a:OpenKind) (b:OpenKind). a -> b

Note [seqId magic]
~~~~~~~~~~~~~~~~~~
'GHC.Prim.seq' is special in several ways. 

a) Its second arg can have an unboxed type
      x `seq` (v +# w)
   Hence its second type variable has ArgKind

b) Its fixity is set in LoadIface.ghcPrimIface

c) It has quite a bit of desugaring magic. 
   See DsUtils.lhs Note [Desugaring seq (1)] and (2) and (3)

d) There is some special rule handing: Note [User-defined RULES for seq]

e) See Note [Typing rule for seq] in TcExpr.

Note [User-defined RULES for seq]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Roman found situations where he had
      case (f n) of _ -> e
where he knew that f (which was strict in n) would terminate if n did.
Notice that the result of (f n) is discarded. So it makes sense to
transform to
      case n of _ -> e

Rather than attempt some general analysis to support this, I've added
enough support that you can do this using a rewrite rule:

  RULE "f/seq" forall n.  seq (f n) e = seq n e

You write that rule.  When GHC sees a case expression that discards
its result, it mentally transforms it to a call to 'seq' and looks for
a RULE.  (This is done in Simplify.rebuildCase.)  As usual, the
correctness of the rule is up to you.

To make this work, we need to be careful that the magical desugaring
done in Note [seqId magic] item (c) is *not* done on the LHS of a rule.
Or rather, we arrange to un-do it, in DsBinds.decomposeRuleLhs.

Note [Built-in RULES for seq]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We also have the following built-in rule for seq

  seq (x `cast` co) y = seq x y

This eliminates unnecessary casts and also allows other seq rules to
match more often.  Notably,     

   seq (f x `cast` co) y  -->  seq (f x) y
  
and now a user-defined rule for seq (see Note [User-defined RULES for seq])
may fire.


Note [lazyId magic]
~~~~~~~~~~~~~~~~~~~
    lazy :: forall a?. a? -> a?   (i.e. works for unboxed types too)

Used to lazify pseq:   pseq a b = a `seq` lazy b

Also, no strictness: by being a built-in Id, all the info about lazyId comes from here,
not from GHC.Base.hi.   This is important, because the strictness
analyser will spot it as strict!

Also no unfolding in lazyId: it gets "inlined" by a HACK in CorePrep.
It's very important to do this inlining *after* unfoldings are exposed 
in the interface file.  Otherwise, the unfolding for (say) pseq in the
interface file will not mention 'lazy', so if we inline 'pseq' we'll totally
miss the very thing that 'lazy' was there for in the first place.
See Trac #3259 for a real world example.

lazyId is defined in GHC.Base, so we don't *have* to inline it.  If it
appears un-applied, we'll end up just calling it.

-------------------------------------------------------------
@realWorld#@ used to be a magic literal, \tr{void#}.  If things get
nasty as-is, change it back to a literal (@Literal@).

voidArgId is a Local Id used simply as an argument in functions
where we just want an arg to avoid having a thunk of unlifted type.
E.g.
        x = \ void :: State# RealWorld -> (# p, q #)

This comes up in strictness analysis

\begin{code}
realWorldPrimId :: Id
realWorldPrimId -- :: State# RealWorld
  = pcMiscPrelId realWorldName realWorldStatePrimTy
                 (noCafIdInfo `setUnfoldingInfo` evaldUnfolding)
        -- The evaldUnfolding makes it look that realWorld# is evaluated
        -- which in turn makes Simplify.interestingArg return True,
        -- which in turn makes INLINE things applied to realWorld# likely
        -- to be inlined

voidArgId :: Id
voidArgId       -- :: State# RealWorld
  = mkSysLocal (fsLit "void") voidArgIdKey realWorldStatePrimTy

coercionTokenId :: Id 	      -- :: () ~ ()
coercionTokenId -- Used to replace Coercion terms when we go to STG
  = pcMiscPrelId coercionTokenName 
                 (mkTyConApp eqPrimTyCon [liftedTypeKind, unitTy, unitTy])
                 noCafIdInfo
\end{code}


\begin{code}
pcMiscPrelId :: Name -> Type -> IdInfo -> Id
pcMiscPrelId name ty info
  = mkVanillaGlobalWithInfo name ty info
    -- We lie and say the thing is imported; otherwise, we get into
    -- a mess with dependency analysis; e.g., core2stg may heave in
    -- random calls to GHCbase.unpackPS__.  If GHCbase is the module
    -- being compiled, then it's just a matter of luck if the definition
    -- will be in "the right place" to be in scope.
\end{code}
