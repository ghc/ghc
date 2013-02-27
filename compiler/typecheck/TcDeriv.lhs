%
% (c) The University of Glasgow 2006
% (c) The GRASP/AQUA Project, Glasgow University, 1992-1998
%

Handles @deriving@ clauses on @data@ declarations.

\begin{code}
module TcDeriv ( tcDeriving ) where

#include "HsVersions.h"

import HsSyn
import DynFlags

import TcRnMonad
import FamInst
import TcErrors( reportAllUnsolved )
import TcValidity( validDerivPred )
import TcEnv
import TcTyClsDecls( tcFamTyPats, tcAddDataFamInstCtxt )
import TcClassDcl( tcAddDeclCtxt )      -- Small helper
import TcGenDeriv                       -- Deriv stuff
import TcGenGenerics
import InstEnv
import Inst
import FamInstEnv
import TcHsType
import TcMType
import TcSimplify
import TcEvidence

import RnBinds
import RnEnv
import RnSource   ( addTcgDUs )
import HscTypes

import Id( idType )
import Class
import Type
import ErrUtils
import MkId
import DataCon
import Maybes
import RdrName
import Name
import NameSet
import TyCon
import CoAxiom
import TcType
import Var
import VarSet
import PrelNames
import SrcLoc
import Util
import ListSetOps
import Outputable
import FastString
import Bag

import Control.Monad
import Data.List
\end{code}

%************************************************************************
%*                                                                      *
                Overview
%*                                                                      *
%************************************************************************

Overall plan
~~~~~~~~~~~~
1.  Convert the decls (i.e. data/newtype deriving clauses,
    plus standalone deriving) to [EarlyDerivSpec]

2.  Infer the missing contexts for the Left DerivSpecs

3.  Add the derived bindings, generating InstInfos


\begin{code}
-- DerivSpec is purely  local to this module
data DerivSpec  = DS { ds_loc     :: SrcSpan
                     , ds_orig    :: CtOrigin
                     , ds_name    :: Name
                     , ds_tvs     :: [TyVar]
                     , ds_theta   :: ThetaType
                     , ds_cls     :: Class
                     , ds_tys     :: [Type]
                     , ds_tc      :: TyCon
                     , ds_tc_args :: [Type]
                     , ds_newtype :: Bool }
        -- This spec implies a dfun declaration of the form
        --       df :: forall tvs. theta => C tys
        -- The Name is the name for the DFun we'll build
        -- The tyvars bind all the variables in the theta
        -- For type families, the tycon in
        --       in ds_tys is the *family* tycon
        --       in ds_tc, ds_tc_args is the *representation* tycon
        -- For non-family tycons, both are the same

        -- ds_newtype = True  <=> Newtype deriving
        --              False <=> Vanilla deriving
\end{code}

Example:

     newtype instance T [a] = MkT (Tree a) deriving( C s )
==>
     axiom T [a] = :RTList a
     axiom :RTList a = Tree a

     DS { ds_tvs = [a,s], ds_cls = C, ds_tys = [s, T [a]]
        , ds_tc = :RTList, ds_tc_args = [a]
        , ds_newtype = True }

\begin{code}
type DerivContext = Maybe ThetaType
   -- Nothing    <=> Vanilla deriving; infer the context of the instance decl
   -- Just theta <=> Standalone deriving: context supplied by programmer

type EarlyDerivSpec = Either DerivSpec DerivSpec
        -- Left  ds => the context for the instance should be inferred
        --             In this case ds_theta is the list of all the
        --                constraints needed, such as (Eq [a], Eq a)
        --                The inference process is to reduce this to a
        --                simpler form (e.g. Eq a)
        --
        -- Right ds => the exact context for the instance is supplied
        --             by the programmer; it is ds_theta

pprDerivSpec :: DerivSpec -> SDoc
pprDerivSpec (DS { ds_loc = l, ds_name = n, ds_tvs = tvs,
                   ds_cls = c, ds_tys = tys, ds_theta = rhs })
  = parens (hsep [ppr l, ppr n, ppr tvs, ppr c, ppr tys]
            <+> equals <+> ppr rhs)

instance Outputable DerivSpec where
  ppr = pprDerivSpec
\end{code}


Inferring missing contexts
~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

        data T a b = C1 (Foo a) (Bar b)
                   | C2 Int (T b a)
                   | C3 (T a a)
                   deriving (Eq)

[NOTE: See end of these comments for what to do with
        data (C a, D b) => T a b = ...
]

We want to come up with an instance declaration of the form

        instance (Ping a, Pong b, ...) => Eq (T a b) where
                x == y = ...

It is pretty easy, albeit tedious, to fill in the code "...".  The
trick is to figure out what the context for the instance decl is,
namely @Ping@, @Pong@ and friends.

Let's call the context reqd for the T instance of class C at types
(a,b, ...)  C (T a b).  Thus:

        Eq (T a b) = (Ping a, Pong b, ...)

Now we can get a (recursive) equation from the @data@ decl:

        Eq (T a b) = Eq (Foo a) u Eq (Bar b)    -- From C1
                   u Eq (T b a) u Eq Int        -- From C2
                   u Eq (T a a)                 -- From C3

Foo and Bar may have explicit instances for @Eq@, in which case we can
just substitute for them.  Alternatively, either or both may have
their @Eq@ instances given by @deriving@ clauses, in which case they
form part of the system of equations.

Now all we need do is simplify and solve the equations, iterating to
find the least fixpoint.  Notice that the order of the arguments can
switch around, as here in the recursive calls to T.

Let's suppose Eq (Foo a) = Eq a, and Eq (Bar b) = Ping b.

We start with:

        Eq (T a b) = {}         -- The empty set

Next iteration:
        Eq (T a b) = Eq (Foo a) u Eq (Bar b)    -- From C1
                   u Eq (T b a) u Eq Int        -- From C2
                   u Eq (T a a)                 -- From C3

        After simplification:
                   = Eq a u Ping b u {} u {} u {}
                   = Eq a u Ping b

Next iteration:

        Eq (T a b) = Eq (Foo a) u Eq (Bar b)    -- From C1
                   u Eq (T b a) u Eq Int        -- From C2
                   u Eq (T a a)                 -- From C3

        After simplification:
                   = Eq a u Ping b
                   u (Eq b u Ping a)
                   u (Eq a u Ping a)

                   = Eq a u Ping b u Eq b u Ping a

The next iteration gives the same result, so this is the fixpoint.  We
need to make a canonical form of the RHS to ensure convergence.  We do
this by simplifying the RHS to a form in which

        - the classes constrain only tyvars
        - the list is sorted by tyvar (major key) and then class (minor key)
        - no duplicates, of course

So, here are the synonyms for the ``equation'' structures:


Note [Data decl contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider

        data (RealFloat a) => Complex a = !a :+ !a deriving( Read )

We will need an instance decl like:

        instance (Read a, RealFloat a) => Read (Complex a) where
          ...

The RealFloat in the context is because the read method for Complex is bound
to construct a Complex, and doing that requires that the argument type is
in RealFloat.

But this ain't true for Show, Eq, Ord, etc, since they don't construct
a Complex; they only take them apart.

Our approach: identify the offending classes, and add the data type
context to the instance decl.  The "offending classes" are

        Read, Enum?

FURTHER NOTE ADDED March 2002.  In fact, Haskell98 now requires that
pattern matching against a constructor from a data type with a context
gives rise to the constraints for that context -- or at least the thinned
version.  So now all classes are "offending".

Note [Newtype deriving]
~~~~~~~~~~~~~~~~~~~~~~~
Consider this:
    class C a b
    instance C [a] Char
    newtype T = T Char deriving( C [a] )

Notice the free 'a' in the deriving.  We have to fill this out to
    newtype T = T Char deriving( forall a. C [a] )

And then translate it to:
    instance C [a] Char => C [a] T where ...


Note [Newtype deriving superclasses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(See also Trac #1220 for an interesting exchange on newtype
deriving and superclasses.)

The 'tys' here come from the partial application in the deriving
clause. The last arg is the new instance type.

We must pass the superclasses; the newtype might be an instance
of them in a different way than the representation type
E.g.            newtype Foo a = Foo a deriving( Show, Num, Eq )
Then the Show instance is not done via isomorphism; it shows
        Foo 3 as "Foo 3"
The Num instance is derived via isomorphism, but the Show superclass
dictionary must the Show instance for Foo, *not* the Show dictionary
gotten from the Num dictionary. So we must build a whole new dictionary
not just use the Num one.  The instance we want is something like:
     instance (Num a, Show (Foo a), Eq (Foo a)) => Num (Foo a) where
        (+) = ((+)@a)
        ...etc...
There may be a coercion needed which we get from the tycon for the newtype
when the dict is constructed in TcInstDcls.tcInstDecl2


Note [Unused constructors and deriving clauses]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See Trac #3221.  Consider
   data T = T1 | T2 deriving( Show )
Are T1 and T2 unused?  Well, no: the deriving clause expands to mention
both of them.  So we gather defs/uses from deriving just like anything else.

%************************************************************************
%*                                                                      *
\subsection[TcDeriv-driver]{Top-level function for \tr{derivings}}
%*                                                                      *
%************************************************************************

\begin{code}
tcDeriving  :: [LTyClDecl Name]  -- All type constructors
            -> [LInstDecl Name]  -- All instance declarations
            -> [LDerivDecl Name] -- All stand-alone deriving declarations
            -> TcM (TcGblEnv, Bag (InstInfo Name), HsValBinds Name)
tcDeriving tycl_decls inst_decls deriv_decls
  = recoverM (do { g <- getGblEnv
                 ; return (g, emptyBag, emptyValBindsOut)}) $
    do  {       -- Fish the "deriving"-related information out of the TcEnv
                -- And make the necessary "equations".
          is_boot <- tcIsHsBoot
        ; traceTc "tcDeriving" (ppr is_boot)

        -- If -XAutoDeriveTypeable is on, add Typeable instances for each
        -- datatype and class defined in this module
        ; isAutoDeriveTypeable <- xoptM Opt_AutoDeriveTypeable
        ; let deriv_decls' = deriv_decls ++ if isAutoDeriveTypeable
                                              then deriveTypeable tycl_decls
                                              else []

        ; early_specs <- makeDerivSpecs is_boot tycl_decls inst_decls deriv_decls'

        -- for each type, determine the auxliary declarations that are common
        -- to multiple derivations involving that type (e.g. Generic and
        -- Generic1 should use the same TcGenGenerics.MetaTyCons)
        ; (commonAuxs, auxDerivStuff) <- commonAuxiliaries $ map (either id id) early_specs

        ; overlap_flag <- getOverlapFlag
        ; let (infer_specs, given_specs) = splitEithers early_specs
        ; insts1 <- mapM (genInst True overlap_flag commonAuxs) given_specs

        -- the stand-alone derived instances (@insts1@) are used when inferring
        -- the contexts for "deriving" clauses' instances (@infer_specs@)
        ; final_specs <- extendLocalInstEnv (map (iSpec . fst) insts1) $
                         inferInstanceContexts overlap_flag infer_specs

        ; insts2 <- mapM (genInst False overlap_flag commonAuxs) final_specs

        ; let (inst_infos, deriv_stuff) = unzip (insts1 ++ insts2)
        ; loc <- getSrcSpanM
        ; let (binds, newTyCons, famInsts, extraInstances) =
                genAuxBinds loc (unionManyBags (auxDerivStuff : deriv_stuff))

        ; (inst_info, rn_binds, rn_dus) <-
            renameDeriv is_boot (inst_infos ++ (bagToList extraInstances)) binds

        ; dflags <- getDynFlags
        ; unless (isEmptyBag inst_info) $
            liftIO (dumpIfSet_dyn dflags Opt_D_dump_deriv "Derived instances"
                   (ddump_deriving inst_info rn_binds newTyCons famInsts))

        ; let all_tycons = map ATyCon (bagToList newTyCons)
        ; gbl_env <- tcExtendGlobalEnv all_tycons $
                     tcExtendGlobalEnvImplicit (concatMap implicitTyThings all_tycons) $
                     tcExtendLocalFamInstEnv (bagToList famInsts) $
                     tcExtendLocalInstEnv (map iSpec (bagToList inst_info)) getGblEnv

        ; return (addTcgDUs gbl_env rn_dus, inst_info, rn_binds) }
  where
    ddump_deriving :: Bag (InstInfo Name) -> HsValBinds Name
                   -> Bag TyCon                 -- ^ Empty data constructors
                   -> Bag (FamInst Unbranched)  -- ^ Rep type family instances
                   -> SDoc
    ddump_deriving inst_infos extra_binds repMetaTys repFamInsts
      =    hang (ptext (sLit "Derived instances:"))
              2 (vcat (map (\i -> pprInstInfoDetails i $$ text "") (bagToList inst_infos))
                 $$ ppr extra_binds)
        $$ hangP "Generic representation:" (
              hangP "Generated datatypes for meta-information:"
               (vcat (map ppr (bagToList repMetaTys)))
           $$ hangP "Representation types:"
                (vcat (map pprRepTy (bagToList repFamInsts))))

    hangP s x = text "" $$ hang (ptext (sLit s)) 2 x

    deriveTypeable :: [LTyClDecl Name] -> [LDerivDecl Name]
    deriveTypeable tys =
      [ L l (DerivDecl (L l (HsAppTy (noLoc (HsTyVar typeableClassName))
                                     (L l (HsTyVar (tcdName t))))))
      | L l t <- tys, not (isSynDecl t), not (isTypeFamilyDecl t) ]

-- Prints the representable type family instance
pprRepTy :: FamInst Unbranched -> SDoc
pprRepTy fi@(FamInst { fi_branches = FirstBranch (FamInstBranch { fib_lhs = lhs
                                                                , fib_rhs = rhs }) })
  = ptext (sLit "type") <+> ppr (mkTyConApp (famInstTyCon fi) lhs) <+>
      equals <+> ppr rhs 


-- As of 24 April 2012, this only shares MetaTyCons between derivations of
-- Generic and Generic1; thus the types and logic are quite simple.
type CommonAuxiliary = MetaTyCons
type CommonAuxiliaries = [(TyCon, CommonAuxiliary)] -- NSF what is a more efficient map type?
commonAuxiliaries :: [DerivSpec] -> TcM (CommonAuxiliaries, BagDerivStuff)
commonAuxiliaries = foldM snoc ([], emptyBag) where
  snoc acc@(cas, stuff) (DS {ds_name = nm, ds_cls = cls, ds_tc = rep_tycon})
    | getUnique cls `elem` [genClassKey, gen1ClassKey] =
      extendComAux $ genGenericMetaTyCons rep_tycon (nameModule nm)
    | otherwise = return acc
   where extendComAux m -- don't run m if its already in the accumulator
           | any ((rep_tycon ==) . fst) cas = return acc
           | otherwise = do (ca, new_stuff) <- m
                            return $ ((rep_tycon, ca) : cas, stuff `unionBags` new_stuff)

renameDeriv :: Bool
            -> [InstInfo RdrName]
            -> Bag (LHsBind RdrName, LSig RdrName)
            -> TcM (Bag (InstInfo Name), HsValBinds Name, DefUses)
renameDeriv is_boot inst_infos bagBinds
  | is_boot     -- If we are compiling a hs-boot file, don't generate any derived bindings
                -- The inst-info bindings will all be empty, but it's easier to
                -- just use rn_inst_info to change the type appropriately
  = do  { (rn_inst_infos, fvs) <- mapAndUnzipM rn_inst_info inst_infos
        ; return ( listToBag rn_inst_infos
                 , emptyValBindsOut, usesOnly (plusFVs fvs)) }

  | otherwise
  = discardWarnings $         -- Discard warnings about unused bindings etc
    setXOptM Opt_EmptyCase $  -- Derived decls (for empty types) can have 
                              --    case x of {}
    do  {
        -- Bring the extra deriving stuff into scope
        -- before renaming the instances themselves
        ; (aux_binds, aux_sigs) <- mapAndUnzipBagM return bagBinds
        ; let aux_val_binds = ValBindsIn aux_binds (bagToList aux_sigs)
        ; rn_aux_lhs <- rnTopBindsLHS emptyFsEnv aux_val_binds
        ; let bndrs = collectHsValBinders rn_aux_lhs
        ; bindLocalNames bndrs $
    do  { (rn_aux, dus_aux) <- rnValBindsRHS (LocalBindCtxt (mkNameSet bndrs)) rn_aux_lhs
        ; (rn_inst_infos, fvs_insts) <- mapAndUnzipM rn_inst_info inst_infos
        ; return (listToBag rn_inst_infos, rn_aux,
                  dus_aux `plusDU` usesOnly (plusFVs fvs_insts)) } }

  where
    rn_inst_info :: InstInfo RdrName -> TcM (InstInfo Name, FreeVars)
    rn_inst_info info@(InstInfo { iBinds = NewTypeDerived coi tc })
        = return ( info { iBinds = NewTypeDerived coi tc }
                 , mkFVs (map dataConName (tyConDataCons tc)))
          -- See Note [Newtype deriving and unused constructors]

    rn_inst_info inst_info@(InstInfo { iSpec = inst, iBinds = VanillaInst binds sigs standalone_deriv })
        =       -- Bring the right type variables into
                -- scope (yuk), and rename the method binds
           ASSERT( null sigs )
           bindLocalNames (map Var.varName tyvars) $
           do { (rn_binds, fvs) <- rnMethodBinds (is_cls_nm inst) (\_ -> []) binds
              ; let binds' = VanillaInst rn_binds [] standalone_deriv
              ; return (inst_info { iBinds = binds' }, fvs) }
        where
          (tyvars, _) = tcSplitForAllTys (idType (instanceDFunId inst))
\end{code}

Note [Newtype deriving and unused constructors]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (see Trac #1954):

  module Bug(P) where
  newtype P a = MkP (IO a) deriving Monad

If you compile with -fwarn-unused-binds you do not expect the warning
"Defined but not used: data consructor MkP". Yet the newtype deriving
code does not explicitly mention MkP, but it should behave as if you
had written
  instance Monad P where
     return x = MkP (return x)
     ...etc...

So we want to signal a user of the data constructor 'MkP'.  That's
what we do in rn_inst_info, and it's the only reason we have the TyCon
stored in NewTypeDerived.


%************************************************************************
%*                                                                      *
                From HsSyn to DerivSpec
%*                                                                      *
%************************************************************************

@makeDerivSpecs@ fishes around to find the info about needed derived instances.

\begin{code}
makeDerivSpecs :: Bool
               -> [LTyClDecl Name]
               -> [LInstDecl Name]
               -> [LDerivDecl Name]
               -> TcM [EarlyDerivSpec]
makeDerivSpecs is_boot tycl_decls inst_decls deriv_decls
  = do  { eqns1 <- concatMapM (recoverM (return []) . deriveTyDecl) tycl_decls
        ; eqns2 <- concatMapM (recoverM (return []) . deriveInstDecl) inst_decls
        ; eqns3 <- mapAndRecoverM deriveStandalone deriv_decls
        ; let eqns = eqns1 ++ eqns2 ++ eqns3
        ; if is_boot then   -- No 'deriving' at all in hs-boot files
              do { unless (null eqns) (add_deriv_err (head eqns))
                 ; return [] }
          else return eqns }
  where
    add_deriv_err eqn
       = setSrcSpan loc $
         addErr (hang (ptext (sLit "Deriving not permitted in hs-boot file"))
                    2 (ptext (sLit "Use an instance declaration instead")))
       where
         loc = case eqn of  { Left ds -> ds_loc ds; Right ds -> ds_loc ds }

------------------------------------------------------------------
deriveTyDecl :: LTyClDecl Name -> TcM [EarlyDerivSpec]
deriveTyDecl (L _ decl@(DataDecl { tcdLName = L _ tc_name
                                 , tcdDataDefn = HsDataDefn { dd_derivs = Just preds } }))
  = tcAddDeclCtxt decl $
    do { tc <- tcLookupTyCon tc_name
       ; let tvs = tyConTyVars tc
             tys = mkTyVarTys tvs
       ; mapM (deriveTyData tvs tc tys) preds }

deriveTyDecl _ = return []

------------------------------------------------------------------
deriveInstDecl :: LInstDecl Name -> TcM [EarlyDerivSpec]
deriveInstDecl (L _ (TyFamInstD {})) = return []
deriveInstDecl (L _ (DataFamInstD { dfid_inst = fam_inst }))
  = deriveFamInst fam_inst
deriveInstDecl (L _ (ClsInstD { cid_inst = ClsInstDecl { cid_datafam_insts = fam_insts } }))
  = concatMapM (deriveFamInst . unLoc) fam_insts

------------------------------------------------------------------
deriveFamInst :: DataFamInstDecl Name -> TcM [EarlyDerivSpec]
deriveFamInst decl@(DataFamInstDecl { dfid_tycon = L _ tc_name, dfid_pats = pats
                                    , dfid_defn = HsDataDefn { dd_derivs = Just preds } })
  = tcAddDataFamInstCtxt decl $
    do { fam_tc <- tcLookupTyCon tc_name
       ; tcFamTyPats fam_tc pats (\_ -> return ()) $ \ tvs' pats' _ ->
         mapM (deriveTyData tvs' fam_tc pats') preds }
        -- Tiresomely we must figure out the "lhs", which is awkward for type families
        -- E.g.   data T a b = .. deriving( Eq )
        --          Here, the lhs is (T a b)
        --        data instance TF Int b = ... deriving( Eq )
        --          Here, the lhs is (TF Int b)
        -- But if we just look up the tycon_name, we get is the *family*
        -- tycon, but not pattern types -- they are in the *rep* tycon.

deriveFamInst _ = return []

------------------------------------------------------------------
deriveStandalone :: LDerivDecl Name -> TcM EarlyDerivSpec
-- Standalone deriving declarations
--  e.g.   deriving instance Show a => Show (T a)
-- Rather like tcLocalInstDecl
deriveStandalone (L loc (DerivDecl deriv_ty))
  = setSrcSpan loc                   $
    addErrCtxt (standaloneCtxt deriv_ty)  $
    do { traceTc "Standalone deriving decl for" (ppr deriv_ty)
       ; (tvs, theta, cls, inst_tys) <- tcHsInstHead TcType.InstDeclCtxt deriv_ty
       ; traceTc "Standalone deriving;" $ vcat
              [ text "tvs:" <+> ppr tvs
              , text "theta:" <+> ppr theta
              , text "cls:" <+> ppr cls
              , text "tys:" <+> ppr inst_tys ]
                -- C.f. TcInstDcls.tcLocalInstDecl1

       ; let cls_tys = take (length inst_tys - 1) inst_tys
             inst_ty = last inst_tys
       ; traceTc "Standalone deriving:" $ vcat
              [ text "class:" <+> ppr cls
              , text "class types:" <+> ppr cls_tys
              , text "type:" <+> ppr inst_ty ]
       ; mkEqnHelp StandAloneDerivOrigin tvs cls cls_tys inst_ty
                   (Just theta) }

------------------------------------------------------------------
deriveTyData :: [TyVar] -> TyCon -> [Type]
             -> LHsType Name           -- The deriving predicate
             -> TcM EarlyDerivSpec
-- The deriving clause of a data or newtype declaration
deriveTyData tvs tc tc_args (L loc deriv_pred)
  = setSrcSpan loc     $        -- Use the location of the 'deriving' item
    tcExtendTyVarEnv tvs $      -- Deriving preds may (now) mention
                                -- the type variables for the type constructor

    do  { (deriv_tvs, cls, cls_tys) <- tcHsDeriv deriv_pred
                -- The "deriv_pred" is a LHsType to take account of the fact that for
                -- newtype deriving we allow deriving (forall a. C [a]).

                -- Typeable is special
        ; if className cls == typeableClassName
          then mkEqnHelp DerivOrigin
                  (varSetElemsKvsFirst (mkVarSet tvs `extendVarSetList` deriv_tvs))
                  cls cls_tys (mkTyConApp tc tc_args) Nothing
          else do {

        -- Given data T a b c = ... deriving( C d ),
        -- we want to drop type variables from T so that (C d (T a)) is well-kinded
        ; let cls_tyvars     = classTyVars cls
              kind           = tyVarKind (last cls_tyvars)
              (arg_kinds, _) = splitKindFunTys kind
              n_args_to_drop = length arg_kinds
              n_args_to_keep = tyConArity tc - n_args_to_drop
              args_to_drop   = drop n_args_to_keep tc_args
              inst_ty        = mkTyConApp tc (take n_args_to_keep tc_args)
              inst_ty_kind   = typeKind inst_ty
              dropped_tvs    = mkVarSet (mapCatMaybes getTyVar_maybe args_to_drop)
              univ_tvs       = (mkVarSet tvs `extendVarSetList` deriv_tvs)
                                             `minusVarSet` dropped_tvs

        ; traceTc "derivTyData" (pprTvBndrs tvs $$ ppr tc $$ ppr tc_args $$
                     pprTvBndrs (varSetElems $ tyVarsOfTypes tc_args) $$ ppr inst_ty)

        -- Check that the result really is well-kinded
        ; checkTc (n_args_to_keep >= 0 && (inst_ty_kind `eqKind` kind))
                  (derivingKindErr tc cls cls_tys kind)

        ; checkTc (sizeVarSet dropped_tvs == n_args_to_drop &&           -- (a)
                   tyVarsOfTypes (inst_ty:cls_tys) `subVarSet` univ_tvs) -- (b)
                  (derivingEtaErr cls cls_tys inst_ty)
                -- Check that
                --  (a) The data type can be eta-reduced; eg reject:
                --              data instance T a a = ... deriving( Monad )
                --  (b) The type class args do not mention any of the dropped type
                --      variables
                --              newtype T a s = ... deriving( ST s )

        -- Type families can't be partially applied
        -- e.g.   newtype instance T Int a = MkT [a] deriving( Monad )
        -- Note [Deriving, type families, and partial applications]
        ; checkTc (not (isFamilyTyCon tc) || n_args_to_drop == 0)
                  (typeFamilyPapErr tc cls cls_tys inst_ty)

        ; mkEqnHelp DerivOrigin (varSetElemsKvsFirst univ_tvs) cls cls_tys inst_ty Nothing } }
\end{code}

Note [Deriving, type families, and partial applications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When there are no type families, it's quite easy:

    newtype S a = MkS [a]
    -- :CoS :: S  ~ []  -- Eta-reduced

    instance Eq [a] => Eq (S a)         -- by coercion sym (Eq (:CoS a)) : Eq [a] ~ Eq (S a)
    instance Monad [] => Monad S        -- by coercion sym (Monad :CoS)  : Monad [] ~ Monad S

When type familes are involved it's trickier:

    data family T a b
    newtype instance T Int a = MkT [a] deriving( Eq, Monad )
    -- :RT is the representation type for (T Int a)
    --  :CoF:R1T a :: T Int a ~ :RT a   -- Not eta reduced
    --  :Co:R1T    :: :RT ~ []          -- Eta-reduced

    instance Eq [a] => Eq (T Int a)     -- easy by coercion
    instance Monad [] => Monad (T Int)  -- only if we can eta reduce???

The "???" bit is that we don't build the :CoF thing in eta-reduced form
Henc the current typeFamilyPapErr, even though the instance makes sense.
After all, we can write it out
    instance Monad [] => Monad (T Int)  -- only if we can eta reduce???
      return x = MkT [x]
      ... etc ...

\begin{code}
mkEqnHelp :: CtOrigin -> [TyVar] -> Class -> [Type] -> Type
          -> DerivContext       -- Just    => context supplied (standalone deriving)
                                -- Nothing => context inferred (deriving on data decl)
          -> TcRn EarlyDerivSpec
-- Make the EarlyDerivSpec for an instance
--      forall tvs. theta => cls (tys ++ [ty])
-- where the 'theta' is optional (that's the Maybe part)
-- Assumes that this declaration is well-kinded

mkEqnHelp orig tvs cls cls_tys tc_app mtheta
  | Just (tycon, tc_args) <- tcSplitTyConApp_maybe tc_app
  , isAlgTyCon tycon    -- Check for functions, primitive types etc
  = mk_alg_eqn tycon tc_args
  | otherwise
  = failWithTc (derivingThingErr False cls cls_tys tc_app
               (ptext (sLit "The last argument of the instance must be a data or newtype application")))

  where
     bale_out msg = failWithTc (derivingThingErr False cls cls_tys tc_app msg)

     mk_alg_eqn tycon tc_args
      | className cls `elem` oldTypeableClassNames
      = do { dflags <- getDynFlags
           ; case checkOldTypeableConditions (dflags, tycon, tc_args) of
               Just err -> bale_out err
               Nothing  -> mk_old_typeable_eqn orig tvs cls tycon tc_args mtheta }

      | className cls == typeableClassName
      = do { dflags <- getDynFlags
           ; case checkTypeableConditions (dflags, tycon, tc_args) of
               Just err -> bale_out err
               Nothing  -> mk_typeable_eqn orig tvs cls tycon tc_args mtheta }

      | isDataFamilyTyCon tycon
      , length tc_args /= tyConArity tycon
      = bale_out (ptext (sLit "Unsaturated data family application"))

      | otherwise
      = do { (rep_tc, rep_tc_args) <- tcLookupDataFamInst tycon tc_args
                  -- Be careful to test rep_tc here: in the case of families,
                  -- we want to check the instance tycon, not the family tycon

           -- For standalone deriving (mtheta /= Nothing),
           -- check that all the data constructors are in scope.
           ; rdr_env <- getGlobalRdrEnv
           ; let hidden_data_cons = not (isWiredInName (tyConName rep_tc)) &&
                                    (isAbstractTyCon rep_tc ||
                                     any not_in_scope (tyConDataCons rep_tc))
                 not_in_scope dc  = null (lookupGRE_Name rdr_env (dataConName dc))
           ; unless (isNothing mtheta || not hidden_data_cons)
                    (bale_out (derivingHiddenErr tycon))

           ; dflags <- getDynFlags
           ; if isDataTyCon rep_tc then
                mkDataTypeEqn orig dflags tvs cls cls_tys
                              tycon tc_args rep_tc rep_tc_args mtheta
             else
                mkNewTypeEqn orig dflags tvs cls cls_tys
                             tycon tc_args rep_tc rep_tc_args mtheta }
\end{code}


%************************************************************************
%*                                                                      *
                Deriving data types
%*                                                                      *
%************************************************************************

\begin{code}
mkDataTypeEqn :: CtOrigin
              -> DynFlags
              -> [Var]                  -- Universally quantified type variables in the instance
              -> Class                  -- Class for which we need to derive an instance
              -> [Type]                 -- Other parameters to the class except the last
              -> TyCon                  -- Type constructor for which the instance is requested
                                        --    (last parameter to the type class)
              -> [Type]                 -- Parameters to the type constructor
              -> TyCon                  -- rep of the above (for type families)
              -> [Type]                 -- rep of the above
              -> DerivContext        -- Context of the instance, for standalone deriving
              -> TcRn EarlyDerivSpec    -- Return 'Nothing' if error

mkDataTypeEqn orig dflags tvs cls cls_tys
              tycon tc_args rep_tc rep_tc_args mtheta
  = case checkSideConditions dflags mtheta cls cls_tys rep_tc rep_tc_args of
        -- NB: pass the *representation* tycon to checkSideConditions
        CanDerive               -> go_for_it
        NonDerivableClass       -> bale_out (nonStdErr cls)
        DerivableClassError msg -> bale_out msg
  where
    go_for_it    = mk_data_eqn orig tvs cls tycon tc_args rep_tc rep_tc_args mtheta
    bale_out msg = failWithTc (derivingThingErr False cls cls_tys (mkTyConApp tycon tc_args) msg)

mk_data_eqn :: CtOrigin -> [TyVar] -> Class
            -> TyCon -> [TcType] -> TyCon -> [TcType] -> DerivContext
            -> TcM EarlyDerivSpec
mk_data_eqn orig tvs cls tycon tc_args rep_tc rep_tc_args mtheta
  = do  { loc                  <- getSrcSpanM
        ; dfun_name            <- new_dfun_name cls tycon
        ; inferred_constraints <- inferConstraints cls inst_tys rep_tc rep_tc_args
        ; let spec = DS { ds_loc = loc, ds_orig = orig
                        , ds_name = dfun_name, ds_tvs = tvs
                        , ds_cls = cls, ds_tys = inst_tys
                        , ds_tc = rep_tc, ds_tc_args = rep_tc_args
                        , ds_theta =  mtheta `orElse` inferred_constraints
                        , ds_newtype = False }

        ; return (if isJust mtheta then Right spec      -- Specified context
                                   else Left spec) }    -- Infer context
  where
    inst_tys = [mkTyConApp tycon tc_args]

----------------------
mk_old_typeable_eqn :: CtOrigin -> [TyVar] -> Class
                    -> TyCon -> [TcType] -> DerivContext
                    -> TcM EarlyDerivSpec
mk_old_typeable_eqn orig tvs cls tycon tc_args mtheta
        -- The Typeable class is special in several ways
        --        data T a b = ... deriving( Typeable )
        -- gives
        --        instance Typeable2 T where ...
        -- Notice that:
        -- 1. There are no constraints in the instance
        -- 2. There are no type variables either
        -- 3. The actual class we want to generate isn't necessarily
        --      Typeable; it depends on the arity of the type
  | isNothing mtheta    -- deriving on a data type decl
  = do  { checkTc (cls `hasKey` oldTypeableClassKey)
                  (ptext (sLit "Use deriving( Typeable ) on a data type declaration"))
        ; real_cls <- tcLookupClass (oldTypeableClassNames `getNth` tyConArity tycon)
                      -- See Note [Getting base classes]
        ; mk_old_typeable_eqn orig tvs real_cls tycon [] (Just []) }

  | otherwise           -- standalone deriving
  = do  { checkTc (null tc_args)
                  (ptext (sLit "Derived typeable instance must be of form (Typeable")
                        <> int (tyConArity tycon) <+> ppr tycon <> rparen)
        ; dfun_name <- new_dfun_name cls tycon
        ; loc <- getSrcSpanM
        ; return (Right $
                  DS { ds_loc = loc, ds_orig = orig, ds_name = dfun_name, ds_tvs = []
                     , ds_cls = cls, ds_tys = [mkTyConApp tycon []]
                     , ds_tc = tycon, ds_tc_args = []
                     , ds_theta = mtheta `orElse` [], ds_newtype = False })  }

mk_typeable_eqn :: CtOrigin -> [TyVar] -> Class
                -> TyCon -> [TcType] -> DerivContext
                -> TcM EarlyDerivSpec
mk_typeable_eqn orig tvs cls tycon tc_args mtheta
  -- The kind-polymorphic Typeable class is less special; namely, there is no
  -- need to select the class with the right kind anymore, as we only have one.
  | isNothing mtheta    -- deriving on a data type decl
  = mk_typeable_eqn orig tvs cls tycon [] (Just [])

  | otherwise -- standalone deriving
  = do  { checkTc (null tc_args)
                  (ptext (sLit "Derived typeable instance must be of form (Typeable")
                        <+> ppr tycon <> rparen)
        ; dfun_name <- new_dfun_name cls tycon
        ; loc <- getSrcSpanM
        ; return (Right $
                  DS { ds_loc = loc, ds_orig = orig, ds_name = dfun_name, ds_tvs = []
                     , ds_cls = cls, ds_tys = tyConKind tycon : [mkTyConApp tycon []]
                     , ds_tc = tycon, ds_tc_args = []
                     , ds_theta = mtheta `orElse` [], ds_newtype = False })  }

----------------------
inferConstraints :: Class -> [TcType]
                 -> TyCon -> [TcType]
                 -> TcM ThetaType
-- Generate a sufficiently large set of constraints that typechecking the
-- generated method definitions should succeed.   This set will be simplified
-- before being used in the instance declaration
inferConstraints cls inst_tys rep_tc rep_tc_args
  | cls `hasKey` genClassKey    -- Generic constraints are easy
  = return []

  | cls `hasKey` gen1ClassKey   -- Gen1 needs Functor
  = ASSERT (length rep_tc_tvs > 0)   -- See Note [Getting base classes]
    do { functorClass <- tcLookupClass functorClassName
       ; return (con_arg_constraints functorClass (get_gen1_constrained_tys last_tv)) }

  | otherwise  -- The others are a bit more complicated
  = ASSERT2( equalLength rep_tc_tvs all_rep_tc_args, ppr cls <+> ppr rep_tc )
    return (stupid_constraints ++ extra_constraints
            ++ sc_constraints
            ++ con_arg_constraints cls get_std_constrained_tys)

  where
       -- Constraints arising from the arguments of each constructor
    con_arg_constraints cls' get_constrained_tys
      = [ mkClassPred cls' [arg_ty]
        | data_con <- tyConDataCons rep_tc,
          arg_ty   <- ASSERT( isVanillaDataCon data_con )
                        get_constrained_tys $
                        dataConInstOrigArgTys data_con all_rep_tc_args,
          not (isUnLiftedType arg_ty) ]
                -- No constraints for unlifted types
                -- See Note [Deriving and unboxed types]

                -- For functor-like classes, two things are different
                -- (a) We recurse over argument types to generate constraints
                --     See Functor examples in TcGenDeriv
                -- (b) The rep_tc_args will be one short
    is_functor_like = getUnique cls `elem` functorLikeClassKeys

    get_std_constrained_tys :: [Type] -> [Type]
    get_std_constrained_tys tys
        | is_functor_like = concatMap (deepSubtypesContaining last_tv) tys
        | otherwise       = tys

    rep_tc_tvs = tyConTyVars rep_tc
    last_tv = last rep_tc_tvs
    all_rep_tc_args | cls `hasKey` gen1ClassKey || is_functor_like
                      = rep_tc_args ++ [mkTyVarTy last_tv]
                    | otherwise       = rep_tc_args

        -- Constraints arising from superclasses
        -- See Note [Superclasses of derived instance]
    sc_constraints = substTheta (zipOpenTvSubst (classTyVars cls) inst_tys)
                                (classSCTheta cls)

        -- Stupid constraints
    stupid_constraints = substTheta subst (tyConStupidTheta rep_tc)
    subst = zipTopTvSubst rep_tc_tvs all_rep_tc_args

        -- Extra Data constraints
        -- The Data class (only) requires that for
        --    instance (...) => Data (T t1 t2)
        -- IF   t1:*, t2:*
        -- THEN (Data t1, Data t2) are among the (...) constraints
        -- Reason: when the IF holds, we generate a method
        --             dataCast2 f = gcast2 f
        --         and we need the Data constraints to typecheck the method
    extra_constraints
      | cls `hasKey` dataClassKey
      , all (isLiftedTypeKind . typeKind) rep_tc_args
      = [mkClassPred cls [ty] | ty <- rep_tc_args]
      | otherwise
      = []
\end{code}

Note [Getting base classes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Functor and Typeable are defined in package 'base', and that is not available
when compiling 'ghc-prim'.  So we must be careful that 'deriving' for stuff in
ghc-prim does not use Functor or Typeable implicitly via these lookups.

Note [Deriving and unboxed types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have some special hacks to support things like
   data T = MkT Int# deriving( Ord, Show )

Specifically
  * For Show we use TcGenDeriv.box_if_necy to box the Int# into an Int
    (which we know how to show)

  * For Eq, Ord, we ust TcGenDeriv.primOrdOps to give Ord operations
    on some primitive types

It's all a bit ad hoc.


\begin{code}
------------------------------------------------------------------
-- Check side conditions that dis-allow derivability for particular classes
-- This is *apart* from the newtype-deriving mechanism
--
-- Here we get the representation tycon in case of family instances as it has
-- the data constructors - but we need to be careful to fall back to the
-- family tycon (with indexes) in error messages.

data DerivStatus = CanDerive
                 | DerivableClassError SDoc  -- Standard class, but can't do it
                 | NonDerivableClass         -- Non-standard class

checkSideConditions :: DynFlags -> DerivContext -> Class -> [TcType]
                    -> TyCon -> [Type] -- tycon and its parameters
                    -> DerivStatus
checkSideConditions dflags mtheta cls cls_tys rep_tc rep_tc_args
  | Just cond <- sideConditions mtheta cls
  = case (cond (dflags, rep_tc, rep_tc_args)) of
        Just err -> DerivableClassError err     -- Class-specific error
        Nothing  | null cls_tys -> CanDerive    -- All derivable classes are unary, so
                                                -- cls_tys (the type args other than last)
                                                -- should be null
                 | otherwise    -> DerivableClassError ty_args_why      -- e.g. deriving( Eq s )
  | otherwise = NonDerivableClass       -- Not a standard class
  where
    ty_args_why = quotes (ppr (mkClassPred cls cls_tys)) <+> ptext (sLit "is not a class")

checkTypeableConditions, checkOldTypeableConditions :: Condition
checkTypeableConditions    = checkFlag Opt_DeriveDataTypeable
checkOldTypeableConditions = checkFlag Opt_DeriveDataTypeable `andCond` cond_oldTypeableOK

nonStdErr :: Class -> SDoc
nonStdErr cls = quotes (ppr cls) <+> ptext (sLit "is not a derivable class")

sideConditions :: DerivContext -> Class -> Maybe Condition
sideConditions mtheta cls
  | cls_key == eqClassKey          = Just (cond_std `andCond` cond_args cls)
  | cls_key == ordClassKey         = Just (cond_std `andCond` cond_args cls)
  | cls_key == showClassKey        = Just (cond_std `andCond` cond_args cls)
  | cls_key == readClassKey        = Just (cond_std `andCond` cond_args cls)
  | cls_key == enumClassKey        = Just (cond_std `andCond` cond_isEnumeration)
  | cls_key == ixClassKey          = Just (cond_std `andCond` cond_enumOrProduct cls)
  | cls_key == boundedClassKey     = Just (cond_std `andCond` cond_enumOrProduct cls)
  | cls_key == dataClassKey        = Just (checkFlag Opt_DeriveDataTypeable `andCond`
                                           cond_std `andCond` cond_args cls)
  | cls_key == functorClassKey     = Just (checkFlag Opt_DeriveFunctor `andCond`
                                           cond_functorOK True)  -- NB: no cond_std!
  | cls_key == foldableClassKey    = Just (checkFlag Opt_DeriveFoldable `andCond`
                                           cond_functorOK False) -- Functor/Fold/Trav works ok for rank-n types
  | cls_key == traversableClassKey = Just (checkFlag Opt_DeriveTraversable `andCond`
                                           cond_functorOK False)
  | cls_key == genClassKey         = Just (cond_RepresentableOk `andCond`
                                           checkFlag Opt_DeriveGeneric)
  | cls_key == gen1ClassKey        = Just (cond_Representable1Ok `andCond`
                                           checkFlag Opt_DeriveGeneric)
  | otherwise = Nothing
  where
    cls_key = getUnique cls
    cond_std = cond_stdOK mtheta

type Condition = (DynFlags, TyCon, [Type]) -> Maybe SDoc
        -- first Bool is whether or not we are allowed to derive Data and Typeable
        -- second Bool is whether or not we are allowed to derive Functor
        -- TyCon is the *representation* tycon if the data type is an indexed one
        -- [Type] are the type arguments to the (representation) TyCon
        -- Nothing => OK

orCond :: Condition -> Condition -> Condition
orCond c1 c2 tc
  = case c1 tc of
        Nothing -> Nothing          -- c1 succeeds
        Just x  -> case c2 tc of    -- c1 fails
                     Nothing -> Nothing
                     Just y  -> Just (x $$ ptext (sLit "  or") $$ y)
                                    -- Both fail

andCond :: Condition -> Condition -> Condition
andCond c1 c2 tc = case c1 tc of
                     Nothing -> c2 tc   -- c1 succeeds
                     Just x  -> Just x  -- c1 fails

cond_stdOK :: DerivContext -> Condition
cond_stdOK (Just _) _
  = Nothing     -- Don't check these conservative conditions for
                -- standalone deriving; just generate the code
                -- and let the typechecker handle the result
cond_stdOK Nothing (_, rep_tc, _)
  | null data_cons      = Just (no_cons_why rep_tc $$ suggestion)
  | not (null con_whys) = Just (vcat con_whys $$ suggestion)
  | otherwise           = Nothing
  where
    suggestion  = ptext (sLit "Possible fix: use a standalone deriving declaration instead")
    data_cons   = tyConDataCons rep_tc
    con_whys = mapCatMaybes check_con data_cons

    check_con :: DataCon -> Maybe SDoc
    check_con con
      | isVanillaDataCon con
      , all isTauTy (dataConOrigArgTys con) = Nothing
      | otherwise = Just (badCon con (ptext (sLit "must have a Haskell-98 type")))

no_cons_why :: TyCon -> SDoc
no_cons_why rep_tc = quotes (pprSourceTyCon rep_tc) <+>
                     ptext (sLit "must have at least one data constructor")

cond_RepresentableOk :: Condition
cond_RepresentableOk (_, tc, tc_args) = canDoGenerics tc tc_args

cond_Representable1Ok :: Condition
cond_Representable1Ok (_, tc, tc_args) = canDoGenerics1 tc tc_args

cond_enumOrProduct :: Class -> Condition
cond_enumOrProduct cls = cond_isEnumeration `orCond`
                         (cond_isProduct `andCond` cond_args cls)

cond_args :: Class -> Condition
-- For some classes (eg Eq, Ord) we allow unlifted arg types
-- by generating specilaised code.  For others (eg Data) we don't.
cond_args cls (_, tc, _)
  = case bad_args of
      []      -> Nothing
      (ty:_) -> Just (hang (ptext (sLit "Don't know how to derive") <+> quotes (ppr cls))
                         2 (ptext (sLit "for type") <+> quotes (ppr ty)))
  where
    bad_args = [ arg_ty | con <- tyConDataCons tc
                        , arg_ty <- dataConOrigArgTys con
                        , isUnLiftedType arg_ty
                        , not (ok_ty arg_ty) ]

    cls_key = classKey cls
    ok_ty arg_ty
     | cls_key == eqClassKey   = check_in arg_ty ordOpTbl
     | cls_key == ordClassKey  = check_in arg_ty ordOpTbl
     | cls_key == showClassKey = check_in arg_ty boxConTbl
     | otherwise               = False    -- Read, Ix etc

    check_in :: Type -> [(Type,a)] -> Bool
    check_in arg_ty tbl = any (eqType arg_ty . fst) tbl


cond_isEnumeration :: Condition
cond_isEnumeration (_, rep_tc, _)
  | isEnumerationTyCon rep_tc = Nothing
  | otherwise                 = Just why
  where
    why = sep [ quotes (pprSourceTyCon rep_tc) <+>
                  ptext (sLit "must be an enumeration type")
              , ptext (sLit "(an enumeration consists of one or more nullary, non-GADT constructors)") ]
                  -- See Note [Enumeration types] in TyCon

cond_isProduct :: Condition
cond_isProduct (_, rep_tc, _)
  | isProductTyCon rep_tc = Nothing
  | otherwise             = Just why
  where
    why = quotes (pprSourceTyCon rep_tc) <+>
          ptext (sLit "must have precisely one constructor")

cond_oldTypeableOK :: Condition
-- OK for kind-monomorphic Typeable class
-- Currently: (a) args all of kind *
--            (b) 7 or fewer args
cond_oldTypeableOK (_, tc, _)
  | tyConArity tc > 7 = Just too_many
  | not (all (isSubOpenTypeKind . tyVarKind) (tyConTyVars tc))
                      = Just bad_kind
  | otherwise         = Nothing
  where
    too_many = quotes (pprSourceTyCon tc) <+>
               ptext (sLit "must have 7 or fewer arguments")
    bad_kind = quotes (pprSourceTyCon tc) <+>
               ptext (sLit "must only have arguments of kind `*'")

functorLikeClassKeys :: [Unique]
functorLikeClassKeys = [functorClassKey, foldableClassKey, traversableClassKey]

cond_functorOK :: Bool -> Condition
-- OK for Functor/Foldable/Traversable class
-- Currently: (a) at least one argument
--            (b) don't use argument contravariantly
--            (c) don't use argument in the wrong place, e.g. data T a = T (X a a)
--            (d) optionally: don't use function types
--            (e) no "stupid context" on data type
cond_functorOK allowFunctions (_, rep_tc, _)
  | null tc_tvs
  = Just (ptext (sLit "Data type") <+> quotes (ppr rep_tc)
          <+> ptext (sLit "must have some type parameters"))

  | not (null bad_stupid_theta)
  = Just (ptext (sLit "Data type") <+> quotes (ppr rep_tc)
          <+> ptext (sLit "must not have a class context") <+> pprTheta bad_stupid_theta)

  | otherwise
  = msum (map check_con data_cons)      -- msum picks the first 'Just', if any
  where
    tc_tvs            = tyConTyVars rep_tc
    Just (_, last_tv) = snocView tc_tvs
    bad_stupid_theta  = filter is_bad (tyConStupidTheta rep_tc)
    is_bad pred       = last_tv `elemVarSet` tyVarsOfType pred

    data_cons = tyConDataCons rep_tc
    check_con con = msum (check_vanilla con : foldDataConArgs (ft_check con) con)

    check_vanilla :: DataCon -> Maybe SDoc
    check_vanilla con | isVanillaDataCon con = Nothing
                      | otherwise            = Just (badCon con existential)

    ft_check :: DataCon -> FFoldType (Maybe SDoc)
    ft_check con = FT { ft_triv = Nothing, ft_var = Nothing
                      , ft_co_var = Just (badCon con covariant)
                      , ft_fun = \x y -> if allowFunctions then x `mplus` y
                                                           else Just (badCon con functions)
                      , ft_tup = \_ xs  -> msum xs
                      , ft_ty_app = \_ x   -> x
                      , ft_bad_app = Just (badCon con wrong_arg)
                      , ft_forall = \_ x   -> x }

    existential = ptext (sLit "must not have existential arguments")
    covariant   = ptext (sLit "must not use the type variable in a function argument")
    functions   = ptext (sLit "must not contain function types")
    wrong_arg   = ptext (sLit "must use the type variable only as the last argument of a data type")

checkFlag :: ExtensionFlag -> Condition
checkFlag flag (dflags, _, _)
  | xopt flag dflags = Nothing
  | otherwise        = Just why
  where
    why = ptext (sLit "You need -X") <> text flag_str
          <+> ptext (sLit "to derive an instance for this class")
    flag_str = case [ s | (s, f, _) <- xFlags, f==flag ] of
                 [s]   -> s
                 other -> pprPanic "checkFlag" (ppr other)

std_class_via_iso :: Class -> Bool
-- These standard classes can be derived for a newtype
-- using the isomorphism trick *even if no -XGeneralizedNewtypeDeriving
-- because giving so gives the same results as generating the boilerplate
std_class_via_iso clas
  = classKey clas `elem` [eqClassKey, ordClassKey, ixClassKey, boundedClassKey]
        -- Not Read/Show because they respect the type
        -- Not Enum, because newtypes are never in Enum


non_iso_class :: Class -> Bool
-- *Never* derive Read, Show, Typeable, Data, Generic, Generic1 by isomorphism,
-- even with -XGeneralizedNewtypeDeriving
non_iso_class cls
  = classKey cls `elem` ([ readClassKey, showClassKey, dataClassKey
                         , genClassKey, gen1ClassKey, typeableClassKey]
                         ++ oldTypeableClassKeys)

oldTypeableClassKeys :: [Unique]
oldTypeableClassKeys = map getUnique oldTypeableClassNames

new_dfun_name :: Class -> TyCon -> TcM Name
new_dfun_name clas tycon        -- Just a simple wrapper
  = do { loc <- getSrcSpanM     -- The location of the instance decl, not of the tycon
        ; newDFunName clas [mkTyConApp tycon []] loc }
        -- The type passed to newDFunName is only used to generate
        -- a suitable string; hence the empty type arg list

badCon :: DataCon -> SDoc -> SDoc
badCon con msg = ptext (sLit "Constructor") <+> quotes (ppr con) <+> msg
\end{code}

Note [Superclasses of derived instance]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In general, a derived instance decl needs the superclasses of the derived
class too.  So if we have
        data T a = ...deriving( Ord )
then the initial context for Ord (T a) should include Eq (T a).  Often this is
redundant; we'll also generate an Ord constraint for each constructor argument,
and that will probably generate enough constraints to make the Eq (T a) constraint
be satisfied too.  But not always; consider:

 data S a = S
 instance Eq (S a)
 instance Ord (S a)

 data T a = MkT (S a) deriving( Ord )
 instance Num a => Eq (T a)

The derived instance for (Ord (T a)) must have a (Num a) constraint!
Similarly consider:
        data T a = MkT deriving( Data, Typeable )
Here there *is* no argument field, but we must nevertheless generate
a context for the Data instances:
        instance Typable a => Data (T a) where ...


%************************************************************************
%*                                                                      *
                Deriving newtypes
%*                                                                      *
%************************************************************************

\begin{code}
mkNewTypeEqn :: CtOrigin -> DynFlags -> [Var] -> Class
             -> [Type] -> TyCon -> [Type] -> TyCon -> [Type]
             -> DerivContext
             -> TcRn EarlyDerivSpec
mkNewTypeEqn orig dflags tvs
             cls cls_tys tycon tc_args rep_tycon rep_tc_args mtheta
-- Want: instance (...) => cls (cls_tys ++ [tycon tc_args]) where ...
  | can_derive_via_isomorphism && (newtype_deriving || std_class_via_iso cls)
  = do  { traceTc "newtype deriving:" (ppr tycon <+> ppr rep_tys <+> ppr all_preds)
        ; dfun_name <- new_dfun_name cls tycon
        ; loc <- getSrcSpanM
        ; let spec = DS { ds_loc = loc, ds_orig = orig
                        , ds_name = dfun_name, ds_tvs = varSetElemsKvsFirst dfun_tvs
                        , ds_cls = cls, ds_tys = inst_tys
                        , ds_tc = rep_tycon, ds_tc_args = rep_tc_args
                        , ds_theta =  mtheta `orElse` all_preds
                        , ds_newtype = True }
        ; return (if isJust mtheta then Right spec
                                   else Left spec) }

  | otherwise
  = case checkSideConditions dflags mtheta cls cls_tys rep_tycon rep_tc_args of
      CanDerive -> go_for_it    -- Use the standard H98 method
      DerivableClassError msg   -- Error with standard class
        | can_derive_via_isomorphism -> bale_out (msg $$ suggest_nd)
        | otherwise                  -> bale_out msg
      NonDerivableClass         -- Must use newtype deriving
        | newtype_deriving           -> bale_out cant_derive_err  -- Too hard, even with newtype deriving
        | can_derive_via_isomorphism -> bale_out (non_std $$ suggest_nd) -- Try newtype deriving!
        | otherwise                  -> bale_out non_std
  where
        newtype_deriving = xopt Opt_GeneralizedNewtypeDeriving dflags
        go_for_it        = mk_data_eqn orig tvs cls tycon tc_args rep_tycon rep_tc_args mtheta
        bale_out msg     = failWithTc (derivingThingErr newtype_deriving cls cls_tys inst_ty msg)

        non_std    = nonStdErr cls
        suggest_nd = ptext (sLit "Try -XGeneralizedNewtypeDeriving for GHC's newtype-deriving extension")

        -- Here is the plan for newtype derivings.  We see
        --        newtype T a1...an = MkT (t ak+1...an) deriving (.., C s1 .. sm, ...)
        -- where t is a type,
        --       ak+1...an is a suffix of a1..an, and are all tyars
        --       ak+1...an do not occur free in t, nor in the s1..sm
        --       (C s1 ... sm) is a  *partial applications* of class C
        --                      with the last parameter missing
        --       (T a1 .. ak) matches the kind of C's last argument
        --              (and hence so does t)
        -- The latter kind-check has been done by deriveTyData already,
        -- and tc_args are already trimmed
        --
        -- We generate the instance
        --       instance forall ({a1..ak} u fvs(s1..sm)).
        --                C s1 .. sm t => C s1 .. sm (T a1...ak)
        -- where T a1...ap is the partial application of
        --       the LHS of the correct kind and p >= k
        --
        --      NB: the variables below are:
        --              tc_tvs = [a1, ..., an]
        --              tyvars_to_keep = [a1, ..., ak]
        --              rep_ty = t ak .. an
        --              deriv_tvs = fvs(s1..sm) \ tc_tvs
        --              tys = [s1, ..., sm]
        --              rep_fn' = t
        --
        -- Running example: newtype T s a = MkT (ST s a) deriving( Monad )
        -- We generate the instance
        --      instance Monad (ST s) => Monad (T s) where

        nt_eta_arity = length (fst (newTyConEtadRhs rep_tycon))
                -- For newtype T a b = MkT (S a a b), the TyCon machinery already
                -- eta-reduces the representation type, so we know that
                --      T a ~ S a a
                -- That's convenient here, because we may have to apply
                -- it to fewer than its original complement of arguments

        -- Note [Newtype representation]
        -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        -- Need newTyConRhs (*not* a recursive representation finder)
        -- to get the representation type. For example
        --      newtype B = MkB Int
        --      newtype A = MkA B deriving( Num )
        -- We want the Num instance of B, *not* the Num instance of Int,
        -- when making the Num instance of A!
        rep_inst_ty = newTyConInstRhs rep_tycon rep_tc_args
        rep_tys     = cls_tys ++ [rep_inst_ty]
        rep_pred    = mkClassPred cls rep_tys
                -- rep_pred is the representation dictionary, from where
                -- we are gong to get all the methods for the newtype
                -- dictionary


    -- Next we figure out what superclass dictionaries to use
    -- See Note [Newtype deriving superclasses] above

        cls_tyvars = classTyVars cls
        dfun_tvs = tyVarsOfTypes inst_tys
        inst_ty = mkTyConApp tycon tc_args
        inst_tys = cls_tys ++ [inst_ty]
        sc_theta = substTheta (zipOpenTvSubst cls_tyvars inst_tys)
                              (classSCTheta cls)

                -- If there are no tyvars, there's no need
                -- to abstract over the dictionaries we need
                -- Example:     newtype T = MkT Int deriving( C )
                -- We get the derived instance
                --              instance C T
                -- rather than
                --              instance C Int => C T
        all_preds = rep_pred : sc_theta         -- NB: rep_pred comes first

        -------------------------------------------------------------------
        --  Figuring out whether we can only do this newtype-deriving thing

        can_derive_via_isomorphism
           =  not (non_iso_class cls)
           && arity_ok
           && eta_ok
           && ats_ok
--         && not (isRecursiveTyCon tycon)      -- Note [Recursive newtypes]

        arity_ok = length cls_tys + 1 == classArity cls
                -- Well kinded; eg not: newtype T ... deriving( ST )
                --                      because ST needs *2* type params

        -- Check that eta reduction is OK
        eta_ok = nt_eta_arity <= length rep_tc_args
                -- The newtype can be eta-reduced to match the number
                --     of type argument actually supplied
                --        newtype T a b = MkT (S [a] b) deriving( Monad )
                --     Here the 'b' must be the same in the rep type (S [a] b)
                --     And the [a] must not mention 'b'.  That's all handled
                --     by nt_eta_rity.

        ats_ok = null (classATs cls)
               -- No associated types for the class, because we don't
               -- currently generate type 'instance' decls; and cannot do
               -- so for 'data' instance decls

        cant_derive_err
           = vcat [ ppUnless arity_ok arity_msg
                  , ppUnless eta_ok eta_msg
                  , ppUnless ats_ok ats_msg ]
        arity_msg = quotes (ppr (mkClassPred cls cls_tys)) <+> ptext (sLit "does not have arity 1")
        eta_msg   = ptext (sLit "cannot eta-reduce the representation type enough")
        ats_msg   = ptext (sLit "the class has associated types")
\end{code}

Note [Recursive newtypes]
~~~~~~~~~~~~~~~~~~~~~~~~~
Newtype deriving works fine, even if the newtype is recursive.
e.g.    newtype S1 = S1 [T1 ()]
        newtype T1 a = T1 (StateT S1 IO a ) deriving( Monad )
Remember, too, that type families are curretly (conservatively) given
a recursive flag, so this also allows newtype deriving to work
for type famillies.

We used to exclude recursive types, because we had a rather simple
minded way of generating the instance decl:
   newtype A = MkA [A]
   instance Eq [A] => Eq A      -- Makes typechecker loop!
But now we require a simple context, so it's ok.


%************************************************************************
%*                                                                      *
\subsection[TcDeriv-fixpoint]{Finding the fixed point of \tr{deriving} equations}
%*                                                                      *
%************************************************************************

A ``solution'' (to one of the equations) is a list of (k,TyVarTy tv)
terms, which is the final correct RHS for the corresponding original
equation.
\begin{itemize}
\item
Each (k,TyVarTy tv) in a solution constrains only a type
variable, tv.

\item
The (k,TyVarTy tv) pairs in a solution are canonically
ordered by sorting on type varible, tv, (major key) and then class, k,
(minor key)
\end{itemize}

\begin{code}
inferInstanceContexts :: OverlapFlag -> [DerivSpec] -> TcM [DerivSpec]

inferInstanceContexts _ [] = return []

inferInstanceContexts oflag infer_specs
  = do  { traceTc "inferInstanceContexts" $ vcat (map pprDerivSpec infer_specs)
        ; iterate_deriv 1 initial_solutions }
  where
    ------------------------------------------------------------------
        -- The initial solutions for the equations claim that each
        -- instance has an empty context; this solution is certainly
        -- in canonical form.
    initial_solutions :: [ThetaType]
    initial_solutions = [ [] | _ <- infer_specs ]

    ------------------------------------------------------------------
        -- iterate_deriv calculates the next batch of solutions,
        -- compares it with the current one; finishes if they are the
        -- same, otherwise recurses with the new solutions.
        -- It fails if any iteration fails
    iterate_deriv :: Int -> [ThetaType] -> TcM [DerivSpec]
    iterate_deriv n current_solns
      | n > 20  -- Looks as if we are in an infinite loop
                -- This can happen if we have -XUndecidableInstances
                -- (See TcSimplify.tcSimplifyDeriv.)
      = pprPanic "solveDerivEqns: probable loop"
                 (vcat (map pprDerivSpec infer_specs) $$ ppr current_solns)
      | otherwise
      = do {      -- Extend the inst info from the explicit instance decls
                  -- with the current set of solutions, and simplify each RHS
             inst_specs <- zipWithM (mkInstance oflag) current_solns infer_specs
           ; new_solns <- checkNoErrs $
                          extendLocalInstEnv inst_specs $
                          mapM gen_soln infer_specs

           ; let eqList :: (a -> b -> Bool) -> [a] -> [b] -> Bool
                 eqList f xs ys = length xs == length ys && and (zipWith f xs ys)

           ; if (eqList (eqList eqType) current_solns new_solns) then
                return [ spec { ds_theta = soln }
                       | (spec, soln) <- zip infer_specs current_solns ]
             else
                iterate_deriv (n+1) new_solns }

    ------------------------------------------------------------------
    gen_soln :: DerivSpec  -> TcM [PredType]
    gen_soln (DS { ds_loc = loc, ds_orig = orig, ds_tvs = tyvars
                 , ds_cls = clas, ds_tys = inst_tys, ds_theta = deriv_rhs })
      = setSrcSpan loc  $
        addErrCtxt (derivInstCtxt the_pred) $
        do { theta <- simplifyDeriv orig the_pred tyvars deriv_rhs
                -- checkValidInstance tyvars theta clas inst_tys
                -- Not necessary; see Note [Exotic derived instance contexts]
                --                in TcSimplify

           ; traceTc "TcDeriv" (ppr deriv_rhs $$ ppr theta)
                -- Claim: the result instance declaration is guaranteed valid
                -- Hence no need to call:
                --   checkValidInstance tyvars theta clas inst_tys
           ; return (sortBy cmpType theta) }    -- Canonicalise before returning the solution
      where
        the_pred = mkClassPred clas inst_tys

------------------------------------------------------------------
mkInstance :: OverlapFlag -> ThetaType -> DerivSpec -> TcM ClsInst
mkInstance overlap_flag theta
           (DS { ds_name = dfun_name
               , ds_tvs = tvs, ds_cls = clas, ds_tys = tys })
  = do { (subst, tvs') <- tcInstSkolTyVars tvs
       ; return (mkLocalInstance dfun overlap_flag tvs' clas (substTys subst tys)) }
  where
    dfun = mkDictFunId dfun_name tvs theta clas tys


extendLocalInstEnv :: [ClsInst] -> TcM a -> TcM a
-- Add new locally-defined instances; don't bother to check
-- for functional dependency errors -- that'll happen in TcInstDcls
extendLocalInstEnv dfuns thing_inside
 = do { env <- getGblEnv
      ; let  inst_env' = extendInstEnvList (tcg_inst_env env) dfuns
             env'      = env { tcg_inst_env = inst_env' }
      ; setGblEnv env' thing_inside }
\end{code}


***********************************************************************************
*                                                                                 * 
*            Simplify derived constraints
*                                                                                 *
***********************************************************************************

\begin{code}
simplifyDeriv :: CtOrigin
              -> PredType
              -> [TyVar]        
              -> ThetaType              -- Wanted
              -> TcM ThetaType  -- Needed
-- Given  instance (wanted) => C inst_ty 
-- Simplify 'wanted' as much as possibles
-- Fail if not possible
simplifyDeriv orig pred tvs theta 
  = do { (skol_subst, tvs_skols) <- tcInstSkolTyVars tvs -- Skolemize
                -- The constraint solving machinery 
                -- expects *TcTyVars* not TyVars.  
                -- We use *non-overlappable* (vanilla) skolems
                -- See Note [Overlap and deriving]

       ; let subst_skol = zipTopTvSubst tvs_skols $ map mkTyVarTy tvs
             skol_set   = mkVarSet tvs_skols
             doc = ptext (sLit "deriving") <+> parens (ppr pred)

       ; wanted <- newFlatWanteds orig (substTheta skol_subst theta)

       ; traceTc "simplifyDeriv" $ 
         vcat [ pprTvBndrs tvs $$ ppr theta $$ ppr wanted, doc ]
       ; (residual_wanted, _ev_binds1)
             <- solveWantedsTcM (mkFlatWC wanted)
                -- Post: residual_wanted are already zonked

       ; let (good, bad) = partitionBagWith get_good (wc_flat residual_wanted)
                         -- See Note [Exotic derived instance contexts]
             get_good :: Ct -> Either PredType Ct
             get_good ct | validDerivPred skol_set p 
                         , isWantedCt ct  = Left p 
                         -- NB: residual_wanted may contain unsolved
                         -- Derived and we stick them into the bad set
                         -- so that reportUnsolved may decide what to do with them
                         | otherwise = Right ct
                         where p = ctPred ct

       -- We never want to defer these errors because they are errors in the
       -- compiler! Hence the `False` below
       ; reportAllUnsolved (residual_wanted { wc_flat = bad })

       ; let min_theta = mkMinimalBySCs (bagToList good)
       ; return (substTheta subst_skol min_theta) }
\end{code}

Note [Overlap and deriving]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider some overlapping instances:
  data Show a => Show [a] where ..
  data Show [Char] where ...

Now a data type with deriving:
  data T a = MkT [a] deriving( Show )

We want to get the derived instance
  instance Show [a] => Show (T a) where...
and NOT
  instance Show a => Show (T a) where...
so that the (Show (T Char)) instance does the Right Thing

It's very like the situation when we're inferring the type
of a function
   f x = show [x]
and we want to infer
   f :: Show [a] => a -> String

BOTTOM LINE: use vanilla, non-overlappable skolems when inferring
             the context for the derived instance. 
             Hence tcInstSkolTyVars not tcInstSuperSkolTyVars

Note [Exotic derived instance contexts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In a 'derived' instance declaration, we *infer* the context.  It's a
bit unclear what rules we should apply for this; the Haskell report is
silent.  Obviously, constraints like (Eq a) are fine, but what about
        data T f a = MkT (f a) deriving( Eq )
where we'd get an Eq (f a) constraint.  That's probably fine too.

One could go further: consider
        data T a b c = MkT (Foo a b c) deriving( Eq )
        instance (C Int a, Eq b, Eq c) => Eq (Foo a b c)

Notice that this instance (just) satisfies the Paterson termination 
conditions.  Then we *could* derive an instance decl like this:

        instance (C Int a, Eq b, Eq c) => Eq (T a b c) 
even though there is no instance for (C Int a), because there just
*might* be an instance for, say, (C Int Bool) at a site where we
need the equality instance for T's.  

However, this seems pretty exotic, and it's quite tricky to allow
this, and yet give sensible error messages in the (much more common)
case where we really want that instance decl for C.

So for now we simply require that the derived instance context
should have only type-variable constraints.

Here is another example:
        data Fix f = In (f (Fix f)) deriving( Eq )
Here, if we are prepared to allow -XUndecidableInstances we
could derive the instance
        instance Eq (f (Fix f)) => Eq (Fix f)
but this is so delicate that I don't think it should happen inside
'deriving'. If you want this, write it yourself!

NB: if you want to lift this condition, make sure you still meet the
termination conditions!  If not, the deriving mechanism generates
larger and larger constraints.  Example:
  data Succ a = S a
  data Seq a = Cons a (Seq (Succ a)) | Nil deriving Show

Note the lack of a Show instance for Succ.  First we'll generate
  instance (Show (Succ a), Show a) => Show (Seq a)
and then
  instance (Show (Succ (Succ a)), Show (Succ a), Show a) => Show (Seq a)
and so on.  Instead we want to complain of no instance for (Show (Succ a)).

The bottom line
~~~~~~~~~~~~~~~
Allow constraints which consist only of type variables, with no repeats.


%************************************************************************
%*                                                                      *
\subsection[TcDeriv-normal-binds]{Bindings for the various classes}
%*                                                                      *
%************************************************************************

After all the trouble to figure out the required context for the
derived instance declarations, all that's left is to chug along to
produce them.  They will then be shoved into @tcInstDecls2@, which
will do all its usual business.

There are lots of possibilities for code to generate.  Here are
various general remarks.

PRINCIPLES:
\begin{itemize}
\item
We want derived instances of @Eq@ and @Ord@ (both v common) to be
``you-couldn't-do-better-by-hand'' efficient.

\item
Deriving @Show@---also pretty common--- should also be reasonable good code.

\item
Deriving for the other classes isn't that common or that big a deal.
\end{itemize}

PRAGMATICS:

\begin{itemize}
\item
Deriving @Ord@ is done mostly with the 1.3 @compare@ method.

\item
Deriving @Eq@ also uses @compare@, if we're deriving @Ord@, too.

\item
We {\em normally} generate code only for the non-defaulted methods;
there are some exceptions for @Eq@ and (especially) @Ord@...

\item
Sometimes we use a @_con2tag_<tycon>@ function, which returns a data
constructor's numeric (@Int#@) tag.  These are generated by
@gen_tag_n_con_binds@, and the heuristic for deciding if one of
these is around is given by @hasCon2TagFun@.

The examples under the different sections below will make this
clearer.

\item
Much less often (really just for deriving @Ix@), we use a
@_tag2con_<tycon>@ function.  See the examples.

\item
We use the renamer!!!  Reason: we're supposed to be
producing @LHsBinds Name@ for the methods, but that means
producing correctly-uniquified code on the fly.  This is entirely
possible (the @TcM@ monad has a @UniqueSupply@), but it is painful.
So, instead, we produce @MonoBinds RdrName@ then heave 'em through
the renamer.  What a great hack!
\end{itemize}

\begin{code}
-- Generate the InstInfo for the required instance paired with the
--   *representation* tycon for that instance,
-- plus any auxiliary bindings required
--
-- Representation tycons differ from the tycon in the instance signature in
-- case of instances for indexed families.
--
genInst :: Bool             -- True <=> standalone deriving
        -> OverlapFlag
        -> CommonAuxiliaries
        -> DerivSpec -> TcM (InstInfo RdrName, BagDerivStuff)
genInst standalone_deriv oflag comauxs
        spec@(DS { ds_tvs = tvs, ds_tc = rep_tycon, ds_tc_args = rep_tc_args
                 , ds_theta = theta, ds_newtype = is_newtype
                 , ds_name = name, ds_cls = clas })
  | is_newtype
  = do { inst_spec <- mkInstance oflag theta spec
       ; return (InstInfo { iSpec   = inst_spec
                          , iBinds  = NewTypeDerived co rep_tycon }, emptyBag) }

  | otherwise
  = do { fix_env <- getFixityEnv
       ; (meth_binds, deriv_stuff) <- genDerivStuff (getSrcSpan name)
                                        fix_env clas name rep_tycon
                                        (lookup rep_tycon comauxs)
       ; inst_spec <- mkInstance oflag theta spec
       ; let inst_info = InstInfo { iSpec   = inst_spec
                                  , iBinds  = VanillaInst meth_binds []
                                                standalone_deriv }
       ; return ( inst_info, deriv_stuff) }
  where
    co1 = case tyConFamilyCoercion_maybe rep_tycon of
              Just co_con -> mkTcUnbranchedAxInstCo co_con rep_tc_args
              Nothing     -> id_co
              -- Not a family => rep_tycon = main tycon
    co2 = mkTcUnbranchedAxInstCo (newTyConCo rep_tycon) rep_tc_args
    co  = mkTcForAllCos tvs (co1 `mkTcTransCo` co2)
    id_co = mkTcReflCo (mkTyConApp rep_tycon rep_tc_args)

-- Example: newtype instance N [a] = N1 (Tree a)
--          deriving instance Eq b => Eq (N [(b,b)])
-- From the instance, we get an implicit newtype R1:N a = N1 (Tree a)
-- When dealing with the deriving clause
--    co1 : N [(b,b)] ~ R1:N (b,b)
--    co2 : R1:N (b,b) ~ Tree (b,b)
--    co  : N [(b,b)] ~ Tree (b,b)

genDerivStuff :: SrcSpan -> FixityEnv -> Class -> Name -> TyCon
              -> Maybe CommonAuxiliary
              -> TcM (LHsBinds RdrName, BagDerivStuff)
genDerivStuff loc fix_env clas name tycon comaux_maybe
  | className clas `elem` oldTypeableClassNames
  = do dflags <- getDynFlags
       return (gen_old_Typeable_binds dflags loc tycon, emptyBag)

  | className clas == typeableClassName
  = do dflags <- getDynFlags
       return (gen_Typeable_binds dflags loc tycon, emptyBag)

  | ck `elem` [genClassKey, gen1ClassKey]   -- Special case because monadic
  = let gk =  if ck == genClassKey then Gen0 else Gen1 -- TODO NSF: correctly identify when we're building Both instead of One
        Just metaTyCons = comaux_maybe -- well-guarded by commonAuxiliaries and genInst
    in do
      (binds, faminst) <- gen_Generic_binds gk tycon metaTyCons (nameModule name)
      return (binds, DerivFamInst faminst `consBag` emptyBag)

  | otherwise                      -- Non-monadic generators
  = do dflags <- getDynFlags
       case assocMaybe (gen_list dflags) (getUnique clas) of
        Just gen_fn -> return (gen_fn loc tycon)
        Nothing     -> pprPanic "genDerivStuff: bad derived class" (ppr clas)
  where
    ck = classKey clas

    gen_list :: DynFlags
             -> [(Unique, SrcSpan -> TyCon -> (LHsBinds RdrName, BagDerivStuff))]
    gen_list dflags
             = [(eqClassKey, gen_Eq_binds)
               ,(ordClassKey, gen_Ord_binds)
               ,(enumClassKey, gen_Enum_binds)
               ,(boundedClassKey, gen_Bounded_binds)
               ,(ixClassKey, gen_Ix_binds)
               ,(showClassKey, gen_Show_binds fix_env)
               ,(readClassKey, gen_Read_binds fix_env)
               ,(dataClassKey, gen_Data_binds dflags)
               ,(functorClassKey, gen_Functor_binds)
               ,(foldableClassKey, gen_Foldable_binds)
               ,(traversableClassKey, gen_Traversable_binds)
               ]
\end{code}

%************************************************************************
%*                                                                      *
\subsection[TcDeriv-taggery-Names]{What con2tag/tag2con functions are available?}
%*                                                                      *
%************************************************************************

\begin{code}
derivingKindErr :: TyCon -> Class -> [Type] -> Kind -> MsgDoc
derivingKindErr tc cls cls_tys cls_kind
  = hang (ptext (sLit "Cannot derive well-kinded instance of form")
                <+> quotes (pprClassPred cls cls_tys <+> parens (ppr tc <+> ptext (sLit "..."))))
       2 (ptext (sLit "Class") <+> quotes (ppr cls)
            <+> ptext (sLit "expects an argument of kind") <+> quotes (pprKind cls_kind))

derivingEtaErr :: Class -> [Type] -> Type -> MsgDoc
derivingEtaErr cls cls_tys inst_ty
  = sep [ptext (sLit "Cannot eta-reduce to an instance of form"),
         nest 2 (ptext (sLit "instance (...) =>")
                <+> pprClassPred cls (cls_tys ++ [inst_ty]))]

typeFamilyPapErr :: TyCon -> Class -> [Type] -> Type -> MsgDoc
typeFamilyPapErr tc cls cls_tys inst_ty
  = hang (ptext (sLit "Derived instance") <+> quotes (pprClassPred cls (cls_tys ++ [inst_ty])))
       2 (ptext (sLit "requires illegal partial application of data type family") <+> ppr tc)

derivingThingErr :: Bool -> Class -> [Type] -> Type -> MsgDoc -> MsgDoc
derivingThingErr newtype_deriving clas tys ty why
  = sep [(hang (ptext (sLit "Can't make a derived instance of"))
             2 (quotes (ppr pred))
          $$ nest 2 extra) <> colon,
         nest 2 why]
  where
    extra | newtype_deriving = ptext (sLit "(even with cunning newtype deriving)")
          | otherwise        = empty
    pred = mkClassPred clas (tys ++ [ty])

derivingHiddenErr :: TyCon -> SDoc
derivingHiddenErr tc
  = hang (ptext (sLit "The data constructors of") <+> quotes (ppr tc) <+> ptext (sLit "are not all in scope"))
       2 (ptext (sLit "so you cannot derive an instance for it"))

standaloneCtxt :: LHsType Name -> SDoc
standaloneCtxt ty = hang (ptext (sLit "In the stand-alone deriving instance for"))
                       2 (quotes (ppr ty))

derivInstCtxt :: PredType -> MsgDoc
derivInstCtxt pred
  = ptext (sLit "When deriving the instance for") <+> parens (ppr pred)
\end{code}
