{-# LANGUAGE AllowAmbiguousTypes    #-}

{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms        #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-----------------------------------------------------------------------------
--
-- (c) The University of Glasgow 2006
--
-- The purpose of this module is to transform an HsExpr into a CoreExpr which
-- when evaluated, returns a (Meta.Q Meta.Exp) computation analogous to the
-- input HsExpr. We do this in the DsM monad, which supplies access to
-- CoreExpr's of the "smart constructors" of the Meta.Exp datatype.
--
-- It also defines a bunch of knownKeyNames, in the same way as is done
-- in prelude/GHC.Builtin.Names.  It's much more convenient to do it here, because
-- otherwise we have to recompile GHC.Builtin.Names whenever we add a Name, which is
-- a Royal Pain (triggers other recompilation).
-----------------------------------------------------------------------------

module GHC.HsToCore.Quote( dsBracket ) where

import GHC.Prelude
import GHC.Platform

import GHC.Driver.Session

import GHC.HsToCore.Errors.Types
import {-# SOURCE #-} GHC.HsToCore.Expr ( dsExpr )
import GHC.HsToCore.Match.Literal
import GHC.HsToCore.Monad
import GHC.HsToCore.Binds

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import GHC.Hs

import GHC.Tc.Utils.TcType
import GHC.Tc.Types.Evidence

import GHC.Core.Class
import GHC.Core.DataCon
import GHC.Core.TyCon
import GHC.Core.Multiplicity ( pattern Many )
import GHC.Core
import GHC.Core.Make
import GHC.Core.Utils

import GHC.Builtin.Names
import GHC.Builtin.Names.TH
import GHC.Builtin.Types

import GHC.Unit.Module

import GHC.Utils.Outputable
import GHC.Utils.Panic
import GHC.Utils.Panic.Plain
import GHC.Utils.Misc
import GHC.Utils.Monad

import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Data.Maybe

import GHC.Types.SrcLoc as SrcLoc
import GHC.Types.Unique
import GHC.Types.Basic
import GHC.Types.ForeignCall
import GHC.Types.Var
import GHC.Types.Id
import GHC.Types.SourceText
import GHC.Types.Fixity
import GHC.Types.TyThing
import GHC.Types.Name hiding( varName, tcName )
import GHC.Types.Name.Env

import GHC.TypeLits
import Data.Kind (Constraint)

import qualified GHC.LanguageExtensions as LangExt

import Data.ByteString ( unpack )
import Control.Monad
import Data.List (sort, sortBy)
import Data.List.NonEmpty ( NonEmpty(..) )
import Data.Function
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class

data MetaWrappers = MetaWrappers {
      -- Applies its argument to a type argument `m` and dictionary `Quote m`
      quoteWrapper :: CoreExpr -> CoreExpr
      -- Apply its argument to a type argument `m` and a dictionary `Monad m`
    , monadWrapper :: CoreExpr -> CoreExpr
      -- Apply the container typed variable `m` to the argument type `T` to get `m T`.
    , metaTy :: Type -> Type
      -- Information about the wrappers which be printed to be inspected
    , _debugWrappers :: (HsWrapper, HsWrapper, Type)
    }

-- | Construct the functions which will apply the relevant part of the
-- QuoteWrapper to identifiers during desugaring.
mkMetaWrappers :: QuoteWrapper -> DsM MetaWrappers
mkMetaWrappers q@(QuoteWrapper quote_var_raw m_var) = do
      let quote_var = Var quote_var_raw
      -- Get the superclass selector to select the Monad dictionary, going
      -- to be used to construct the monadWrapper.
      quote_tc <- dsLookupTyCon quoteClassName
      monad_tc <- dsLookupTyCon monadClassName
      let Just cls = tyConClass_maybe quote_tc
          Just monad_cls = tyConClass_maybe monad_tc
          -- Quote m -> Monad m
          monad_sel = classSCSelId cls 0

          -- Only used for the defensive assertion that the selector has
          -- the expected type
          tyvars = dataConUserTyVarBinders (classDataCon cls)
          expected_ty = mkInvisForAllTys tyvars $
                          mkInvisFunTyMany (mkClassPred cls (mkTyVarTys (binderVars tyvars)))
                                           (mkClassPred monad_cls (mkTyVarTys (binderVars tyvars)))

      massertPpr (idType monad_sel `eqType` expected_ty) (ppr monad_sel $$ ppr expected_ty)

      let m_ty = Type m_var
          -- Construct the contents of MetaWrappers
          quoteWrapper = applyQuoteWrapper q
          monadWrapper = mkWpEvApps [EvExpr $ mkCoreApps (Var monad_sel) [m_ty, quote_var]] <.>
                            mkWpTyApps [m_var]
          tyWrapper t = mkAppTy m_var t
          debug = (quoteWrapper, monadWrapper, m_var)
      q_f <- dsHsWrapper quoteWrapper
      m_f <- dsHsWrapper monadWrapper
      return (MetaWrappers q_f m_f tyWrapper debug)

-- Turn A into m A
wrapName :: Name -> MetaM Type
wrapName n = do
  t <- lookupType n
  wrap_fn <- asks metaTy
  return (wrap_fn t)

-- The local state is always the same, calculated from the passed in
-- wrapper
type MetaM a = ReaderT MetaWrappers DsM a

getPlatform :: MetaM Platform
getPlatform = targetPlatform <$> getDynFlags

-----------------------------------------------------------------------------
dsBracket :: Maybe QuoteWrapper -- ^ This is Nothing only when we are dealing with a VarBr
          -> HsBracket GhcRn
          -> [PendingTcSplice]
          -> DsM CoreExpr
-- See Note [Desugaring Brackets]
-- Returns a CoreExpr of type (M TH.Exp)
-- The quoted thing is parameterised over Name, even though it has
-- been type checked.  We don't want all those type decorations!

dsBracket wrap brack splices
  = do_brack brack

  where
    runOverloaded act = do
      -- In the overloaded case we have to get given a wrapper, it is just
      -- for variable quotations that there is no wrapper, because they
      -- have a simple type.
      mw <- mkMetaWrappers (expectJust "runOverloaded" wrap)
      runReaderT (mapReaderT (dsExtendMetaEnv new_bit) act) mw


    new_bit = mkNameEnv [(n, DsSplice (unLoc e))
                        | PendingTcSplice n e <- splices]

    do_brack (VarBr _ _ n) = do { MkC e1  <- lookupOccDsM (unLoc n) ; return e1 }
    do_brack (ExpBr _ e)   = runOverloaded $ do { MkC e1  <- repLE e     ; return e1 }
    do_brack (PatBr _ p)   = runOverloaded $ do { MkC p1  <- repTopP p   ; return p1 }
    do_brack (TypBr _ t)   = runOverloaded $ do { MkC t1  <- repLTy t    ; return t1 }
    do_brack (DecBrG _ gp) = runOverloaded $ do { MkC ds1 <- repTopDs gp ; return ds1 }
    do_brack (DecBrL {})   = panic "dsBracket: unexpected DecBrL"
    do_brack (TExpBr _ e)  = runOverloaded $ do { MkC e1  <- repLE e     ; return e1 }

{-
Note [Desugaring Brackets]
~~~~~~~~~~~~~~~~~~~~~~~~~~

In the old days (pre Dec 2019) quotation brackets used to be monomorphic, ie
an expression bracket was of type Q Exp. This made the desugaring process simple
as there were no complicated type variables to keep consistent throughout the
whole AST. Due to the overloaded quotations proposal a quotation bracket is now
of type `Quote m => m Exp` and all the combinators defined in TH.Lib have been
generalised to work with any monad implementing a minimal interface.

https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0246-overloaded-bracket.rst

Users can rejoice at the flexibility but now there is some additional complexity in
how brackets are desugared as all these polymorphic combinators need their arguments
instantiated.

> IF YOU ARE MODIFYING THIS MODULE DO NOT USE ANYTHING SPECIFIC TO Q. INSTEAD
> USE THE `wrapName` FUNCTION TO APPLY THE `m` TYPE VARIABLE TO A TYPE CONSTRUCTOR.

What the arguments should be instantiated to is supplied by the `QuoteWrapper`
datatype which is produced by `GHC.Tc.Gen.Splice`. It is a pair of an evidence variable
for `Quote m` and a type variable `m`. All the polymorphic combinators in desugaring
need to be applied to these two type variables.

There are three important functions which do the application.

1. The default is `rep2` which takes a function name of type `Quote m => T` as an argument.
2. `rep2M` takes a function name of type `Monad m => T` as an argument
3. `rep2_nw` takes a function name without any constraints as an argument.

These functions then use the information in QuoteWrapper to apply the correct
arguments to the functions as the representation is constructed.

The `MetaM` monad carries around an environment of three functions which are
used in order to wrap the polymorphic combinators and instantiate the arguments
to the correct things.

1. quoteWrapper wraps functions of type `forall m . Quote m => T`
2. monadWrapper wraps functions of type `forall m . Monad m => T`
3. metaTy wraps a type in the polymorphic `m` variable of the whole representation.

Historical note about the implementation: At the first attempt, I attempted to
lie that the type of any quotation was `Quote m => m Exp` and then specialise it
by applying a wrapper to pass the `m` and `Quote m` arguments. This approach was
simpler to implement but didn't work because of nested splices. For example,
you might have a nested splice of a more specific type which fixes the type of
the overall quote and so all the combinators used must also be instantiated to
that specific type. Therefore you really have to use the contents of the quote
wrapper to directly apply the right type to the combinators rather than
first generate a polymorphic definition and then just apply the wrapper at the end.

-}

{- -------------- Examples --------------------

  [| \x -> x |]
====>
  gensym (unpackString "x"#) `bindQ` \ x1::String ->
  lam (pvar x1) (var x1)


  [| \x -> $(f [| x |]) |]
====>
  gensym (unpackString "x"#) `bindQ` \ x1::String ->
  lam (pvar x1) (f (var x1))
-}


-------------------------------------------------------
--                      Declarations
-------------------------------------------------------

-- Proxy for the phantom type of `Core`. All the generated fragments have
-- type something like `Quote m => m Exp` so to keep things simple we represent fragments
-- of that type as `M Exp`.
data M a

repTopP :: LPat GhcRn -> MetaM (Core (M TH.Pat))
repTopP pat = do { ss <- mkGenSyms (collectPatBinders CollNoDictBinders pat)
                 ; pat' <- addBinds ss (repLP pat)
                 ; wrapGenSyms ss pat' }

repTopDs :: HsGroup GhcRn -> MetaM (Core (M [TH.Dec]))
repTopDs group@(HsGroup { hs_valds   = valds
                        , hs_splcds  = splcds
                        , hs_tyclds  = tyclds
                        , hs_derivds = derivds
                        , hs_fixds   = fixds
                        , hs_defds   = defds
                        , hs_fords   = fords
                        , hs_warnds  = warnds
                        , hs_annds   = annds
                        , hs_ruleds  = ruleds
                        , hs_docs    = docs })
 = do { let { bndrs  = hsScopedTvBinders valds
                       ++ hsGroupBinders group
                       ++ map foExt (hsPatSynSelectors valds)
            ; instds = tyclds >>= group_instds } ;
        ss <- mkGenSyms bndrs ;

        -- Bind all the names mainly to avoid repeated use of explicit strings.
        -- Thus we get
        --      do { t :: String <- genSym "T" ;
        --           return (Data t [] ...more t's... }
        -- The other important reason is that the output must mention
        -- only "T", not "Foo:T" where Foo is the current module

        decls <- addBinds ss (
                  do { val_ds   <- rep_val_binds valds
                     ; _        <- mapM no_splice splcds
                     ; tycl_ds  <- mapM repTyClD (tyClGroupTyClDecls tyclds)
                     ; role_ds  <- mapM repRoleD (concatMap group_roles tyclds)
                     ; kisig_ds <- mapM repKiSigD (concatMap group_kisigs tyclds)
                     ; inst_ds  <- mapM repInstD instds
                     ; deriv_ds <- mapM repStandaloneDerivD derivds
                     ; fix_ds   <- mapM repLFixD fixds
                     ; def_ds   <- mapM repDefD defds
                     ; for_ds   <- mapM repForD fords
                     ; _        <- mapM no_warn (concatMap (wd_warnings . unLoc)
                                                           warnds)
                     ; ann_ds   <- mapM repAnnD annds
                     ; rule_ds  <- mapM repRuleD (concatMap (rds_rules . unLoc)
                                                            ruleds)
                     ; _        <- mapM no_doc docs

                        -- more needed
                     ;  return (de_loc $ sort_by_loc $
                                val_ds ++ catMaybes tycl_ds ++ role_ds
                                       ++ kisig_ds
                                       ++ (concat fix_ds)
                                       ++ def_ds
                                       ++ inst_ds ++ rule_ds ++ for_ds
                                       ++ ann_ds ++ deriv_ds) }) ;

        core_list <- repListM decTyConName return decls ;

        dec_ty <- lookupType decTyConName ;
        q_decs  <- repSequenceM dec_ty core_list ;

        wrapGenSyms ss q_decs
      }
  where
    no_splice (L loc _)
      = notHandledL (locA loc) ThSplicesWithinDeclBrackets
    no_warn :: LWarnDecl GhcRn -> MetaM a
    no_warn (L loc (Warning _ thing _))
      = notHandledL (locA loc) (ThWarningAndDeprecationPragmas thing)
    no_doc (L loc _)
      = notHandledL (locA loc) ThHaddockDocumentation

hsScopedTvBinders :: HsValBinds GhcRn -> [Name]
-- See Note [Scoped type variables in quotes]
hsScopedTvBinders binds
  = concatMap get_scoped_tvs sigs
  where
    sigs = case binds of
             ValBinds           _ _ sigs  -> sigs
             XValBindsLR (NValBinds _ sigs) -> sigs

get_scoped_tvs :: LSig GhcRn -> [Name]
get_scoped_tvs (L _ signature)
  | TypeSig _ _ sig <- signature
  = get_scoped_tvs_from_sig (hswc_body sig)
  | ClassOpSig _ _ _ sig <- signature
  = get_scoped_tvs_from_sig sig
  | PatSynSig _ _ sig <- signature
  = get_scoped_tvs_from_sig sig
  | otherwise
  = []

get_scoped_tvs_from_sig :: LHsSigType GhcRn -> [Name]
  -- Collect both implicit and explicit quantified variables, since
  -- the types in instance heads, as well as `via` types in DerivingVia, can
  -- bring implicitly quantified type variables into scope, e.g.,
  --
  --   instance Foo [a] where
  --     m = n @a
  --
  -- See also Note [Scoped type variables in quotes]
get_scoped_tvs_from_sig (L _ (HsSig{sig_bndrs = outer_bndrs})) =
  hsOuterTyVarNames outer_bndrs

{- Notes

Note [Scoped type variables in quotes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Quoting declarations with scoped type variables requires some care. Consider:

  $([d| f :: forall a. a -> a
        f x = x::a
      |])

Here, the `forall a` brings `a` into scope over the binding group. This has
ramifications when desugaring the quote, as we must ensure that that the
desugared code binds `a` with `Language.Haskell.TH.newName` and refers to the
bound `a` type variable in the type signature and in the body of `f`. As a
result, the call to `newName` must occur before any part of the declaration for
`f` is processed. To achieve this, we:

 (a) Gensym a binding for `a` at the same time as we do one for `f`,
     collecting the relevant binders with the hsScopedTvBinders family of
     functions.

 (b) Use `addBinds` to bring these gensymmed bindings into scope over any
     part of the code where the type variables scope. In the `f` example,
     above, that means the type signature and the body of `f`.

 (c) When processing the `forall`, /don't/ gensym the type variables. We have
     already brought the type variables into scope in part (b), after all, so
     gensymming them again would lead to shadowing. We use the rep_ty_sig
     family of functions for processing types without gensymming the type
     variables again.

 (d) Finally, we use wrapGenSyms to generate the Core for these scoped type
     variables:

       newName "a" >>= \a ->
         ... -- process the type signature and body of `f`

The relevant places are signposted with references to this Note.

Note [Binders and occurrences]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we desugar [d| data T = MkT |]
we want to get
        Data "T" [] [Con "MkT" []] []
and *not*
        Data "Foo:T" [] [Con "Foo:MkT" []] []
That is, the new data decl should fit into whatever new module it is
asked to fit in.   We do *not* clone, though; no need for this:
        Data "T79" ....

But if we see this:
        data T = MkT
        foo = reifyDecl T

then we must desugar to
        foo = Data "Foo:T" [] [Con "Foo:MkT" []] []

So in repTopDs we bring the binders into scope with mkGenSyms and addBinds.
And we use lookupOcc, rather than lookupBinder
in repTyClD and repC.

Note [Don't quantify implicit type variables in quotes]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If you're not careful, it's surprisingly easy to take this quoted declaration:

  [d| id :: a -> a
      id x = x
    |]

and have Template Haskell turn it into this:

  id :: forall a. a -> a
  id x = x

Notice that we explicitly quantified the variable `a`! The latter declaration
isn't what the user wrote in the first place.

Usually, the culprit behind these bugs is taking implicitly quantified type
variables (often from the hsib_vars field of HsImplicitBinders) and putting
them into a `ForallT` or `ForallC`. Doing so caused #13018 and #13123.
-}

-- represent associated family instances
--
repTyClD :: LTyClDecl GhcRn -> MetaM (Maybe (SrcSpan, Core (M TH.Dec)))

repTyClD (L loc (FamDecl { tcdFam = fam })) = liftM Just $
                                              repFamilyDecl (L loc fam)

repTyClD (L loc (SynDecl { tcdLName = tc, tcdTyVars = tvs, tcdRhs = rhs }))
  = do { tc1 <- lookupLOcc tc           -- See note [Binders and occurrences]
       ; dec <- addTyClTyVarBinds tvs $ \bndrs ->
                repSynDecl tc1 bndrs rhs
       ; return (Just (locA loc, dec)) }

repTyClD (L loc (DataDecl { tcdLName = tc
                          , tcdTyVars = tvs
                          , tcdDataDefn = defn }))
  = do { tc1 <- lookupLOcc tc           -- See note [Binders and occurrences]
       ; dec <- addTyClTyVarBinds tvs $ \bndrs ->
                repDataDefn tc1 (Left bndrs) defn
       ; return (Just (locA loc, dec)) }

repTyClD (L loc (ClassDecl { tcdCtxt = cxt, tcdLName = cls,
                             tcdTyVars = tvs, tcdFDs = fds,
                             tcdSigs = sigs, tcdMeths = meth_binds,
                             tcdATs = ats, tcdATDefs = atds }))
  = do { cls1 <- lookupLOcc cls         -- See note [Binders and occurrences]
       ; dec  <- addQTyVarBinds tvs $ \bndrs ->
           do { cxt1   <- repLContext cxt
          -- See Note [Scoped type variables in quotes]
              ; (ss, sigs_binds) <- rep_meth_sigs_binds sigs meth_binds
              ; fds1   <- repLFunDeps fds
              ; ats1   <- repFamilyDecls ats
              ; atds1  <- mapM (repAssocTyFamDefaultD . unLoc) atds
              ; decls1 <- repListM decTyConName return (ats1 ++ atds1 ++ sigs_binds)
              ; decls2 <- repClass cxt1 cls1 bndrs fds1 decls1
              ; wrapGenSyms ss decls2 }
       ; return $ Just (locA loc, dec)
       }

-------------------------
repRoleD :: LRoleAnnotDecl GhcRn -> MetaM (SrcSpan, Core (M TH.Dec))
repRoleD (L loc (RoleAnnotDecl _ tycon roles))
  = do { tycon1 <- lookupLOcc tycon
       ; roles1 <- mapM repRole roles
       ; roles2 <- coreList roleTyConName roles1
       ; dec <- repRoleAnnotD tycon1 roles2
       ; return (locA loc, dec) }

-------------------------
repKiSigD :: LStandaloneKindSig GhcRn -> MetaM (SrcSpan, Core (M TH.Dec))
repKiSigD (L loc kisig) =
  case kisig of
    StandaloneKindSig _ v ki -> do
      MkC th_v  <- lookupLOcc v
      MkC th_ki <- repHsSigType ki
      dec       <- rep2 kiSigDName [th_v, th_ki]
      pure (locA loc, dec)

-------------------------
repDataDefn :: Core TH.Name
            -> Either (Core [(M (TH.TyVarBndr ()))])
                        -- the repTyClD case
                      (Core (Maybe [(M (TH.TyVarBndr ()))]), Core (M TH.Type))
                        -- the repDataFamInstD case
            -> HsDataDefn GhcRn
            -> MetaM (Core (M TH.Dec))
repDataDefn tc opts
          (HsDataDefn { dd_ND = new_or_data, dd_ctxt = cxt, dd_kindSig = ksig
                      , dd_cons = cons, dd_derivs = mb_derivs })
  = do { cxt1     <- repLContext cxt
       ; derivs1  <- repDerivs mb_derivs
       ; case (new_or_data, cons) of
           (NewType, [con])  -> do { con'  <- repC con
                                   ; ksig' <- repMaybeLTy ksig
                                   ; repNewtype cxt1 tc opts ksig' con'
                                                derivs1 }
           (NewType, _) -> lift $ failWithDs (DsMultipleConForNewtype (getConNames $ unLoc $ head cons))
           (DataType, _) -> do { ksig' <- repMaybeLTy ksig
                               ; consL <- mapM repC cons
                               ; cons1 <- coreListM conTyConName consL
                               ; repData cxt1 tc opts ksig' cons1
                                         derivs1 }
       }

repSynDecl :: Core TH.Name -> Core [(M (TH.TyVarBndr ()))]
           -> LHsType GhcRn
           -> MetaM (Core (M TH.Dec))
repSynDecl tc bndrs ty
  = do { ty1 <- repLTy ty
       ; repTySyn tc bndrs ty1 }

repFamilyDecl :: LFamilyDecl GhcRn -> MetaM (SrcSpan, Core (M TH.Dec))
repFamilyDecl decl@(L loc (FamilyDecl { fdInfo      = info
                                      , fdLName     = tc
                                      , fdTyVars    = tvs
                                      , fdResultSig = L _ resultSig
                                      , fdInjectivityAnn = injectivity }))
  = do { tc1 <- lookupLOcc tc           -- See note [Binders and occurrences]
       ; let mkHsQTvs :: [LHsTyVarBndr () GhcRn] -> LHsQTyVars GhcRn
             mkHsQTvs tvs = HsQTvs { hsq_ext = []
                                   , hsq_explicit = tvs }
             resTyVar = case resultSig of
                     TyVarSig _ bndr -> mkHsQTvs [bndr]
                     _               -> mkHsQTvs []
       ; dec <- addTyClTyVarBinds tvs $ \bndrs ->
                addTyClTyVarBinds resTyVar $ \_ ->
           case info of
             ClosedTypeFamily Nothing ->
                 notHandled (ThAbstractClosedTypeFamily decl)
             ClosedTypeFamily (Just eqns) ->
               do { eqns1  <- mapM (repTyFamEqn . unLoc) eqns
                  ; eqns2  <- coreListM tySynEqnTyConName eqns1
                  ; result <- repFamilyResultSig resultSig
                  ; inj    <- repInjectivityAnn injectivity
                  ; repClosedFamilyD tc1 bndrs result inj eqns2 }
             OpenTypeFamily ->
               do { result <- repFamilyResultSig resultSig
                  ; inj    <- repInjectivityAnn injectivity
                  ; repOpenFamilyD tc1 bndrs result inj }
             DataFamily ->
               do { kind <- repFamilyResultSigToMaybeKind resultSig
                  ; repDataFamilyD tc1 bndrs kind }
       ; return (locA loc, dec)
       }

-- | Represent result signature of a type family
repFamilyResultSig :: FamilyResultSig GhcRn -> MetaM (Core (M TH.FamilyResultSig))
repFamilyResultSig (NoSig _)         = repNoSig
repFamilyResultSig (KindSig _ ki)    = do { ki' <- repLTy ki
                                          ; repKindSig ki' }
repFamilyResultSig (TyVarSig _ bndr) = do { bndr' <- repTyVarBndr bndr
                                          ; repTyVarSig bndr' }

-- | Represent result signature using a Maybe Kind. Used with data families,
-- where the result signature can be either missing or a kind but never a named
-- result variable.
repFamilyResultSigToMaybeKind :: FamilyResultSig GhcRn
                              -> MetaM (Core (Maybe (M TH.Kind)))
repFamilyResultSigToMaybeKind (NoSig _) =
    coreNothingM kindTyConName
repFamilyResultSigToMaybeKind (KindSig _ ki) =
    coreJustM kindTyConName =<< repLTy ki
repFamilyResultSigToMaybeKind TyVarSig{} =
    panic "repFamilyResultSigToMaybeKind: unexpected TyVarSig"

-- | Represent injectivity annotation of a type family
repInjectivityAnn :: Maybe (LInjectivityAnn GhcRn)
                  -> MetaM (Core (Maybe TH.InjectivityAnn))
repInjectivityAnn Nothing =
    coreNothing injAnnTyConName
repInjectivityAnn (Just (L _ (InjectivityAnn _ lhs rhs))) =
    do { lhs'   <- lookupBinder (unLoc lhs)
       ; rhs1   <- mapM (lookupBinder . unLoc) rhs
       ; rhs2   <- coreList nameTyConName rhs1
       ; injAnn <- rep2_nw injectivityAnnName [unC lhs', unC rhs2]
       ; coreJust injAnnTyConName injAnn }

repFamilyDecls :: [LFamilyDecl GhcRn] -> MetaM [Core (M TH.Dec)]
repFamilyDecls fds = liftM de_loc (mapM repFamilyDecl fds)

repAssocTyFamDefaultD :: TyFamDefltDecl GhcRn -> MetaM (Core (M TH.Dec))
repAssocTyFamDefaultD = repTyFamInstD

-------------------------
-- represent fundeps
--
repLFunDeps :: [LHsFunDep GhcRn] -> MetaM (Core [TH.FunDep])
repLFunDeps fds = repList funDepTyConName repLFunDep fds

repLFunDep :: LHsFunDep GhcRn -> MetaM (Core TH.FunDep)
repLFunDep (L _ (FunDep _ xs ys))
   = do xs' <- repList nameTyConName (lookupBinder . unLoc) xs
        ys' <- repList nameTyConName (lookupBinder . unLoc) ys
        repFunDep xs' ys'

-- Represent instance declarations
--
repInstD :: LInstDecl GhcRn -> MetaM (SrcSpan, Core (M TH.Dec))
repInstD (L loc (TyFamInstD { tfid_inst = fi_decl }))
  = do { dec <- repTyFamInstD fi_decl
       ; return (locA loc, dec) }
repInstD (L loc (DataFamInstD { dfid_inst = fi_decl }))
  = do { dec <- repDataFamInstD fi_decl
       ; return (locA loc, dec) }
repInstD (L loc (ClsInstD { cid_inst = cls_decl }))
  = do { dec <- repClsInstD cls_decl
       ; return (locA loc, dec) }

repClsInstD :: ClsInstDecl GhcRn -> MetaM (Core (M TH.Dec))
repClsInstD (ClsInstDecl { cid_poly_ty = ty, cid_binds = binds
                         , cid_sigs = sigs, cid_tyfam_insts = ats
                         , cid_datafam_insts = adts
                         , cid_overlap_mode = overlap
                         })
  = addSimpleTyVarBinds tvs $
            -- We must bring the type variables into scope, so their
            -- occurrences don't fail, even though the binders don't
            -- appear in the resulting data structure
            --
            -- But we do NOT bring the binders of 'binds' into scope
            -- because they are properly regarded as occurrences
            -- For example, the method names should be bound to
            -- the selector Ids, not to fresh names (#5410)
            --
            do { cxt1     <- repLContext cxt
               ; inst_ty1 <- repLTy inst_ty
          -- See Note [Scoped type variables in quotes]
               ; (ss, sigs_binds) <- rep_meth_sigs_binds sigs binds
               ; ats1   <- mapM (repTyFamInstD . unLoc) ats
               ; adts1  <- mapM (repDataFamInstD . unLoc) adts
               ; decls1 <- coreListM decTyConName (ats1 ++ adts1 ++ sigs_binds)
               ; rOver  <- repOverlap (fmap unLoc overlap)
               ; decls2 <- repInst rOver cxt1 inst_ty1 decls1
               ; wrapGenSyms ss decls2 }
 where
   (tvs, cxt, inst_ty) = splitLHsInstDeclTy ty

repStandaloneDerivD :: LDerivDecl GhcRn -> MetaM (SrcSpan, Core (M TH.Dec))
repStandaloneDerivD (L loc (DerivDecl { deriv_strategy = strat
                                       , deriv_type     = ty }))
  = do { dec <- repDerivStrategy strat  $ \strat' ->
                addSimpleTyVarBinds tvs $
                do { cxt'     <- repLContext cxt
                   ; inst_ty' <- repLTy inst_ty
                   ; repDeriv strat' cxt' inst_ty' }
       ; return (locA loc, dec) }
  where
    (tvs, cxt, inst_ty) = splitLHsInstDeclTy (dropWildCards ty)

repTyFamInstD :: TyFamInstDecl GhcRn -> MetaM (Core (M TH.Dec))
repTyFamInstD (TyFamInstDecl { tfid_eqn = eqn })
  = do { eqn1 <- repTyFamEqn eqn
       ; repTySynInst eqn1 }

repTyFamEqn :: TyFamInstEqn GhcRn -> MetaM (Core (M TH.TySynEqn))
repTyFamEqn (FamEqn { feqn_tycon = tc_name
                    , feqn_bndrs = outer_bndrs
                    , feqn_pats = tys
                    , feqn_fixity = fixity
                    , feqn_rhs  = rhs })
  = do { tc <- lookupLOcc tc_name     -- See note [Binders and occurrences]
       ; addHsOuterFamEqnTyVarBinds outer_bndrs $ \mb_exp_bndrs ->
         do { tys1 <- case fixity of
                        Prefix -> repTyArgs (repNamedTyCon tc) tys
                        Infix  -> do { (HsValArg t1: HsValArg t2: args) <- checkTys tys
                                     ; t1' <- repLTy t1
                                     ; t2'  <- repLTy t2
                                     ; repTyArgs (repTInfix t1' tc t2') args }
            ; rhs1 <- repLTy rhs
            ; repTySynEqn mb_exp_bndrs tys1 rhs1 } }
     where checkTys :: [LHsTypeArg GhcRn] -> MetaM [LHsTypeArg GhcRn]
           checkTys tys@(HsValArg _:HsValArg _:_) = return tys
           checkTys _ = panic "repTyFamEqn:checkTys"

repTyArgs :: MetaM (Core (M TH.Type)) -> [LHsTypeArg GhcRn] -> MetaM (Core (M TH.Type))
repTyArgs f [] = f
repTyArgs f (HsValArg ty : as) = do { f' <- f
                                    ; ty' <- repLTy ty
                                    ; repTyArgs (repTapp f' ty') as }
repTyArgs f (HsTypeArg _ ki : as) = do { f' <- f
                                       ; ki' <- repLTy ki
                                       ; repTyArgs (repTappKind f' ki') as }
repTyArgs f (HsArgPar _ : as) = repTyArgs f as

repDataFamInstD :: DataFamInstDecl GhcRn -> MetaM (Core (M TH.Dec))
repDataFamInstD (DataFamInstDecl { dfid_eqn =
                                      FamEqn { feqn_tycon = tc_name
                                             , feqn_bndrs = outer_bndrs
                                             , feqn_pats  = tys
                                             , feqn_fixity = fixity
                                             , feqn_rhs   = defn }})
  = do { tc <- lookupLOcc tc_name         -- See note [Binders and occurrences]
       ; addHsOuterFamEqnTyVarBinds outer_bndrs $ \mb_exp_bndrs ->
         do { tys1 <- case fixity of
                        Prefix -> repTyArgs (repNamedTyCon tc) tys
                        Infix  -> do { (HsValArg t1: HsValArg t2: args) <- checkTys tys
                                     ; t1' <- repLTy t1
                                     ; t2'  <- repLTy t2
                                     ; repTyArgs (repTInfix t1' tc t2') args }
            ; repDataDefn tc (Right (mb_exp_bndrs, tys1)) defn } }

      where checkTys :: [LHsTypeArg GhcRn] -> MetaM [LHsTypeArg GhcRn]
            checkTys tys@(HsValArg _: HsValArg _: _) = return tys
            checkTys _ = panic "repDataFamInstD:checkTys"

repForD :: LForeignDecl GhcRn -> MetaM (SrcSpan, Core (M TH.Dec))
repForD (L loc (ForeignImport { fd_name = name, fd_sig_ty = typ
                                  , fd_fi = CImport (L _ cc)
                                                    (L _ s) mch cis _ }))
 = do MkC name' <- lookupLOcc name
      MkC typ' <- repHsSigType typ
      MkC cc' <- repCCallConv cc
      MkC s' <- repSafety s
      cis' <- conv_cimportspec cis
      MkC str <- coreStringLit (static ++ chStr ++ cis')
      dec <- rep2 forImpDName [cc', s', str, name', typ']
      return (locA loc, dec)
 where
    conv_cimportspec (CLabel cls)
      = notHandled (ThForeignLabel cls)
    conv_cimportspec (CFunction DynamicTarget) = return "dynamic"
    conv_cimportspec (CFunction (StaticTarget _ fs _ True))
                            = return (unpackFS fs)
    conv_cimportspec (CFunction (StaticTarget _ _  _ False))
                            = panic "conv_cimportspec: values not supported yet"
    conv_cimportspec CWrapper = return "wrapper"
    -- these calling conventions do not support headers and the static keyword
    raw_cconv = cc == PrimCallConv || cc == JavaScriptCallConv
    static = case cis of
                 CFunction (StaticTarget _ _ _ _) | not raw_cconv -> "static "
                 _ -> ""
    chStr = case mch of
            Just (Header _ h) | not raw_cconv -> unpackFS h ++ " "
            _ -> ""
repForD decl@(L _ ForeignExport{}) = notHandled (ThForeignExport decl)

repCCallConv :: CCallConv -> MetaM (Core TH.Callconv)
repCCallConv CCallConv          = rep2_nw cCallName []
repCCallConv StdCallConv        = rep2_nw stdCallName []
repCCallConv CApiConv           = rep2_nw cApiCallName []
repCCallConv PrimCallConv       = rep2_nw primCallName []
repCCallConv JavaScriptCallConv = rep2_nw javaScriptCallName []

repSafety :: Safety -> MetaM (Core TH.Safety)
repSafety PlayRisky = rep2_nw unsafeName []
repSafety PlayInterruptible = rep2_nw interruptibleName []
repSafety PlaySafe = rep2_nw safeName []

repLFixD :: LFixitySig GhcRn -> MetaM [(SrcSpan, Core (M TH.Dec))]
repLFixD (L loc fix_sig) = rep_fix_d (locA loc) fix_sig

rep_fix_d :: SrcSpan -> FixitySig GhcRn -> MetaM [(SrcSpan, Core (M TH.Dec))]
rep_fix_d loc (FixitySig _ names (Fixity _ prec dir))
  = do { MkC prec' <- coreIntLit prec
       ; let rep_fn = case dir of
                        InfixL -> infixLDName
                        InfixR -> infixRDName
                        InfixN -> infixNDName
       ; let do_one name
              = do { MkC name' <- lookupLOcc name
                   ; dec <- rep2 rep_fn [prec', name']
                   ; return (loc,dec) }
       ; mapM do_one names }

repDefD :: LDefaultDecl GhcRn -> MetaM (SrcSpan, Core (M TH.Dec))
repDefD (L loc (DefaultDecl _ tys)) = do { tys1 <- repLTys tys
                                         ; MkC tys2 <- coreListM typeTyConName tys1
                                         ; dec <- rep2 defaultDName [tys2]
                                         ; return (locA loc, dec)}

repRuleD :: LRuleDecl GhcRn -> MetaM (SrcSpan, Core (M TH.Dec))
repRuleD (L loc (HsRule { rd_name = n
                        , rd_act = act
                        , rd_tyvs = ty_bndrs
                        , rd_tmvs = tm_bndrs
                        , rd_lhs = lhs
                        , rd_rhs = rhs }))
  = do { rule <- addHsTyVarBinds (fromMaybe [] ty_bndrs) $ \ ex_bndrs ->
         do { let tm_bndr_names = concatMap ruleBndrNames tm_bndrs
            ; ss <- mkGenSyms tm_bndr_names
            ; rule <- addBinds ss $
                      do { elt_ty <- wrapName tyVarBndrUnitTyConName
                         ; ty_bndrs' <- return $ case ty_bndrs of
                             Nothing -> coreNothing' (mkListTy elt_ty)
                             Just _  -> coreJust' (mkListTy elt_ty) ex_bndrs
                         ; tm_bndrs' <- repListM ruleBndrTyConName
                                                repRuleBndr
                                                tm_bndrs
                         ; n'   <- coreStringLit $ unpackFS $ snd $ unLoc n
                         ; act' <- repPhases act
                         ; lhs' <- repLE lhs
                         ; rhs' <- repLE rhs
                         ; repPragRule n' ty_bndrs' tm_bndrs' lhs' rhs' act' }
           ; wrapGenSyms ss rule  }
       ; return (locA loc, rule) }

ruleBndrNames :: LRuleBndr GhcRn -> [Name]
ruleBndrNames (L _ (RuleBndr _ n))      = [unLoc n]
ruleBndrNames (L _ (RuleBndrSig _ n sig))
  | HsPS { hsps_ext = HsPSRn { hsps_imp_tvs = vars }} <- sig
  = unLoc n : vars

repRuleBndr :: LRuleBndr GhcRn -> MetaM (Core (M TH.RuleBndr))
repRuleBndr (L _ (RuleBndr _ n))
  = do { MkC n' <- lookupNBinder n
       ; rep2 ruleVarName [n'] }
repRuleBndr (L _ (RuleBndrSig _ n sig))
  = do { MkC n'  <- lookupNBinder n
       ; MkC ty' <- repLTy (hsPatSigType sig)
       ; rep2 typedRuleVarName [n', ty'] }

repAnnD :: LAnnDecl GhcRn -> MetaM (SrcSpan, Core (M TH.Dec))
repAnnD (L loc (HsAnnotation _ _ ann_prov (L _ exp)))
  = do { target <- repAnnProv ann_prov
       ; exp'   <- repE exp
       ; dec    <- repPragAnn target exp'
       ; return (locA loc, dec) }

repAnnProv :: AnnProvenance GhcRn -> MetaM (Core TH.AnnTarget)
repAnnProv (ValueAnnProvenance n)
  = do { -- An ANN references an identifier bound elsewhere in the module, so
         -- we must look it up using lookupLOcc (#19377).
         -- Similarly for TypeAnnProvenance (`ANN type`) below.
         MkC n' <- lookupLOcc n
       ; rep2_nw valueAnnotationName [ n' ] }
repAnnProv (TypeAnnProvenance n)
  = do { MkC n' <- lookupLOcc n
       ; rep2_nw typeAnnotationName [ n' ] }
repAnnProv ModuleAnnProvenance
  = rep2_nw moduleAnnotationName []

-------------------------------------------------------
--                      Constructors
-------------------------------------------------------

repC :: LConDecl GhcRn -> MetaM (Core (M TH.Con))
repC (L _ (ConDeclH98 { con_name   = con
                      , con_forall = False
                      , con_mb_cxt = Nothing
                      , con_args   = args }))
  = repH98DataCon con args

repC (L _ (ConDeclH98 { con_name = con
                      , con_forall = is_existential
                      , con_ex_tvs = con_tvs
                      , con_mb_cxt = mcxt
                      , con_args = args }))
  = addHsTyVarBinds con_tvs $ \ ex_bndrs ->
         do { c'    <- repH98DataCon con args
            ; ctxt' <- repMbContext mcxt
            ; if not is_existential && isNothing mcxt
              then return c'
              else rep2 forallCName ([unC ex_bndrs, unC ctxt', unC c'])
            }

repC (L _ (ConDeclGADT { con_names  = cons
                       , con_bndrs  = L _ outer_bndrs
                       , con_mb_cxt = mcxt
                       , con_g_args = args
                       , con_res_ty = res_ty }))
  | null_outer_imp_tvs && null_outer_exp_tvs
                                 -- No implicit or explicit variables
  , Nothing <- mcxt              -- No context
                                 -- ==> no need for a forall
  = repGadtDataCons cons args res_ty

  | otherwise
  = addHsOuterSigTyVarBinds outer_bndrs $ \ outer_bndrs' ->
             -- See Note [Don't quantify implicit type variables in quotes]
    do { c'    <- repGadtDataCons cons args res_ty
       ; ctxt' <- repMbContext mcxt
       ; if null_outer_exp_tvs && isNothing mcxt
         then return c'
         else rep2 forallCName ([unC outer_bndrs', unC ctxt', unC c']) }
  where
    null_outer_imp_tvs = nullOuterImplicit outer_bndrs
    null_outer_exp_tvs = nullOuterExplicit outer_bndrs

repMbContext :: Maybe (LHsContext GhcRn) -> MetaM (Core (M TH.Cxt))
repMbContext Nothing          = repContext []
repMbContext (Just (L _ cxt)) = repContext cxt

repSrcUnpackedness :: SrcUnpackedness -> MetaM (Core (M TH.SourceUnpackedness))
repSrcUnpackedness SrcUnpack   = rep2 sourceUnpackName         []
repSrcUnpackedness SrcNoUnpack = rep2 sourceNoUnpackName       []
repSrcUnpackedness NoSrcUnpack = rep2 noSourceUnpackednessName []

repSrcStrictness :: SrcStrictness -> MetaM (Core (M TH.SourceStrictness))
repSrcStrictness SrcLazy     = rep2 sourceLazyName         []
repSrcStrictness SrcStrict   = rep2 sourceStrictName       []
repSrcStrictness NoSrcStrict = rep2 noSourceStrictnessName []

repBangTy :: LBangType GhcRn -> MetaM (Core (M TH.BangType))
repBangTy ty = do
  MkC u <- repSrcUnpackedness su'
  MkC s <- repSrcStrictness ss'
  MkC b <- rep2 bangName [u, s]
  MkC t <- repLTy ty'
  rep2 bangTypeName [b, t]
  where
    (su', ss', ty') = case unLoc ty of
            HsBangTy _ (HsSrcBang _ su ss) ty -> (su, ss, ty)
            _ -> (NoSrcUnpack, NoSrcStrict, ty)

-------------------------------------------------------
--                      Deriving clauses
-------------------------------------------------------

repDerivs :: HsDeriving GhcRn -> MetaM (Core [M TH.DerivClause])
repDerivs clauses
  = repListM derivClauseTyConName repDerivClause clauses

repDerivClause :: LHsDerivingClause GhcRn
               -> MetaM (Core (M TH.DerivClause))
repDerivClause (L _ (HsDerivingClause
                          { deriv_clause_strategy = dcs
                          , deriv_clause_tys      = dct }))
  = repDerivStrategy dcs $ \(MkC dcs') ->
    do MkC dct' <- rep_deriv_clause_tys dct
       rep2 derivClauseName [dcs',dct']
  where
    rep_deriv_clause_tys :: LDerivClauseTys GhcRn -> MetaM (Core [M TH.Type])
    rep_deriv_clause_tys (L _ dct) = case dct of
      DctSingle _ ty -> rep_deriv_tys [ty]
      DctMulti _ tys -> rep_deriv_tys tys

    rep_deriv_tys :: [LHsSigType GhcRn] -> MetaM (Core [M TH.Type])
    rep_deriv_tys = repListM typeTyConName repHsSigType

rep_meth_sigs_binds :: [LSig GhcRn] -> LHsBinds GhcRn
                    -> MetaM ([GenSymBind], [Core (M TH.Dec)])
-- Represent signatures and methods in class/instance declarations.
-- See Note [Scoped type variables in quotes]
--
-- Why not use 'repBinds': we have already created symbols for methods in
-- 'repTopDs' via 'hsGroupBinders'. However in 'repBinds', we recreate
-- these fun_id via 'collectHsValBinders decs', which would lead to the
-- instance declarations failing in TH.
rep_meth_sigs_binds sigs binds
  = do { let tvs = concatMap get_scoped_tvs sigs
       ; ss <- mkGenSyms tvs
       ; sigs1 <- addBinds ss $ rep_sigs sigs
       ; binds1 <- addBinds ss $ rep_binds binds
       ; return (ss, de_loc (sort_by_loc (sigs1 ++ binds1))) }

-------------------------------------------------------
--   Signatures in a class decl, or a group of bindings
-------------------------------------------------------

rep_sigs :: [LSig GhcRn] -> MetaM [(SrcSpan, Core (M TH.Dec))]
        -- We silently ignore ones we don't recognise
rep_sigs = concatMapM rep_sig

rep_sig :: LSig GhcRn -> MetaM [(SrcSpan, Core (M TH.Dec))]
rep_sig (L loc (TypeSig _ nms ty))
  = mapM (rep_wc_ty_sig sigDName (locA loc) ty) nms
rep_sig (L loc (PatSynSig _ nms ty))
  = mapM (rep_patsyn_ty_sig (locA loc) ty) nms
rep_sig (L loc (ClassOpSig _ is_deflt nms ty))
  | is_deflt     = mapM (rep_ty_sig defaultSigDName (locA loc) ty) nms
  | otherwise    = mapM (rep_ty_sig sigDName (locA loc) ty) nms
rep_sig d@(L _ (IdSig {}))           = pprPanic "rep_sig IdSig" (ppr d)
rep_sig (L loc (FixSig _ fix_sig))   = rep_fix_d (locA loc) fix_sig
rep_sig (L loc (InlineSig _ nm ispec))= rep_inline nm ispec (locA loc)
rep_sig (L loc (SpecSig _ nm tys ispec))
  = concatMapM (\t -> rep_specialise nm t ispec (locA loc)) tys
rep_sig (L loc (SpecInstSig _ _ ty))  = rep_specialiseInst ty (locA loc)
rep_sig (L _   (MinimalSig {}))       = notHandled ThMinimalPragmas
rep_sig (L _   (SCCFunSig {}))        = notHandled ThSCCPragmas
rep_sig (L loc (CompleteMatchSig _ _st cls mty))
  = rep_complete_sig cls mty (locA loc)

-- Desugar the explicit type variable binders in an 'LHsSigType', making
-- sure not to gensym them.
-- See Note [Scoped type variables in quotes]
-- and Note [Don't quantify implicit type variables in quotes]
rep_ty_sig_tvs :: [LHsTyVarBndr Specificity GhcRn]
               -> MetaM (Core [M TH.TyVarBndrSpec])
rep_ty_sig_tvs explicit_tvs
  = repListM tyVarBndrSpecTyConName repTyVarBndr
             explicit_tvs

-- Desugar the outer type variable binders in an 'LHsSigType', making
-- sure not to gensym them.
-- See Note [Scoped type variables in quotes]
-- and Note [Don't quantify implicit type variables in quotes]
rep_ty_sig_outer_tvs :: HsOuterSigTyVarBndrs GhcRn
                     -> MetaM (Core [M TH.TyVarBndrSpec])
rep_ty_sig_outer_tvs (HsOuterImplicit{}) =
  coreListM tyVarBndrSpecTyConName []
rep_ty_sig_outer_tvs (HsOuterExplicit{hso_bndrs = explicit_tvs}) =
  rep_ty_sig_tvs explicit_tvs

-- Desugar a top-level type signature. Unlike 'repHsSigType', this
-- deliberately avoids gensymming the type variables.
-- See Note [Scoped type variables in quotes]
-- and Note [Don't quantify implicit type variables in quotes]
rep_ty_sig :: Name -> SrcSpan -> LHsSigType GhcRn -> LocatedN Name
           -> MetaM (SrcSpan, Core (M TH.Dec))
rep_ty_sig mk_sig loc sig_ty nm
  = do { nm1 <- lookupLOcc nm
       ; ty1 <- rep_ty_sig' sig_ty
       ; sig <- repProto mk_sig nm1 ty1
       ; return (loc, sig) }

-- Desugar an 'LHsSigType', making sure not to gensym the type variables at
-- the front of the type signature.
-- See Note [Scoped type variables in quotes]
-- and Note [Don't quantify implicit type variables in quotes]
rep_ty_sig' :: LHsSigType GhcRn
            -> MetaM (Core (M TH.Type))
rep_ty_sig' (L _ (HsSig{sig_bndrs = outer_bndrs, sig_body = body}))
  | (ctxt, tau) <- splitLHsQualTy body
  = do { th_explicit_tvs <- rep_ty_sig_outer_tvs outer_bndrs
       ; th_ctxt <- repLContext ctxt
       ; th_tau  <- repLTy tau
       ; if nullOuterExplicit outer_bndrs && null (fromMaybeContext ctxt)
            then return th_tau
            else repTForall th_explicit_tvs th_ctxt th_tau }

rep_patsyn_ty_sig :: SrcSpan -> LHsSigType GhcRn -> LocatedN Name
                  -> MetaM (SrcSpan, Core (M TH.Dec))
-- represents a pattern synonym type signature;
-- see Note [Pattern synonym type signatures and Template Haskell] in "GHC.ThToHs"
--
-- Don't create the implicit and explicit variables when desugaring signatures,
-- see Note [Scoped type variables in quotes]
-- and Note [Don't quantify implicit type variables in quotes]
rep_patsyn_ty_sig loc sig_ty nm
  | (univs, reqs, exis, provs, ty) <- splitLHsPatSynTy sig_ty
  = do { nm1 <- lookupLOcc nm
       ; th_univs <- rep_ty_sig_tvs univs
       ; th_exis  <- rep_ty_sig_tvs exis

       ; th_reqs  <- repLContext reqs
       ; th_provs <- repLContext provs
       ; th_ty    <- repLTy ty
       ; ty1      <- repTForall th_univs th_reqs =<<
                       repTForall th_exis th_provs th_ty
       ; sig      <- repProto patSynSigDName nm1 ty1
       ; return (loc, sig) }

rep_wc_ty_sig :: Name -> SrcSpan -> LHsSigWcType GhcRn -> LocatedN Name
              -> MetaM (SrcSpan, Core (M TH.Dec))
rep_wc_ty_sig mk_sig loc sig_ty nm
  = rep_ty_sig mk_sig loc (hswc_body sig_ty) nm

rep_inline :: LocatedN Name
           -> InlinePragma      -- Never defaultInlinePragma
           -> SrcSpan
           -> MetaM [(SrcSpan, Core (M TH.Dec))]
rep_inline nm ispec loc
  = do { nm1    <- lookupLOcc nm
       ; inline <- repInline $ inl_inline ispec
       ; rm     <- repRuleMatch $ inl_rule ispec
       ; phases <- repPhases $ inl_act ispec
       ; pragma <- repPragInl nm1 inline rm phases
       ; return [(loc, pragma)]
       }

rep_specialise :: LocatedN Name -> LHsSigType GhcRn -> InlinePragma
               -> SrcSpan
               -> MetaM [(SrcSpan, Core (M TH.Dec))]
rep_specialise nm ty ispec loc
  = do { nm1 <- lookupLOcc nm
       ; ty1 <- repHsSigType ty
       ; phases <- repPhases $ inl_act ispec
       ; let inline = inl_inline ispec
       ; pragma <- if noUserInlineSpec inline
                   then -- SPECIALISE
                     repPragSpec nm1 ty1 phases
                   else -- SPECIALISE INLINE
                     do { inline1 <- repInline inline
                        ; repPragSpecInl nm1 ty1 inline1 phases }
       ; return [(loc, pragma)]
       }

rep_specialiseInst :: LHsSigType GhcRn -> SrcSpan
                   -> MetaM [(SrcSpan, Core (M TH.Dec))]
rep_specialiseInst ty loc
  = do { ty1    <- repHsSigType ty
       ; pragma <- repPragSpecInst ty1
       ; return [(loc, pragma)] }

repInline :: InlineSpec -> MetaM (Core TH.Inline)
repInline (NoInline          _ )   = dataCon noInlineDataConName
repInline (Inline            _ )   = dataCon inlineDataConName
repInline (Inlinable         _ )   = dataCon inlinableDataConName
repInline NoUserInlinePrag        = notHandled ThNoUserInline

repRuleMatch :: RuleMatchInfo -> MetaM (Core TH.RuleMatch)
repRuleMatch ConLike = dataCon conLikeDataConName
repRuleMatch FunLike = dataCon funLikeDataConName

repPhases :: Activation -> MetaM (Core TH.Phases)
repPhases (ActiveBefore _ i) = do { MkC arg <- coreIntLit i
                                  ; dataCon' beforePhaseDataConName [arg] }
repPhases (ActiveAfter _ i)  = do { MkC arg <- coreIntLit i
                                  ; dataCon' fromPhaseDataConName [arg] }
repPhases _                  = dataCon allPhasesDataConName

rep_complete_sig :: Located [LocatedN Name]
                 -> Maybe (LocatedN Name)
                 -> SrcSpan
                 -> MetaM [(SrcSpan, Core (M TH.Dec))]
rep_complete_sig (L _ cls) mty loc
  = do { mty' <- repMaybe nameTyConName lookupLOcc mty
       ; cls' <- repList nameTyConName lookupLOcc cls
       ; sig <- repPragComplete cls' mty'
       ; return [(loc, sig)] }

-------------------------------------------------------
--                      Types
-------------------------------------------------------

class RepTV flag flag' | flag -> flag' where
    tyVarBndrName :: Name
    repPlainTV  :: Core TH.Name -> flag -> MetaM (Core (M (TH.TyVarBndr flag')))
    repKindedTV :: Core TH.Name -> flag -> Core (M TH.Kind)
                -> MetaM (Core (M (TH.TyVarBndr flag')))

instance RepTV () () where
    tyVarBndrName = tyVarBndrUnitTyConName
    repPlainTV  (MkC nm) ()          = rep2 plainTVName  [nm]
    repKindedTV (MkC nm) () (MkC ki) = rep2 kindedTVName [nm, ki]

instance RepTV Specificity TH.Specificity where
    tyVarBndrName = tyVarBndrSpecTyConName
    repPlainTV  (MkC nm) spec          = do { (MkC spec') <- rep_flag spec
                                            ; rep2 plainInvisTVName  [nm, spec'] }
    repKindedTV (MkC nm) spec (MkC ki) = do { (MkC spec') <- rep_flag spec
                                            ; rep2 kindedInvisTVName [nm, spec', ki] }

rep_flag :: Specificity -> MetaM (Core TH.Specificity)
rep_flag SpecifiedSpec = rep2_nw specifiedSpecName []
rep_flag InferredSpec  = rep2_nw inferredSpecName []

addHsOuterFamEqnTyVarBinds ::
     HsOuterFamEqnTyVarBndrs GhcRn
  -> (Core (Maybe [M TH.TyVarBndrUnit]) -> MetaM (Core (M a)))
  -> MetaM (Core (M a))
addHsOuterFamEqnTyVarBinds outer_bndrs thing_inside = do
  elt_ty <- wrapName tyVarBndrUnitTyConName
  case outer_bndrs of
    HsOuterImplicit{hso_ximplicit = imp_tvs} ->
      addTyClTyVarBinds (mk_qtvs imp_tvs []) $ \_th_exp_bndrs ->
      thing_inside $ coreNothingList elt_ty
    HsOuterExplicit{hso_bndrs = exp_bndrs} ->
      addTyClTyVarBinds (mk_qtvs [] exp_bndrs) $ \th_exp_bndrs ->
      thing_inside $ coreJustList elt_ty th_exp_bndrs
  where
    mk_qtvs imp_tvs exp_tvs = HsQTvs { hsq_ext = imp_tvs
                                     , hsq_explicit = exp_tvs }

addHsOuterSigTyVarBinds ::
     HsOuterSigTyVarBndrs GhcRn
  -> (Core [M TH.TyVarBndrSpec] -> MetaM (Core (M a)))
  -> MetaM (Core (M a))
addHsOuterSigTyVarBinds outer_bndrs thing_inside = case outer_bndrs of
  HsOuterImplicit{hso_ximplicit = imp_tvs} ->
    do th_nil <- coreListM tyVarBndrSpecTyConName []
       addSimpleTyVarBinds imp_tvs $ thing_inside th_nil
  HsOuterExplicit{hso_bndrs = exp_bndrs} ->
    addHsTyVarBinds exp_bndrs thing_inside

-- | If a type implicitly quantifies its outermost type variables, return
-- 'True' if the list of implicitly bound type variables is empty. If a type
-- explicitly quantifies its outermost type variables, always return 'True'.
--
-- This is used in various places to determine if a Template Haskell 'Type'
-- should be headed by a 'ForallT' or not.
nullOuterImplicit :: HsOuterSigTyVarBndrs GhcRn -> Bool
nullOuterImplicit (HsOuterImplicit{hso_ximplicit = imp_tvs}) = null imp_tvs
nullOuterImplicit (HsOuterExplicit{})                        = True
  -- Vacuously true, as there is no implicit quantification

-- | If a type explicitly quantifies its outermost type variables, return
-- 'True' if the list of explicitly bound type variables is empty. If a type
-- implicitly quantifies its outermost type variables, always return 'True'.
--
-- This is used in various places to determine if a Template Haskell 'Type'
-- should be headed by a 'ForallT' or not.
nullOuterExplicit :: HsOuterSigTyVarBndrs GhcRn -> Bool
nullOuterExplicit (HsOuterExplicit{hso_bndrs = exp_bndrs}) = null exp_bndrs
nullOuterExplicit (HsOuterImplicit{})                      = True
  -- Vacuously true, as there is no outermost explicit quantification

addSimpleTyVarBinds :: [Name]             -- the binders to be added
                    -> MetaM (Core (M a)) -- action in the ext env
                    -> MetaM (Core (M a))
addSimpleTyVarBinds names thing_inside
  = do { fresh_names <- mkGenSyms names
       ; term <- addBinds fresh_names thing_inside
       ; wrapGenSyms fresh_names term }

addHsTyVarBinds :: forall flag flag' a. RepTV flag flag'
                => [LHsTyVarBndr flag GhcRn] -- the binders to be added
                -> (Core [(M (TH.TyVarBndr flag'))] -> MetaM (Core (M a))) -- action in the ext env
                -> MetaM (Core (M a))
addHsTyVarBinds exp_tvs thing_inside
  = do { fresh_exp_names <- mkGenSyms (hsLTyVarNames exp_tvs)
       ; term <- addBinds fresh_exp_names $
                 do { kbs <- repListM (tyVarBndrName @flag @flag') repTyVarBndr
                                      exp_tvs
                    ; thing_inside kbs }
       ; wrapGenSyms fresh_exp_names term }

addQTyVarBinds :: LHsQTyVars GhcRn -- the binders to be added
               -> (Core [(M (TH.TyVarBndr ()))] -> MetaM (Core (M a))) -- action in the ext env
               -> MetaM (Core (M a))
addQTyVarBinds (HsQTvs { hsq_ext = imp_tvs
                      , hsq_explicit = exp_tvs })
              thing_inside
  = addTyVarBinds exp_tvs imp_tvs thing_inside

addTyVarBinds :: RepTV flag flag'
              => [LHsTyVarBndr flag GhcRn] -- the binders to be added
              -> [Name]
              -> (Core [(M (TH.TyVarBndr flag'))] -> MetaM (Core (M a))) -- action in the ext env
              -> MetaM (Core (M a))
-- gensym a list of type variables and enter them into the meta environment;
-- the computations passed as the second argument is executed in that extended
-- meta environment and gets the *new* names on Core-level as an argument
addTyVarBinds exp_tvs imp_tvs thing_inside
  = addSimpleTyVarBinds imp_tvs $
    addHsTyVarBinds exp_tvs $
    thing_inside

addTyClTyVarBinds :: LHsQTyVars GhcRn
                  -> (Core [(M (TH.TyVarBndr ()))] -> MetaM (Core (M a)))
                  -> MetaM (Core (M a))
-- Used for data/newtype declarations, and family instances,
-- so that the nested type variables work right
--    instance C (T a) where
--      type W (T a) = blah
-- The 'a' in the type instance is the one bound by the instance decl
addTyClTyVarBinds tvs m
  = do { let tv_names = hsAllLTyVarNames tvs
       ; env <- lift $ dsGetMetaEnv
       ; freshNames <- mkGenSyms (filterOut (`elemNameEnv` env) tv_names)
            -- Make fresh names for the ones that are not already in scope
            -- This makes things work for family declarations

       ; term <- addBinds freshNames $
                 do { kbs <- repListM tyVarBndrUnitTyConName repTyVarBndr
                                     (hsQTvExplicit tvs)
                    ; m kbs }

       ; wrapGenSyms freshNames term }

-- | Represent a type variable binder
repTyVarBndr :: RepTV flag flag'
             => LHsTyVarBndr flag GhcRn -> MetaM (Core (M (TH.TyVarBndr flag')))
repTyVarBndr (L _ (UserTyVar _ fl (L _ nm)) )
  = do { nm' <- lookupBinder nm
       ; repPlainTV nm' fl }
repTyVarBndr (L _ (KindedTyVar _ fl (L _ nm) ki))
  = do { nm' <- lookupBinder nm
       ; ki' <- repLTy ki
       ; repKindedTV nm' fl ki' }

-- represent a type context
--
repLContext :: Maybe (LHsContext GhcRn) -> MetaM (Core (M TH.Cxt))
repLContext Nothing = repContext []
repLContext (Just ctxt) = repContext (unLoc ctxt)

repContext :: HsContext GhcRn -> MetaM (Core (M TH.Cxt))
repContext ctxt = do preds <- repListM typeTyConName repLTy ctxt
                     repCtxt preds

repHsSigType :: LHsSigType GhcRn -> MetaM (Core (M TH.Type))
repHsSigType (L _ (HsSig { sig_bndrs = outer_bndrs, sig_body = body }))
  | (ctxt, tau) <- splitLHsQualTy body
  = addHsOuterSigTyVarBinds outer_bndrs $ \ th_outer_bndrs ->
    do { th_ctxt <- repLContext ctxt
       ; th_tau  <- repLTy tau
       ; if nullOuterExplicit outer_bndrs && null (fromMaybeContext ctxt)
         then pure th_tau
         else repTForall th_outer_bndrs th_ctxt th_tau }

-- yield the representation of a list of types
repLTys :: [LHsType GhcRn] -> MetaM [Core (M TH.Type)]
repLTys tys = mapM repLTy tys

-- represent a type
repLTy :: LHsType GhcRn -> MetaM (Core (M TH.Type))
repLTy ty = repTy (unLoc ty)

-- Desugar a type headed by an invisible forall (e.g., @forall a. a@) or
-- a context (e.g., @Show a => a@) into a ForallT from L.H.TH.Syntax.
-- In other words, the argument to this function is always an
-- @HsForAllTy HsForAllInvis{}@ or @HsQualTy@.
-- Types headed by visible foralls (which are desugared to ForallVisT) are
-- handled separately in repTy.
repForallT :: HsType GhcRn -> MetaM (Core (M TH.Type))
repForallT ty
 | (tvs, ctxt, tau) <- splitLHsSigmaTyInvis (noLocA ty)
 = addHsTyVarBinds tvs $ \bndrs ->
   do { ctxt1  <- repLContext ctxt
      ; tau1   <- repLTy tau
      ; repTForall bndrs ctxt1 tau1 -- forall a. C a => {...}
      }

repTy :: HsType GhcRn -> MetaM (Core (M TH.Type))
repTy ty@(HsForAllTy { hst_tele = tele, hst_body = body }) =
  case tele of
    HsForAllInvis{} -> repForallT ty
    HsForAllVis { hsf_vis_bndrs = tvs } ->
      addHsTyVarBinds tvs $ \bndrs ->
      do body1 <- repLTy body
         repTForallVis bndrs body1
repTy ty@(HsQualTy {}) = repForallT ty

repTy (HsTyVar _ _ (L _ n))
  | isLiftedTypeKindTyConName n        = repTStar
  | n `hasKey` constraintKindTyConKey  = repTConstraint
  | n `hasKey` unrestrictedFunTyConKey = repArrowTyCon
  | n `hasKey` funTyConKey             = repMulArrowTyCon
  | isTvOcc occ   = do tv1 <- lookupOcc n
                       repTvar tv1
  | isDataOcc occ = do tc1 <- lookupOcc n
                       repPromotedDataCon tc1
  | n == eqTyConName = repTequality
  | otherwise     = do tc1 <- lookupOcc n
                       repNamedTyCon tc1
  where
    occ = nameOccName n

repTy (HsAppTy _ f a)       = do
                                f1 <- repLTy f
                                a1 <- repLTy a
                                repTapp f1 a1
repTy (HsAppKindTy _ ty ki) = do
                                ty1 <- repLTy ty
                                ki1 <- repLTy ki
                                repTappKind ty1 ki1
repTy (HsFunTy _ w f a) | isUnrestricted w = do
                                f1   <- repLTy f
                                a1   <- repLTy a
                                tcon <- repArrowTyCon
                                repTapps tcon [f1, a1]
repTy (HsFunTy _ w f a) = do w1   <- repLTy (arrowToHsType w)
                             f1   <- repLTy f
                             a1   <- repLTy a
                             tcon <- repMulArrowTyCon
                             repTapps tcon [w1, f1, a1]
repTy (HsListTy _ t)        = do
                                t1   <- repLTy t
                                tcon <- repListTyCon
                                repTapp tcon t1
repTy (HsTupleTy _ HsUnboxedTuple tys) = do
                                tys1 <- repLTys tys
                                tcon <- repUnboxedTupleTyCon (length tys)
                                repTapps tcon tys1
repTy (HsTupleTy _ _ tys)   = do tys1 <- repLTys tys
                                 tcon <- repTupleTyCon (length tys)
                                 repTapps tcon tys1
repTy (HsSumTy _ tys)       = do tys1 <- repLTys tys
                                 tcon <- repUnboxedSumTyCon (length tys)
                                 repTapps tcon tys1
repTy (HsOpTy _ ty1 n ty2)  = repLTy ((nlHsTyVar (unLoc n) `nlHsAppTy` ty1)
                                   `nlHsAppTy` ty2)
repTy (HsParTy _ t)         = repLTy t
repTy (HsStarTy _ _) =  repTStar
repTy (HsKindSig _ t k)     = do
                                t1 <- repLTy t
                                k1 <- repLTy k
                                repTSig t1 k1
repTy (HsSpliceTy _ splice)      = repSplice splice
repTy (HsExplicitListTy _ _ tys) = do
                                    tys1 <- repLTys tys
                                    repTPromotedList tys1
repTy (HsExplicitTupleTy _ tys) = do
                                    tys1 <- repLTys tys
                                    tcon <- repPromotedTupleTyCon (length tys)
                                    repTapps tcon tys1
repTy (HsTyLit _ lit) = do
                          lit' <- repTyLit lit
                          repTLit lit'
repTy (HsWildCardTy _) = repTWildCard
repTy (HsIParamTy _ n t) = do
                             n' <- rep_implicit_param_name (unLoc n)
                             t' <- repLTy t
                             repTImplicitParam n' t'

repTy ty                      = notHandled (ThExoticFormOfType ty)

repTyLit :: HsTyLit -> MetaM (Core (M TH.TyLit))
repTyLit (HsNumTy _ i) = do
                         platform <- getPlatform
                         rep2 numTyLitName [mkIntegerExpr platform i]
repTyLit (HsStrTy _ s) = do { s' <- mkStringExprFS s
                            ; rep2 strTyLitName [s']
                            }
repTyLit (HsCharTy _ c) = do { c' <- return (mkCharExpr c)
                             ; rep2 charTyLitName [c']
                             }

-- | Represent a type wrapped in a Maybe
repMaybeLTy :: Maybe (LHsKind GhcRn)
            -> MetaM (Core (Maybe (M TH.Type)))
repMaybeLTy m = do
  k_ty <- wrapName kindTyConName
  repMaybeT k_ty repLTy m

repRole :: LocatedAn NoEpAnns (Maybe Role) -> MetaM (Core TH.Role)
repRole (L _ (Just Nominal))          = rep2_nw nominalRName []
repRole (L _ (Just Representational)) = rep2_nw representationalRName []
repRole (L _ (Just Phantom))          = rep2_nw phantomRName []
repRole (L _ Nothing)                 = rep2_nw inferRName []

-----------------------------------------------------------------------------
--              Splices
-----------------------------------------------------------------------------

repSplice :: HsSplice GhcRn -> MetaM (Core a)
-- See Note [How brackets and nested splices are handled] in GHC.Tc.Gen.Splice
-- We return a CoreExpr of any old type; the context should know
repSplice (HsTypedSplice   _ _ n _) = rep_splice n
repSplice (HsUntypedSplice _ _ n _) = rep_splice n
repSplice (HsQuasiQuote _ n _ _ _)  = rep_splice n
repSplice e@(HsSpliced {})          = pprPanic "repSplice" (ppr e)

rep_splice :: Name -> MetaM (Core a)
rep_splice splice_name
 = do { mb_val <- lift $ dsLookupMetaEnv splice_name
       ; case mb_val of
           Just (DsSplice e) -> do { e' <- lift $ dsExpr e
                                   ; return (MkC e') }
           _ -> pprPanic "HsSplice" (ppr splice_name) }
                        -- Should not happen; statically checked

-----------------------------------------------------------------------------
--              Expressions
-----------------------------------------------------------------------------

repLEs :: [LHsExpr GhcRn] -> MetaM (Core [(M TH.Exp)])
repLEs es = repListM expTyConName repLE es

-- FIXME: some of these panics should be converted into proper error messages
--        unless we can make sure that constructs, which are plainly not
--        supported in TH already lead to error messages at an earlier stage
repLE :: LHsExpr GhcRn -> MetaM (Core (M TH.Exp))
repLE (L loc e) = mapReaderT (putSrcSpanDs (locA loc)) (repE e)

repE :: HsExpr GhcRn -> MetaM (Core (M TH.Exp))
repE (HsVar _ (L _ x)) =
  do { mb_val <- lift $ dsLookupMetaEnv x
     ; case mb_val of
        Nothing            -> do { str <- lift $ globalVar x
                                 ; repVarOrCon x str }
        Just (DsBound y)   -> repVarOrCon x (coreVar y)
        Just (DsSplice e)  -> do { e' <- lift $ dsExpr e
                                 ; return (MkC e') } }
repE (HsIPVar _ n) = rep_implicit_param_name n >>= repImplicitParamVar
repE (HsOverLabel _ s) = repOverLabel s

repE (HsRecSel _ (FieldOcc x _)) = repE (HsVar noExtField (noLocA x))

        -- Remember, we're desugaring renamer output here, so
        -- HsOverlit can definitely occur
repE (HsOverLit _ l) = do { a <- repOverloadedLiteral l; repLit a }
repE (HsLit _ l)     = do { a <- repLiteral l;           repLit a }
repE (HsLam _ (MG { mg_alts = (L _ [m]) })) = repLambda m
repE e@(HsLam _ (MG { mg_alts = (L _ _) })) = pprPanic "repE: HsLam with multiple alternatives" (ppr e)
repE (HsLamCase _ (MG { mg_alts = (L _ ms) }))
                   = do { ms' <- mapM repMatchTup ms
                        ; core_ms <- coreListM matchTyConName ms'
                        ; repLamCase core_ms }
repE (HsApp _ x y)   = do {a <- repLE x; b <- repLE y; repApp a b}
repE (HsAppType _ e t) = do { a <- repLE e
                            ; s <- repLTy (hswc_body t)
                            ; repAppType a s }

repE (OpApp _ e1 op e2) =
  do { arg1 <- repLE e1;
       arg2 <- repLE e2;
       the_op <- repLE op ;
       repInfixApp arg1 the_op arg2 }
repE (NegApp _ x _)      = do
                              a         <- repLE x
                              negateVar <- lookupOcc negateName >>= repVar
                              negateVar `repApp` a
repE (HsPar _ _ x _)        = repLE x
repE (SectionL _ x y)       = do { a <- repLE x; b <- repLE y; repSectionL a b }
repE (SectionR _ x y)       = do { a <- repLE x; b <- repLE y; repSectionR a b }
repE (HsCase _ e (MG { mg_alts = (L _ ms) }))
                          = do { arg <- repLE e
                               ; ms2 <- mapM repMatchTup ms
                               ; core_ms2 <- coreListM matchTyConName ms2
                               ; repCaseE arg core_ms2 }
repE (HsIf _ x y z)       = do
                            a <- repLE x
                            b <- repLE y
                            c <- repLE z
                            repCond a b c
repE (HsMultiIf _ alts)
  = do { (binds, alts') <- liftM unzip $ mapM repLGRHS alts
       ; expr' <- repMultiIf (nonEmptyCoreList alts')
       ; wrapGenSyms (concat binds) expr' }
repE (HsLet _ _ bs _ e)         = do { (ss,ds) <- repBinds bs
                                     ; e2 <- addBinds ss (repLE e)
                                     ; z <- repLetE ds e2
                                     ; wrapGenSyms ss z }

-- FIXME: I haven't got the types here right yet
repE e@(HsDo _ ctxt (L _ sts))
 | Just maybeModuleName <- case ctxt of
     { DoExpr m -> Just m; GhciStmtCtxt -> Just Nothing; _ -> Nothing }
 = do { (ss,zs) <- repLSts sts;
        e'      <- repDoE maybeModuleName (nonEmptyCoreList zs);
        wrapGenSyms ss e' }

 | ListComp <- ctxt
 = do { (ss,zs) <- repLSts sts;
        e'      <- repComp (nonEmptyCoreList zs);
        wrapGenSyms ss e' }

 | MDoExpr maybeModuleName <- ctxt
 = do { (ss,zs) <- repLSts sts;
        e'      <- repMDoE maybeModuleName (nonEmptyCoreList zs);
        wrapGenSyms ss e' }

  | otherwise
  = notHandled (ThMonadComprehensionSyntax e)

repE (ExplicitList _ es) = do { xs <- repLEs es; repListExp xs }
repE (ExplicitTuple _ es boxity) =
  let tupArgToCoreExp :: HsTupArg GhcRn -> MetaM (Core (Maybe (M TH.Exp)))
      tupArgToCoreExp a
        | (Present _ e) <- a = do { e' <- repLE e
                                  ; coreJustM expTyConName e' }
        | otherwise = coreNothingM expTyConName

  in do { args <- mapM tupArgToCoreExp es
        ; expTy <- wrapName  expTyConName
        ; let maybeExpQTy = mkTyConApp maybeTyCon [expTy]
              listArg = coreList' maybeExpQTy args
        ; if isBoxed boxity
          then repTup listArg
          else repUnboxedTup listArg }

repE (ExplicitSum _ alt arity e)
 = do { e1 <- repLE e
      ; repUnboxedSum e1 alt arity }

repE (RecordCon { rcon_con = c, rcon_flds = flds })
 = do { x <- lookupLOcc c;
        fs <- repFields flds;
        repRecCon x fs }
repE (RecordUpd { rupd_expr = e, rupd_flds = Left flds })
 = do { x <- repLE e;
        fs <- repUpdFields flds;
        repRecUpd x fs }
repE (RecordUpd { rupd_flds = Right _ })
  = do
      -- Not possible due to elimination in the renamer. See Note
      -- [Handling overloaded and rebindable constructs]
      panic "The impossible has happened!"

repE (ExprWithTySig _ e wc_ty)
  = addSimpleTyVarBinds (get_scoped_tvs_from_sig sig_ty) $
    do { e1 <- repLE e
       ; t1 <- rep_ty_sig' sig_ty
       ; repSigExp e1 t1 }
  where
    sig_ty = dropWildCards wc_ty

repE (ArithSeq _ _ aseq) =
  case aseq of
    From e              -> do { ds1 <- repLE e; repFrom ds1 }
    FromThen e1 e2      -> do
                             ds1 <- repLE e1
                             ds2 <- repLE e2
                             repFromThen ds1 ds2
    FromTo   e1 e2      -> do
                             ds1 <- repLE e1
                             ds2 <- repLE e2
                             repFromTo ds1 ds2
    FromThenTo e1 e2 e3 -> do
                             ds1 <- repLE e1
                             ds2 <- repLE e2
                             ds3 <- repLE e3
                             repFromThenTo ds1 ds2 ds3

repE (HsSpliceE _ splice)  = repSplice splice
repE (HsStatic _ e)        = repLE e >>= rep2 staticEName . (:[]) . unC
repE (HsUnboundVar _ uv)   = do
                               occ   <- occNameLit uv
                               sname <- repNameS occ
                               repUnboundVar sname
repE (HsGetField _ e (L _ (DotFieldOcc _ (L _ f)))) = do
  e1 <- repLE e
  repGetField e1 f
repE (HsProjection _ xs) = repProjection (fmap (unLoc . dfoLabel . unLoc) xs)
repE (XExpr (HsExpanded orig_expr ds_expr))
  = do { rebindable_on <- lift $ xoptM LangExt.RebindableSyntax
       ; if rebindable_on  -- See Note [Quotation and rebindable syntax]
         then repE ds_expr
         else repE orig_expr }
repE e@(HsPragE _ (HsPragSCC {}) _) = notHandled (ThCostCentres e)
repE e@(HsBracket{}) = notHandled (ThExpressionForm e)
repE e@(HsRnBracketOut{}) = notHandled (ThExpressionForm e)
repE e@(HsTcBracketOut{}) = notHandled (ThExpressionForm e)
repE e@(HsProc{}) = notHandled (ThExpressionForm e)

{- Note [Quotation and rebindable syntax]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  f = [| (* 3) |]

Because of Note [Handling overloaded and rebindable constructs] in GHC.Rename.Expr,
the renamer will expand (* 3) to (rightSection (*) 3), regardless of RebindableSyntax.
Then, concerning the TH quotation,

* If RebindableSyntax is off, we want the TH quote to generate the section (* 3),
  as the user originally wrote.

* If RebindableSyntax is on, we perhaps want the TH quote to generate
  (rightSection (*) 3), using whatever 'rightSection' is in scope, because
  (a) RebindableSyntax might not be on in the splicing context
  (b) Even if it is, 'rightSection' might not be in scope
  (c) At least in the case of Typed Template Haskell we should never get
      a type error from the splice.

We consult the module-wide RebindableSyntax flag here. We could instead record
the choice in HsExpanded, but it seems simpler to consult the flag (again).
-}

-----------------------------------------------------------------------------
-- Building representations of auxiliary structures like Match, Clause, Stmt,

repMatchTup ::  LMatch GhcRn (LHsExpr GhcRn) -> MetaM (Core (M TH.Match))
repMatchTup (L _ (Match { m_pats = [p]
                        , m_grhss = GRHSs _ guards wheres })) =
  do { ss1 <- mkGenSyms (collectLMatchPatBinders CollNoDictBinders p)
     ; addBinds ss1 $ do {
     ; p1 <- repLMP p
     ; (ss2,ds) <- repBinds wheres
     ; addBinds ss2 $ do {
     ; gs    <- repGuards guards
     ; match <- repMatch p1 gs ds
     ; wrapGenSyms (ss1++ss2) match }}}
repMatchTup _ = panic "repMatchTup: case alt with more than one arg"

repClauseTup ::  LMatch GhcRn (LHsExpr GhcRn) -> MetaM (Core (M TH.Clause))
repClauseTup (L _ (Match { m_pats = ps
                         , m_grhss = GRHSs _ guards  wheres })) =
  do { ss1 <- mkGenSyms (collectLMatchPatsBinders CollNoDictBinders ps)
     ; addBinds ss1 $ do {
       ps1 <- repLMPs ps
     ; (ss2,ds) <- repBinds wheres
     ; addBinds ss2 $ do {
       gs <- repGuards guards
     ; clause <- repClause ps1 gs ds
     ; wrapGenSyms (ss1++ss2) clause }}}

repGuards ::  [LGRHS GhcRn (LHsExpr GhcRn)] ->  MetaM (Core (M TH.Body))
repGuards [L _ (GRHS _ [] e)]
  = do {a <- repLE e; repNormal a }
repGuards other
  = do { zs <- mapM repLGRHS other
       ; let (xs, ys) = unzip zs
       ; gd <- repGuarded (nonEmptyCoreList ys)
       ; wrapGenSyms (concat xs) gd }

repLGRHS :: LGRHS GhcRn (LHsExpr GhcRn)
         -> MetaM ([GenSymBind], (Core (M (TH.Guard, TH.Exp))))
repLGRHS (L _ (GRHS _ [L _ (BodyStmt _ e1 _ _)] e2))
  = do { guarded <- repLNormalGE e1 e2
       ; return ([], guarded) }
repLGRHS (L _ (GRHS _ ss rhs))
  = do { (gs, ss') <- repLSts ss
       ; rhs' <- addBinds gs $ repLE rhs
       ; guarded <- repPatGE (nonEmptyCoreList ss') rhs'
       ; return (gs, guarded) }

repFields :: HsRecordBinds GhcRn -> MetaM (Core [M TH.FieldExp])
repFields (HsRecFields { rec_flds = flds })
  = repListM fieldExpTyConName rep_fld flds
  where
    rep_fld :: LHsRecField GhcRn (LHsExpr GhcRn)
            -> MetaM (Core (M TH.FieldExp))
    rep_fld (L _ fld) = do { fn <- lookupOcc (hsRecFieldSel fld)
                           ; e  <- repLE (hfbRHS fld)
                           ; repFieldExp fn e }

repUpdFields :: [LHsRecUpdField GhcRn] -> MetaM (Core [M TH.FieldExp])
repUpdFields = repListM fieldExpTyConName rep_fld
  where
    rep_fld :: LHsRecUpdField GhcRn -> MetaM (Core (M TH.FieldExp))
    rep_fld (L l fld) = case unLoc (hfbLHS fld) of
      Unambiguous sel_name _ -> do { fn <- lookupLOcc (L l sel_name)
                                   ; e  <- repLE (hfbRHS fld)
                                   ; repFieldExp fn e }
      Ambiguous{}            -> notHandled (ThAmbiguousRecordUpdates fld)



-----------------------------------------------------------------------------
-- Representing Stmt's is tricky, especially if bound variables
-- shadow each other. Consider:  [| do { x <- f 1; x <- f x; g x } |]
-- First gensym new names for every variable in any of the patterns.
-- both static (x'1 and x'2), and dynamic ((gensym "x") and (gensym "y"))
-- if variables didn't shadow, the static gensym wouldn't be necessary
-- and we could reuse the original names (x and x).
--
-- do { x'1 <- gensym "x"
--    ; x'2 <- gensym "x"
--    ; doE Nothing
--          [ BindSt (pvar x'1) [| f 1 |]
--          , BindSt (pvar x'2) [| f x |]
--          , NoBindSt [| g x |]
--          ]
--    }

-- The strategy is to translate a whole list of do-bindings by building a
-- bigger environment, and a bigger set of meta bindings
-- (like:  x'1 <- gensym "x" ) and then combining these with the translations
-- of the expressions within the Do

-----------------------------------------------------------------------------
-- The helper function repSts computes the translation of each sub expression
-- and a bunch of prefix bindings denoting the dynamic renaming.

repLSts :: [LStmt GhcRn (LHsExpr GhcRn)] -> MetaM ([GenSymBind], [Core (M TH.Stmt)])
repLSts stmts = repSts (map unLoc stmts)

repSts :: [Stmt GhcRn (LHsExpr GhcRn)] -> MetaM ([GenSymBind], [Core (M TH.Stmt)])
repSts (BindStmt _ p e : ss) =
   do { e2 <- repLE e
      ; ss1 <- mkGenSyms (collectPatBinders CollNoDictBinders p)
      ; addBinds ss1 $ do {
      ; p1 <- repLP p;
      ; (ss2,zs) <- repSts ss
      ; z <- repBindSt p1 e2
      ; return (ss1++ss2, z : zs) }}
repSts (LetStmt _ bs : ss) =
   do { (ss1,ds) <- repBinds bs
      ; z <- repLetSt ds
      ; (ss2,zs) <- addBinds ss1 (repSts ss)
      ; return (ss1++ss2, z : zs) }
repSts (BodyStmt _ e _ _ : ss) =
   do { e2 <- repLE e
      ; z <- repNoBindSt e2
      ; (ss2,zs) <- repSts ss
      ; return (ss2, z : zs) }
repSts (ParStmt _ stmt_blocks _ _ : ss) =
   do { (ss_s, stmt_blocks1) <- mapAndUnzipM rep_stmt_block stmt_blocks
      ; let stmt_blocks2 = nonEmptyCoreList stmt_blocks1
            ss1 = concat ss_s
      ; z <- repParSt stmt_blocks2
      ; (ss2, zs) <- addBinds ss1 (repSts ss)
      ; return (ss1++ss2, z : zs) }
   where
     rep_stmt_block :: ParStmtBlock GhcRn GhcRn
                    -> MetaM ([GenSymBind], Core [(M TH.Stmt)])
     rep_stmt_block (ParStmtBlock _ stmts _ _) =
       do { (ss1, zs) <- repSts (map unLoc stmts)
          ; zs1 <- coreListM stmtTyConName zs
          ; return (ss1, zs1) }
repSts [LastStmt _ e _ _]
  = do { e2 <- repLE e
       ; z <- repNoBindSt e2
       ; return ([], [z]) }
repSts (stmt@RecStmt{} : ss)
  = do { let binders = collectLStmtsBinders CollNoDictBinders (unLoc $ recS_stmts stmt)
       ; ss1 <- mkGenSyms binders
       -- Bring all of binders in the recursive group into scope for the
       -- whole group.
       ; (ss1_other,rss) <- addBinds ss1 $ repSts (map unLoc (unLoc $ recS_stmts stmt))
       ; massert (sort ss1 == sort ss1_other)
       ; z <- repRecSt (nonEmptyCoreList rss)
       ; (ss2,zs) <- addBinds ss1 (repSts ss)
       ; return (ss1++ss2, z : zs) }
repSts []    = return ([],[])
repSts other = notHandled (ThExoticStatement other)


-----------------------------------------------------------
--                      Bindings
-----------------------------------------------------------

repBinds :: HsLocalBinds GhcRn -> MetaM ([GenSymBind], Core [(M TH.Dec)])
repBinds (EmptyLocalBinds _)
  = do  { core_list <- coreListM decTyConName []
        ; return ([], core_list) }

repBinds (HsIPBinds _ (IPBinds _ decs))
 = do   { ips <- mapM rep_implicit_param_bind decs
        ; core_list <- coreListM decTyConName
                                (de_loc (sort_by_loc ips))
        ; return ([], core_list)
        }

repBinds (HsValBinds _ decs)
 = do   { let { bndrs = hsScopedTvBinders decs ++ collectHsValBinders CollNoDictBinders decs }
                -- No need to worry about detailed scopes within
                -- the binding group, because we are talking Names
                -- here, so we can safely treat it as a mutually
                -- recursive group
                -- For hsScopedTvBinders see Note [Scoped type variables in quotes]
        ; ss        <- mkGenSyms bndrs
        ; prs       <- addBinds ss (rep_val_binds decs)
        ; core_list <- coreListM decTyConName
                                (de_loc (sort_by_loc prs))
        ; return (ss, core_list) }

rep_implicit_param_bind :: LIPBind GhcRn -> MetaM (SrcSpan, Core (M TH.Dec))
rep_implicit_param_bind (L loc (IPBind _ ename (L _ rhs)))
 = do { name <- case ename of
                    Left (L _ n) -> rep_implicit_param_name n
                    Right _ ->
                        panic "rep_implicit_param_bind: post typechecking"
      ; rhs' <- repE rhs
      ; ipb <- repImplicitParamBind name rhs'
      ; return (locA loc, ipb) }

rep_implicit_param_name :: HsIPName -> MetaM (Core String)
rep_implicit_param_name (HsIPName name) = coreStringLit (unpackFS name)

rep_val_binds :: HsValBinds GhcRn -> MetaM [(SrcSpan, Core (M TH.Dec))]
-- Assumes: all the binders of the binding are already in the meta-env
rep_val_binds (XValBindsLR (NValBinds binds sigs))
 = do { core1 <- rep_binds (unionManyBags (map snd binds))
      ; core2 <- rep_sigs sigs
      ; return (core1 ++ core2) }
rep_val_binds (ValBinds _ _ _)
 = panic "rep_val_binds: ValBinds"

rep_binds :: LHsBinds GhcRn -> MetaM [(SrcSpan, Core (M TH.Dec))]
rep_binds = mapM rep_bind . bagToList

rep_bind :: LHsBind GhcRn -> MetaM (SrcSpan, Core (M TH.Dec))
-- Assumes: all the binders of the binding are already in the meta-env

-- Note GHC treats declarations of a variable (not a pattern)
-- e.g.  x = g 5 as a Fun MonoBinds. This is indicated by a single match
-- with an empty list of patterns
rep_bind (L loc (FunBind
                 { fun_id = fn,
                   fun_matches = MG { mg_alts
                           = (L _ [L _ (Match
                                   { m_pats = []
                                   , m_grhss = GRHSs _ guards wheres }
                                      )]) } }))
 = do { (ss,wherecore) <- repBinds wheres
        ; guardcore <- addBinds ss (repGuards guards)
        ; fn'  <- lookupNBinder fn
        ; p    <- repPvar fn'
        ; ans  <- repVal p guardcore wherecore
        ; ans' <- wrapGenSyms ss ans
        ; return (locA loc, ans') }

rep_bind (L loc (FunBind { fun_id = fn
                         , fun_matches = MG { mg_alts = L _ ms } }))
 =   do { ms1 <- mapM repClauseTup ms
        ; fn' <- lookupNBinder fn
        ; ans <- repFun fn' (nonEmptyCoreList ms1)
        ; return (locA loc, ans) }

rep_bind (L loc (PatBind { pat_lhs = pat
                         , pat_rhs = GRHSs _ guards wheres }))
 =   do { patcore <- repLP pat
        ; (ss,wherecore) <- repBinds wheres
        ; guardcore <- addBinds ss (repGuards guards)
        ; ans  <- repVal patcore guardcore wherecore
        ; ans' <- wrapGenSyms ss ans
        ; return (locA loc, ans') }

rep_bind (L _ (VarBind { var_id = v, var_rhs = e}))
 =   do { v' <- lookupBinder v
        ; e2 <- repLE e
        ; x <- repNormal e2
        ; patcore <- repPvar v'
        ; empty_decls <- coreListM decTyConName []
        ; ans <- repVal patcore x empty_decls
        ; return (srcLocSpan (getSrcLoc v), ans) }

rep_bind (L _ (AbsBinds {}))  = panic "rep_bind: AbsBinds"
rep_bind (L loc (PatSynBind _ (PSB { psb_id   = syn
                                   , psb_args = args
                                   , psb_def  = pat
                                   , psb_dir  = dir })))
  = do { syn'      <- lookupNBinder syn
       ; dir'      <- repPatSynDir dir
       ; ss        <- mkGenArgSyms args
       ; patSynD'  <- addBinds ss (
         do { args'  <- repPatSynArgs args
            ; pat'   <- repLP pat
            ; repPatSynD syn' args' dir' pat' })
       ; patSynD'' <- wrapGenArgSyms args ss patSynD'
       ; return (locA loc, patSynD'') }
  where
    mkGenArgSyms :: HsPatSynDetails GhcRn -> MetaM [GenSymBind]
    -- for Record Pattern Synonyms we want to conflate the selector
    -- and the pattern-only names in order to provide a nicer TH
    -- API. Whereas inside GHC, record pattern synonym selectors and
    -- their pattern-only bound right hand sides have different names,
    -- we want to treat them the same in TH. This is the reason why we
    -- need an adjusted mkGenArgSyms in the `RecCon` case below.
    mkGenArgSyms (PrefixCon _ args)   = mkGenSyms (map unLoc args)
    mkGenArgSyms (InfixCon arg1 arg2) = mkGenSyms [unLoc arg1, unLoc arg2]
    mkGenArgSyms (RecCon fields)
      = do { let pats = map (unLoc . recordPatSynPatVar) fields
                 sels = map (foExt . recordPatSynField) fields
           ; ss <- mkGenSyms sels
           ; return $ replaceNames (zip sels pats) ss }

    replaceNames selsPats genSyms
      = [ (pat, id) | (sel, id) <- genSyms, (sel', pat) <- selsPats
                    , sel == sel' ]

    wrapGenArgSyms :: HsPatSynDetails GhcRn
                   -> [GenSymBind] -> Core (M TH.Dec) -> MetaM (Core (M TH.Dec))
    wrapGenArgSyms (RecCon _) _  dec = return dec
    wrapGenArgSyms _          ss dec = wrapGenSyms ss dec

repPatSynD :: Core TH.Name
           -> Core (M TH.PatSynArgs)
           -> Core (M TH.PatSynDir)
           -> Core (M TH.Pat)
           -> MetaM (Core (M TH.Dec))
repPatSynD (MkC syn) (MkC args) (MkC dir) (MkC pat)
  = rep2 patSynDName [syn, args, dir, pat]

repPatSynArgs :: HsPatSynDetails GhcRn -> MetaM (Core (M TH.PatSynArgs))
repPatSynArgs (PrefixCon _ args)
  = do { args' <- repList nameTyConName lookupLOcc args
       ; repPrefixPatSynArgs args' }
repPatSynArgs (InfixCon arg1 arg2)
  = do { arg1' <- lookupLOcc arg1
       ; arg2' <- lookupLOcc arg2
       ; repInfixPatSynArgs arg1' arg2' }
repPatSynArgs (RecCon fields)
  = do { sels' <- repList nameTyConName (lookupOcc . foExt) sels
       ; repRecordPatSynArgs sels' }
  where sels = map recordPatSynField fields

repPrefixPatSynArgs :: Core [TH.Name] -> MetaM (Core (M TH.PatSynArgs))
repPrefixPatSynArgs (MkC nms) = rep2 prefixPatSynName [nms]

repInfixPatSynArgs :: Core TH.Name -> Core TH.Name -> MetaM (Core (M TH.PatSynArgs))
repInfixPatSynArgs (MkC nm1) (MkC nm2) = rep2 infixPatSynName [nm1, nm2]

repRecordPatSynArgs :: Core [TH.Name]
                    -> MetaM (Core (M TH.PatSynArgs))
repRecordPatSynArgs (MkC sels) = rep2 recordPatSynName [sels]

repPatSynDir :: HsPatSynDir GhcRn -> MetaM (Core (M TH.PatSynDir))
repPatSynDir Unidirectional        = rep2 unidirPatSynName []
repPatSynDir ImplicitBidirectional = rep2 implBidirPatSynName []
repPatSynDir (ExplicitBidirectional (MG { mg_alts = (L _ clauses) }))
  = do { clauses' <- mapM repClauseTup clauses
       ; repExplBidirPatSynDir (nonEmptyCoreList clauses') }

repExplBidirPatSynDir :: Core [(M TH.Clause)] -> MetaM (Core (M TH.PatSynDir))
repExplBidirPatSynDir (MkC cls) = rep2 explBidirPatSynName [cls]


-----------------------------------------------------------------------------
-- Since everything in a Bind is mutually recursive we need rename all
-- all the variables simultaneously. For example:
-- [| AndMonoBinds (f x = x + g 2) (g x = f 1 + 2) |] would translate to
-- do { f'1 <- gensym "f"
--    ; g'2 <- gensym "g"
--    ; [ do { x'3 <- gensym "x"; fun f'1 [pvar x'3] [| x + g2 |]},
--        do { x'4 <- gensym "x"; fun g'2 [pvar x'4] [| f 1 + 2 |]}
--      ]}
-- This requires collecting the bindings (f'1 <- gensym "f"), and the
-- environment ( f |-> f'1 ) from each binding, and then unioning them
-- together. As we do this we collect GenSymBinds's which represent the renamed
-- variables bound by the Bindings. In order not to lose track of these
-- representations we build a shadow datatype MB with the same structure as
-- MonoBinds, but which has slots for the representations


-----------------------------------------------------------------------------
-- GHC allows a more general form of lambda abstraction than specified
-- by Haskell 98. In particular it allows guarded lambda's like :
-- (\  x | even x -> 0 | odd x -> 1) at the moment we can't represent this in
-- Haskell Template's Meta.Exp type so we punt if it isn't a simple thing like
-- (\ p1 .. pn -> exp) by causing an error.

repLambda :: LMatch GhcRn (LHsExpr GhcRn) -> MetaM (Core (M TH.Exp))
repLambda (L _ (Match { m_pats = ps
                      , m_grhss = GRHSs _ [L _ (GRHS _ [] e)]
                                              (EmptyLocalBinds _) } ))
 = do { let bndrs = collectLMatchPatsBinders CollNoDictBinders ps ;
      ; ss  <- mkGenSyms bndrs
      ; lam <- addBinds ss (
                do { xs <- repLMPs ps; body <- repLE e; repLam xs body })
      ; wrapGenSyms ss lam }

repLambda (L _ m) = notHandled (ThGuardedLambdas m)


-----------------------------------------------------------------------------
--                      Patterns
-- repP deals with patterns.  It assumes that we have already
-- walked over the pattern(s) once to collect the binders, and
-- have extended the environment.  So every pattern-bound
-- variable should already appear in the environment.

-- Process a list of patterns
repLPs :: [LPat GhcRn] -> MetaM (Core [(M TH.Pat)])
repLPs ps = repListM patTyConName repLP ps

repLP :: LPat GhcRn -> MetaM (Core (M TH.Pat))
repLP p = repP (unLoc p)

repLMP :: LMatchPat GhcRn -> MetaM (Core (M TH.Pat))
repLMP (L _ (VisPat _ p)) = repLP p
repLMP _                  = panic "we don't have other patterns at the moment"

repLMPs :: [LMatchPat GhcRn] -> MetaM (Core ([M TH.Pat]))
repLMPs ps = repListM patTyConName repLMP ps


repP :: Pat GhcRn -> MetaM (Core (M TH.Pat))
repP (WildPat _)        = repPwild
repP (LitPat _ l)       = do { l2 <- repLiteral l; repPlit l2 }
repP (VarPat _ x)       = do { x' <- lookupBinder (unLoc x); repPvar x' }
repP (LazyPat _ p)      = do { p1 <- repLP p; repPtilde p1 }
repP (BangPat _ p)      = do { p1 <- repLP p; repPbang p1 }
repP (AsPat _ x p)      = do { x' <- lookupNBinder x; p1 <- repLP p
                             ; repPaspat x' p1 }
repP (ParPat _ _ p _)   = repLP p
repP (ListPat _ ps)     = do { qs <- repLPs ps; repPlist qs }
repP (TuplePat _ ps boxed)
  | isBoxed boxed       = do { qs <- repLPs ps; repPtup qs }
  | otherwise           = do { qs <- repLPs ps; repPunboxedTup qs }
repP (SumPat _ p alt arity) = do { p1 <- repLP p
                                 ; repPunboxedSum p1 alt arity }
repP (ConPat NoExtField dc details)
 = do { con_str <- lookupLOcc dc
      ; case details of
         PrefixCon tyargs ps -> do { qs <- repLPs ps
                                   ; ts <- repListM typeTyConName (repTy . unLoc . hsps_body) tyargs
                                   ; repPcon con_str ts qs }
         RecCon rec   -> do { fps <- repListM fieldPatTyConName rep_fld (rec_flds rec)
                            ; repPrec con_str fps }
         InfixCon p1 p2 -> do { p1' <- repLP p1;
                                p2' <- repLP p2;
                                repPinfix p1' con_str p2' }
   }
 where
   rep_fld :: LHsRecField GhcRn (LPat GhcRn) -> MetaM (Core (M (TH.Name, TH.Pat)))
   rep_fld (L _ fld) = do { MkC v <- lookupOcc (hsRecFieldSel fld)
                          ; MkC p <- repLP (hfbRHS fld)
                          ; rep2 fieldPatName [v,p] }
repP (NPat _ (L _ l) Nothing _) = do { a <- repOverloadedLiteral l
                                     ; repPlit a }
repP (ViewPat _ e p) = do { e' <- repLE e; p' <- repLP p; repPview e' p' }
repP p@(NPat _ _ (Just _) _) = notHandled (ThNegativeOverloadedPatterns p)
repP (SigPat _ p t) = do { p' <- repLP p
                         ; t' <- repLTy (hsPatSigType t)
                         ; repPsig p' t' }
repP (SplicePat _ splice) = repSplice splice
repP other = notHandled (ThExoticPattern other)

----------------------------------------------------------
-- Declaration ordering helpers

sort_by_loc :: [(SrcSpan, a)] -> [(SrcSpan, a)]
sort_by_loc = sortBy (SrcLoc.leftmost_smallest `on` fst)

de_loc :: [(a, b)] -> [b]
de_loc = map snd

----------------------------------------------------------
--      The meta-environment

-- A name/identifier association for fresh names of locally bound entities
type GenSymBind = (Name, Id)    -- Gensym the string and bind it to the Id
                                -- I.e.         (x, x_id) means
                                --      let x_id = gensym "x" in ...

-- Generate a fresh name for a locally bound entity

mkGenSyms :: [Name] -> MetaM [GenSymBind]
-- We can use the existing name.  For example:
--      [| \x_77 -> x_77 + x_77 |]
-- desugars to
--      do { x_77 <- genSym "x"; .... }
-- We use the same x_77 in the desugared program, but with the type Bndr
-- instead of Int
--
-- We do make it an Internal name, though (hence localiseName)
--
-- Nevertheless, it's monadic because we have to generate nameTy
mkGenSyms ns = do { var_ty <- lookupType nameTyConName
                  ; return [(nm, mkLocalId (localiseName nm) Many var_ty) | nm <- ns] }


addBinds :: [GenSymBind] -> MetaM a -> MetaM a
-- Add a list of fresh names for locally bound entities to the
-- meta environment (which is part of the state carried around
-- by the desugarer monad)
addBinds bs m = mapReaderT (dsExtendMetaEnv (mkNameEnv [(n,DsBound id) | (n,id) <- bs])) m

-- Look up a locally bound name
--
lookupNBinder :: LocatedN Name -> MetaM (Core TH.Name)
lookupNBinder n = lookupBinder (unLoc n)

lookupBinder :: Name -> MetaM (Core TH.Name)
lookupBinder = lookupOcc
  -- Binders are brought into scope before the pattern or what-not is
  -- desugared.  Moreover, in instance declaration the binder of a method
  -- will be the selector Id and hence a global; so we need the
  -- globalVar case of lookupOcc

-- Look up a name that is either locally bound or a global name
--
--  * If it is a global name, generate the "original name" representation (ie,
--   the <module>:<name> form) for the associated entity
--
lookupLOcc :: GenLocated l Name -> MetaM (Core TH.Name)
-- Lookup an occurrence; it can't be a splice.
-- Use the in-scope bindings if they exist
lookupLOcc n = lookupOcc (unLoc n)

lookupOcc :: Name -> MetaM (Core TH.Name)
lookupOcc = lift . lookupOccDsM

lookupOccDsM :: Name -> DsM (Core TH.Name)
lookupOccDsM n
  = do {  mb_val <- dsLookupMetaEnv n ;
          case mb_val of
                Nothing           -> globalVar n
                Just (DsBound x)  -> return (coreVar x)
                Just (DsSplice _) -> pprPanic "repE:lookupOcc" (ppr n)
    }

globalVar :: Name -> DsM (Core TH.Name)
-- Not bound by the meta-env
-- Could be top-level; or could be local
--      f x = $(g [| x |])
-- Here the x will be local
globalVar name
  | isExternalName name
  = do  { MkC mod <- coreStringLit name_mod
        ; MkC pkg <- coreStringLit name_pkg
        ; MkC occ <- nameLit name
        ; rep2_nwDsM mk_varg [pkg,mod,occ] }
  | otherwise
  = do  { MkC occ <- nameLit name
        ; platform <- targetPlatform <$> getDynFlags
        ; let uni = mkIntegerExpr platform (toInteger $ getKey (getUnique name))
        ; rep2_nwDsM mkNameLName [occ,uni] }
  where
      mod = assert (isExternalName name) nameModule name
      name_mod = moduleNameString (moduleName mod)
      name_pkg = unitString (moduleUnit mod)
      name_occ = nameOccName name
      mk_varg | isDataOcc name_occ = mkNameG_dName
              | isVarOcc  name_occ = mkNameG_vName
              | isTcOcc   name_occ = mkNameG_tcName
              | otherwise          = pprPanic "GHC.HsToCore.Quote.globalVar" (ppr name)

lookupType :: Name      -- Name of type constructor (e.g. (M TH.Exp))
           -> MetaM Type  -- The type
lookupType tc_name = do { tc <- lift $ dsLookupTyCon tc_name ;
                          return (mkTyConApp tc []) }

wrapGenSyms :: [GenSymBind]
            -> Core (M a) -> MetaM (Core (M a))
-- wrapGenSyms [(nm1,id1), (nm2,id2)] y
--      --> bindQ (gensym nm1) (\ id1 ->
--          bindQ (gensym nm2 (\ id2 ->
--          y))

wrapGenSyms binds body@(MkC b)
  = do  { var_ty <- lookupType nameTyConName
        ; go var_ty binds }
  where
    (_, elt_ty) = tcSplitAppTy (exprType b)
        -- b :: m a, so we can get the type 'a' by looking at the
        -- argument type. Need to use `tcSplitAppTy` here as since
        -- the overloaded quotations patch the type of the expression can
        -- be something more complicated than just `Q a`.
        -- See #17839 for when this went wrong with the type `WriterT () m a`

    go _ [] = return body
    go var_ty ((name,id) : binds)
      = do { MkC body'  <- go var_ty binds
           ; lit_str    <- lift $ nameLit name
           ; gensym_app <- repGensym lit_str
           ; repBindM var_ty elt_ty
                      gensym_app (MkC (Lam id body')) }

nameLit :: Name -> DsM (Core String)
nameLit n = coreStringLit (occNameString (nameOccName n))

occNameLit :: OccName -> MetaM (Core String)
occNameLit name = coreStringLit (occNameString name)


-- %*********************************************************************
-- %*                                                                   *
--              Constructing code
-- %*                                                                   *
-- %*********************************************************************

-----------------------------------------------------------------------------
-- PHANTOM TYPES for consistency. In order to make sure we do this correct
-- we invent a new datatype which uses phantom types.

newtype Core a = MkC CoreExpr
unC :: Core a -> CoreExpr
unC (MkC x) = x

type family NotM a where
  NotM (M _) = TypeError ('Text ("rep2_nw must not produce something of overloaded type"))
  NotM _other = (() :: Constraint)

rep2M :: Name -> [CoreExpr] -> MetaM (Core (M a))
rep2 :: Name -> [CoreExpr] -> MetaM (Core (M a))
rep2_nw :: NotM a => Name -> [CoreExpr] -> MetaM (Core a)
rep2_nwDsM :: NotM a => Name -> [CoreExpr] -> DsM (Core a)
rep2 = rep2X lift (asks quoteWrapper)
rep2M = rep2X lift (asks monadWrapper)
rep2_nw n xs = lift (rep2_nwDsM n xs)
rep2_nwDsM = rep2X id (return id)

rep2X :: Monad m => (forall z . DsM z -> m z)
      -> m (CoreExpr -> CoreExpr)
      -> Name
      -> [ CoreExpr ]
      -> m (Core a)
rep2X lift_dsm get_wrap n xs = do
  { rep_id <- lift_dsm $ dsLookupGlobalId n
  ; wrap <- get_wrap
  ; return (MkC $ (foldl' App (wrap (Var rep_id)) xs)) }


dataCon' :: Name -> [CoreExpr] -> MetaM (Core a)
dataCon' n args = do { id <- lift $ dsLookupDataCon n
                     ; return $ MkC $ mkCoreConApps id args }

dataCon :: Name -> MetaM (Core a)
dataCon n = dataCon' n []


-- %*********************************************************************
-- %*                                                                   *
--              The 'smart constructors'
-- %*                                                                   *
-- %*********************************************************************

--------------- Patterns -----------------
repPlit   :: Core TH.Lit -> MetaM (Core (M TH.Pat))
repPlit (MkC l) = rep2 litPName [l]

repPvar :: Core TH.Name -> MetaM (Core (M TH.Pat))
repPvar (MkC s) = rep2 varPName [s]

repPtup :: Core [(M TH.Pat)] -> MetaM (Core (M TH.Pat))
repPtup (MkC ps) = rep2 tupPName [ps]

repPunboxedTup :: Core [(M TH.Pat)] -> MetaM (Core (M TH.Pat))
repPunboxedTup (MkC ps) = rep2 unboxedTupPName [ps]

repPunboxedSum :: Core (M TH.Pat) -> TH.SumAlt -> TH.SumArity -> MetaM (Core (M TH.Pat))
-- Note: not Core TH.SumAlt or Core TH.SumArity; it's easier to be direct here
repPunboxedSum (MkC p) alt arity
 = do { platform <- getPlatform
      ; rep2 unboxedSumPName [ p
                             , mkIntExprInt platform alt
                             , mkIntExprInt platform arity ] }

repPcon   :: Core TH.Name -> Core [(M TH.Type)] -> Core [(M TH.Pat)] -> MetaM (Core (M TH.Pat))
repPcon (MkC s) (MkC ts) (MkC ps) = rep2 conPName [s, ts, ps]

repPrec   :: Core TH.Name -> Core [M (TH.Name, TH.Pat)] -> MetaM (Core (M TH.Pat))
repPrec (MkC c) (MkC rps) = rep2 recPName [c,rps]

repPinfix :: Core (M TH.Pat) -> Core TH.Name -> Core (M TH.Pat) -> MetaM (Core (M TH.Pat))
repPinfix (MkC p1) (MkC n) (MkC p2) = rep2 infixPName [p1, n, p2]

repPtilde :: Core (M TH.Pat) -> MetaM (Core (M TH.Pat))
repPtilde (MkC p) = rep2 tildePName [p]

repPbang :: Core (M TH.Pat) -> MetaM (Core (M TH.Pat))
repPbang (MkC p) = rep2 bangPName [p]

repPaspat :: Core TH.Name -> Core (M TH.Pat) -> MetaM (Core (M TH.Pat))
repPaspat (MkC s) (MkC p) = rep2 asPName [s, p]

repPwild  :: MetaM (Core (M TH.Pat))
repPwild = rep2 wildPName []

repPlist :: Core [(M TH.Pat)] -> MetaM (Core (M TH.Pat))
repPlist (MkC ps) = rep2 listPName [ps]

repPview :: Core (M TH.Exp) -> Core (M TH.Pat) -> MetaM (Core (M TH.Pat))
repPview (MkC e) (MkC p) = rep2 viewPName [e,p]

repPsig :: Core (M TH.Pat) -> Core (M TH.Type) -> MetaM (Core (M TH.Pat))
repPsig (MkC p) (MkC t) = rep2 sigPName [p, t]

--------------- Expressions -----------------
repVarOrCon :: Name -> Core TH.Name -> MetaM (Core (M TH.Exp))
repVarOrCon vc str
    | isVarNameSpace ns = repVar str  -- Both type and term variables (#18740)
    | otherwise         = repCon str
  where
    ns = nameNameSpace vc

repVar :: Core TH.Name -> MetaM (Core (M TH.Exp))
repVar (MkC s) = rep2 varEName [s]

repCon :: Core TH.Name -> MetaM (Core (M TH.Exp))
repCon (MkC s) = rep2 conEName [s]

repLit :: Core TH.Lit -> MetaM (Core (M TH.Exp))
repLit (MkC c) = rep2 litEName [c]

repApp :: Core (M TH.Exp) -> Core (M TH.Exp) -> MetaM (Core (M TH.Exp))
repApp (MkC x) (MkC y) = rep2 appEName [x,y]

repAppType :: Core (M TH.Exp) -> Core (M TH.Type) -> MetaM (Core (M TH.Exp))
repAppType (MkC x) (MkC y) = rep2 appTypeEName [x,y]

repLam :: Core [(M TH.Pat)] -> Core (M TH.Exp) -> MetaM (Core (M TH.Exp))
repLam (MkC ps) (MkC e) = rep2 lamEName [ps, e]

repLamCase :: Core [(M TH.Match)] -> MetaM (Core (M TH.Exp))
repLamCase (MkC ms) = rep2 lamCaseEName [ms]

repTup :: Core [Maybe (M TH.Exp)] -> MetaM (Core (M TH.Exp))
repTup (MkC es) = rep2 tupEName [es]

repUnboxedTup :: Core [Maybe (M TH.Exp)] -> MetaM (Core (M TH.Exp))
repUnboxedTup (MkC es) = rep2 unboxedTupEName [es]

repUnboxedSum :: Core (M TH.Exp) -> TH.SumAlt -> TH.SumArity -> MetaM (Core (M TH.Exp))
-- Note: not Core TH.SumAlt or Core TH.SumArity; it's easier to be direct here
repUnboxedSum (MkC e) alt arity
 = do { platform <- getPlatform
      ; rep2 unboxedSumEName [ e
                             , mkIntExprInt platform alt
                             , mkIntExprInt platform arity ] }

repCond :: Core (M TH.Exp) -> Core (M TH.Exp) -> Core (M TH.Exp) -> MetaM (Core (M TH.Exp))
repCond (MkC x) (MkC y) (MkC z) = rep2 condEName [x,y,z]

repMultiIf :: Core [M (TH.Guard, TH.Exp)] -> MetaM (Core (M TH.Exp))
repMultiIf (MkC alts) = rep2 multiIfEName [alts]

repLetE :: Core [(M TH.Dec)] -> Core (M TH.Exp) -> MetaM (Core (M TH.Exp))
repLetE (MkC ds) (MkC e) = rep2 letEName [ds, e]

repCaseE :: Core (M TH.Exp) -> Core [(M TH.Match)] -> MetaM (Core (M TH.Exp))
repCaseE (MkC e) (MkC ms) = rep2 caseEName [e, ms]

repDoE :: Maybe ModuleName -> Core [(M TH.Stmt)] -> MetaM (Core (M TH.Exp))
repDoE = repDoBlock doEName

repMDoE :: Maybe ModuleName -> Core [(M TH.Stmt)] -> MetaM (Core (M TH.Exp))
repMDoE = repDoBlock mdoEName

repDoBlock :: Name -> Maybe ModuleName -> Core [(M TH.Stmt)] -> MetaM (Core (M TH.Exp))
repDoBlock doName maybeModName (MkC ss) = do
    MkC coreModName <- coreModNameM
    rep2 doName [coreModName, ss]
  where
    coreModNameM :: MetaM (Core (Maybe TH.ModName))
    coreModNameM = case maybeModName of
      Just m -> do
        MkC s <- coreStringLit (moduleNameString m)
        mName <- rep2_nw mkModNameName [s]
        coreJust modNameTyConName mName
      _ -> coreNothing modNameTyConName

repComp :: Core [(M TH.Stmt)] -> MetaM (Core (M TH.Exp))
repComp (MkC ss) = rep2 compEName [ss]

repListExp :: Core [(M TH.Exp)] -> MetaM (Core (M TH.Exp))
repListExp (MkC es) = rep2 listEName [es]

repSigExp :: Core (M TH.Exp) -> Core (M TH.Type) -> MetaM (Core (M TH.Exp))
repSigExp (MkC e) (MkC t) = rep2 sigEName [e,t]

repRecCon :: Core TH.Name -> Core [M TH.FieldExp]-> MetaM (Core (M TH.Exp))
repRecCon (MkC c) (MkC fs) = rep2 recConEName [c,fs]

repRecUpd :: Core (M TH.Exp) -> Core [M TH.FieldExp] -> MetaM (Core (M TH.Exp))
repRecUpd (MkC e) (MkC fs) = rep2 recUpdEName [e,fs]

repFieldExp :: Core TH.Name -> Core (M TH.Exp) -> MetaM (Core (M TH.FieldExp))
repFieldExp (MkC n) (MkC x) = rep2 fieldExpName [n,x]

repInfixApp :: Core (M TH.Exp) -> Core (M TH.Exp) -> Core (M TH.Exp) -> MetaM (Core (M TH.Exp))
repInfixApp (MkC x) (MkC y) (MkC z) = rep2 infixAppName [x,y,z]

repSectionL :: Core (M TH.Exp) -> Core (M TH.Exp) -> MetaM (Core (M TH.Exp))
repSectionL (MkC x) (MkC y) = rep2 sectionLName [x,y]

repSectionR :: Core (M TH.Exp) -> Core (M TH.Exp) -> MetaM (Core (M TH.Exp))
repSectionR (MkC x) (MkC y) = rep2 sectionRName [x,y]

repImplicitParamVar :: Core String -> MetaM (Core (M TH.Exp))
repImplicitParamVar (MkC x) = rep2 implicitParamVarEName [x]

------------ Right hand sides (guarded expressions) ----
repGuarded :: Core [M (TH.Guard, TH.Exp)] -> MetaM (Core (M TH.Body))
repGuarded (MkC pairs) = rep2 guardedBName [pairs]

repNormal :: Core (M TH.Exp) -> MetaM (Core (M TH.Body))
repNormal (MkC e) = rep2 normalBName [e]

------------ Guards ----
repLNormalGE :: LHsExpr GhcRn -> LHsExpr GhcRn
             -> MetaM (Core (M (TH.Guard, TH.Exp)))
repLNormalGE g e = do g' <- repLE g
                      e' <- repLE e
                      repNormalGE g' e'

repNormalGE :: Core (M TH.Exp) -> Core (M TH.Exp) -> MetaM (Core (M (TH.Guard, TH.Exp)))
repNormalGE (MkC g) (MkC e) = rep2 normalGEName [g, e]

repPatGE :: Core [(M TH.Stmt)] -> Core (M TH.Exp) -> MetaM (Core (M (TH.Guard, TH.Exp)))
repPatGE (MkC ss) (MkC e) = rep2 patGEName [ss, e]

------------- Stmts -------------------
repBindSt :: Core (M TH.Pat) -> Core (M TH.Exp) -> MetaM (Core (M TH.Stmt))
repBindSt (MkC p) (MkC e) = rep2 bindSName [p,e]

repLetSt :: Core [(M TH.Dec)] -> MetaM (Core (M TH.Stmt))
repLetSt (MkC ds) = rep2 letSName [ds]

repNoBindSt :: Core (M TH.Exp) -> MetaM (Core (M TH.Stmt))
repNoBindSt (MkC e) = rep2 noBindSName [e]

repParSt :: Core [[(M TH.Stmt)]] -> MetaM (Core (M TH.Stmt))
repParSt (MkC sss) = rep2 parSName [sss]

repRecSt :: Core [(M TH.Stmt)] -> MetaM (Core (M TH.Stmt))
repRecSt (MkC ss) = rep2 recSName [ss]

-------------- Range (Arithmetic sequences) -----------
repFrom :: Core (M TH.Exp) -> MetaM (Core (M TH.Exp))
repFrom (MkC x) = rep2 fromEName [x]

repFromThen :: Core (M TH.Exp) -> Core (M TH.Exp) -> MetaM (Core (M TH.Exp))
repFromThen (MkC x) (MkC y) = rep2 fromThenEName [x,y]

repFromTo :: Core (M TH.Exp) -> Core (M TH.Exp) -> MetaM (Core (M TH.Exp))
repFromTo (MkC x) (MkC y) = rep2 fromToEName [x,y]

repFromThenTo :: Core (M TH.Exp) -> Core (M TH.Exp) -> Core (M TH.Exp) -> MetaM (Core (M TH.Exp))
repFromThenTo (MkC x) (MkC y) (MkC z) = rep2 fromThenToEName [x,y,z]

------------ Match and Clause Tuples -----------
repMatch :: Core (M TH.Pat) -> Core (M TH.Body) -> Core [(M TH.Dec)] -> MetaM (Core (M TH.Match))
repMatch (MkC p) (MkC bod) (MkC ds) = rep2 matchName [p, bod, ds]

repClause :: Core [(M TH.Pat)] -> Core (M TH.Body) -> Core [(M TH.Dec)] -> MetaM (Core (M TH.Clause))
repClause (MkC ps) (MkC bod) (MkC ds) = rep2 clauseName [ps, bod, ds]

-------------- Dec -----------------------------
repVal :: Core (M TH.Pat) -> Core (M TH.Body) -> Core [(M TH.Dec)] -> MetaM (Core (M TH.Dec))
repVal (MkC p) (MkC b) (MkC ds) = rep2 valDName [p, b, ds]

repFun :: Core TH.Name -> Core [(M TH.Clause)] -> MetaM (Core (M TH.Dec))
repFun (MkC nm) (MkC b) = rep2 funDName [nm, b]

repData :: Core (M TH.Cxt) -> Core TH.Name
        -> Either (Core [(M (TH.TyVarBndr ()))])
                  (Core (Maybe [(M (TH.TyVarBndr ()))]), Core (M TH.Type))
        -> Core (Maybe (M TH.Kind)) -> Core [(M TH.Con)] -> Core [M TH.DerivClause]
        -> MetaM (Core (M TH.Dec))
repData (MkC cxt) (MkC nm) (Left (MkC tvs)) (MkC ksig) (MkC cons) (MkC derivs)
  = rep2 dataDName [cxt, nm, tvs, ksig, cons, derivs]
repData (MkC cxt) (MkC _) (Right (MkC mb_bndrs, MkC ty)) (MkC ksig) (MkC cons)
        (MkC derivs)
  = rep2 dataInstDName [cxt, mb_bndrs, ty, ksig, cons, derivs]

repNewtype :: Core (M TH.Cxt) -> Core TH.Name
           -> Either (Core [(M (TH.TyVarBndr ()))])
                     (Core (Maybe [(M (TH.TyVarBndr ()))]), Core (M TH.Type))
           -> Core (Maybe (M TH.Kind)) -> Core (M TH.Con) -> Core [M TH.DerivClause]
           -> MetaM (Core (M TH.Dec))
repNewtype (MkC cxt) (MkC nm) (Left (MkC tvs)) (MkC ksig) (MkC con)
           (MkC derivs)
  = rep2 newtypeDName [cxt, nm, tvs, ksig, con, derivs]
repNewtype (MkC cxt) (MkC _) (Right (MkC mb_bndrs, MkC ty)) (MkC ksig) (MkC con)
           (MkC derivs)
  = rep2 newtypeInstDName [cxt, mb_bndrs, ty, ksig, con, derivs]

repTySyn :: Core TH.Name -> Core [(M (TH.TyVarBndr ()))]
         -> Core (M TH.Type) -> MetaM (Core (M TH.Dec))
repTySyn (MkC nm) (MkC tvs) (MkC rhs)
  = rep2 tySynDName [nm, tvs, rhs]

repInst :: Core (Maybe TH.Overlap) ->
           Core (M TH.Cxt) -> Core (M TH.Type) -> Core [(M TH.Dec)] -> MetaM (Core (M TH.Dec))
repInst (MkC o) (MkC cxt) (MkC ty) (MkC ds) = rep2 instanceWithOverlapDName
                                                              [o, cxt, ty, ds]

repDerivStrategy :: Maybe (LDerivStrategy GhcRn)
                 -> (Core (Maybe (M TH.DerivStrategy)) -> MetaM (Core (M a)))
                 -> MetaM (Core (M a))
repDerivStrategy mds thing_inside =
  case mds of
    Nothing -> thing_inside =<< nothing
    Just ds ->
      case unLoc ds of
        StockStrategy    _ -> thing_inside =<< just =<< repStockStrategy
        AnyclassStrategy _ -> thing_inside =<< just =<< repAnyclassStrategy
        NewtypeStrategy  _ -> thing_inside =<< just =<< repNewtypeStrategy
        ViaStrategy ty     -> addSimpleTyVarBinds (get_scoped_tvs_from_sig ty) $
                              do ty' <- rep_ty_sig' ty
                                 via_strat <- repViaStrategy ty'
                                 m_via_strat <- just via_strat
                                 thing_inside m_via_strat
  where
  nothing = coreNothingM derivStrategyTyConName
  just    = coreJustM    derivStrategyTyConName

repStockStrategy :: MetaM (Core (M TH.DerivStrategy))
repStockStrategy = rep2 stockStrategyName []

repAnyclassStrategy :: MetaM (Core (M TH.DerivStrategy))
repAnyclassStrategy = rep2 anyclassStrategyName []

repNewtypeStrategy :: MetaM (Core (M TH.DerivStrategy))
repNewtypeStrategy = rep2 newtypeStrategyName []

repViaStrategy :: Core (M TH.Type) -> MetaM (Core (M TH.DerivStrategy))
repViaStrategy (MkC t) = rep2 viaStrategyName [t]

repOverlap :: Maybe OverlapMode -> MetaM (Core (Maybe TH.Overlap))
repOverlap mb =
  case mb of
    Nothing -> nothing
    Just o ->
      case o of
        NoOverlap _    -> nothing
        Overlappable _ -> just =<< dataCon overlappableDataConName
        Overlapping _  -> just =<< dataCon overlappingDataConName
        Overlaps _     -> just =<< dataCon overlapsDataConName
        Incoherent _   -> just =<< dataCon incoherentDataConName
  where
  nothing = coreNothing overlapTyConName
  just    = coreJust overlapTyConName


repClass :: Core (M TH.Cxt) -> Core TH.Name -> Core [(M (TH.TyVarBndr ()))]
         -> Core [TH.FunDep] -> Core [(M TH.Dec)]
         -> MetaM (Core (M TH.Dec))
repClass (MkC cxt) (MkC cls) (MkC tvs) (MkC fds) (MkC ds)
  = rep2 classDName [cxt, cls, tvs, fds, ds]

repDeriv :: Core (Maybe (M TH.DerivStrategy))
         -> Core (M TH.Cxt) -> Core (M TH.Type)
         -> MetaM (Core (M TH.Dec))
repDeriv (MkC ds) (MkC cxt) (MkC ty)
  = rep2 standaloneDerivWithStrategyDName [ds, cxt, ty]

repPragInl :: Core TH.Name -> Core TH.Inline -> Core TH.RuleMatch
           -> Core TH.Phases -> MetaM (Core (M TH.Dec))
repPragInl (MkC nm) (MkC inline) (MkC rm) (MkC phases)
  = rep2 pragInlDName [nm, inline, rm, phases]

repPragSpec :: Core TH.Name -> Core (M TH.Type) -> Core TH.Phases
            -> MetaM (Core (M TH.Dec))
repPragSpec (MkC nm) (MkC ty) (MkC phases)
  = rep2 pragSpecDName [nm, ty, phases]

repPragSpecInl :: Core TH.Name -> Core (M TH.Type) -> Core TH.Inline
               -> Core TH.Phases -> MetaM (Core (M TH.Dec))
repPragSpecInl (MkC nm) (MkC ty) (MkC inline) (MkC phases)
  = rep2 pragSpecInlDName [nm, ty, inline, phases]

repPragSpecInst :: Core (M TH.Type) -> MetaM (Core (M TH.Dec))
repPragSpecInst (MkC ty) = rep2 pragSpecInstDName [ty]

repPragComplete :: Core [TH.Name] -> Core (Maybe TH.Name) -> MetaM (Core (M TH.Dec))
repPragComplete (MkC cls) (MkC mty) = rep2 pragCompleteDName [cls, mty]

repPragRule :: Core String -> Core (Maybe [(M (TH.TyVarBndr ()))])
            -> Core [(M TH.RuleBndr)] -> Core (M TH.Exp) -> Core (M TH.Exp)
            -> Core TH.Phases -> MetaM (Core (M TH.Dec))
repPragRule (MkC nm) (MkC ty_bndrs) (MkC tm_bndrs) (MkC lhs) (MkC rhs) (MkC phases)
  = rep2 pragRuleDName [nm, ty_bndrs, tm_bndrs, lhs, rhs, phases]

repPragAnn :: Core TH.AnnTarget -> Core (M TH.Exp) -> MetaM (Core (M TH.Dec))
repPragAnn (MkC targ) (MkC e) = rep2 pragAnnDName [targ, e]

repTySynInst :: Core (M TH.TySynEqn) -> MetaM (Core (M TH.Dec))
repTySynInst (MkC eqn)
    = rep2 tySynInstDName [eqn]

repDataFamilyD :: Core TH.Name -> Core [(M (TH.TyVarBndr ()))]
               -> Core (Maybe (M TH.Kind)) -> MetaM (Core (M TH.Dec))
repDataFamilyD (MkC nm) (MkC tvs) (MkC kind)
    = rep2 dataFamilyDName [nm, tvs, kind]

repOpenFamilyD :: Core TH.Name
               -> Core [(M (TH.TyVarBndr ()))]
               -> Core (M TH.FamilyResultSig)
               -> Core (Maybe TH.InjectivityAnn)
               -> MetaM (Core (M TH.Dec))
repOpenFamilyD (MkC nm) (MkC tvs) (MkC result) (MkC inj)
    = rep2 openTypeFamilyDName [nm, tvs, result, inj]

repClosedFamilyD :: Core TH.Name
                 -> Core [(M (TH.TyVarBndr ()))]
                 -> Core (M TH.FamilyResultSig)
                 -> Core (Maybe TH.InjectivityAnn)
                 -> Core [(M TH.TySynEqn)]
                 -> MetaM (Core (M TH.Dec))
repClosedFamilyD (MkC nm) (MkC tvs) (MkC res) (MkC inj) (MkC eqns)
    = rep2 closedTypeFamilyDName [nm, tvs, res, inj, eqns]

repTySynEqn :: Core (Maybe [(M (TH.TyVarBndr ()))]) ->
               Core (M TH.Type) -> Core (M TH.Type) -> MetaM (Core (M TH.TySynEqn))
repTySynEqn (MkC mb_bndrs) (MkC lhs) (MkC rhs)
  = rep2 tySynEqnName [mb_bndrs, lhs, rhs]

repRoleAnnotD :: Core TH.Name -> Core [TH.Role] -> MetaM (Core (M TH.Dec))
repRoleAnnotD (MkC n) (MkC roles) = rep2 roleAnnotDName [n, roles]

repFunDep :: Core [TH.Name] -> Core [TH.Name] -> MetaM (Core TH.FunDep)
repFunDep (MkC xs) (MkC ys) = rep2_nw funDepName [xs, ys]

repProto :: Name -> Core TH.Name -> Core (M TH.Type) -> MetaM (Core (M TH.Dec))
repProto mk_sig (MkC s) (MkC ty) = rep2 mk_sig [s, ty]

repImplicitParamBind :: Core String -> Core (M TH.Exp) -> MetaM (Core (M TH.Dec))
repImplicitParamBind (MkC n) (MkC e) = rep2 implicitParamBindDName [n, e]

repCtxt :: Core [(M TH.Pred)] -> MetaM (Core (M TH.Cxt))
repCtxt (MkC tys) = rep2 cxtName [tys]

repH98DataCon :: LocatedN Name
              -> HsConDeclH98Details GhcRn
              -> MetaM (Core (M TH.Con))
repH98DataCon con details
    = do con' <- lookupLOcc con -- See Note [Binders and occurrences]
         case details of
           PrefixCon _ ps -> do
             arg_tys <- repPrefixConArgs ps
             rep2 normalCName [unC con', unC arg_tys]
           InfixCon st1 st2 -> do
             verifyLinearFields [st1, st2]
             arg1 <- repBangTy (hsScaledThing st1)
             arg2 <- repBangTy (hsScaledThing st2)
             rep2 infixCName [unC arg1, unC con', unC arg2]
           RecCon ips -> do
             arg_vtys <- repRecConArgs ips
             rep2 recCName [unC con', unC arg_vtys]

repGadtDataCons :: [LocatedN Name]
                -> HsConDeclGADTDetails GhcRn
                -> LHsType GhcRn
                -> MetaM (Core (M TH.Con))
repGadtDataCons cons details res_ty
    = do cons' <- mapM lookupLOcc cons -- See Note [Binders and occurrences]
         case details of
           PrefixConGADT ps -> do
             arg_tys <- repPrefixConArgs ps
             res_ty' <- repLTy res_ty
             rep2 gadtCName [ unC (nonEmptyCoreList cons'), unC arg_tys, unC res_ty']
           RecConGADT ips _ -> do
             arg_vtys <- repRecConArgs ips
             res_ty'  <- repLTy res_ty
             rep2 recGadtCName [unC (nonEmptyCoreList cons'), unC arg_vtys,
                                unC res_ty']

-- TH currently only supports linear constructors.
-- We also accept the (->) arrow when -XLinearTypes is off, because this
-- denotes a linear field.
-- This check is not performed in repRecConArgs, since the GADT record
-- syntax currently does not have a way to mark fields as nonlinear.
verifyLinearFields :: [HsScaled GhcRn (LHsType GhcRn)] -> MetaM ()
verifyLinearFields ps = do
  linear <- lift $ xoptM LangExt.LinearTypes
  let allGood = all (\st -> case hsMult st of
                              HsUnrestrictedArrow _ -> not linear
                              HsLinearArrow _       -> True
                              _                     -> False) ps
  unless allGood $ notHandled ThNonLinearDataCon

-- Desugar the arguments in a data constructor declared with prefix syntax.
repPrefixConArgs :: [HsScaled GhcRn (LHsType GhcRn)]
                 -> MetaM (Core [M TH.BangType])
repPrefixConArgs ps = do
  verifyLinearFields ps
  repListM bangTypeTyConName repBangTy (map hsScaledThing ps)

-- Desugar the arguments in a data constructor declared with record syntax.
repRecConArgs :: LocatedL [LConDeclField GhcRn]
              -> MetaM (Core [M TH.VarBangType])
repRecConArgs ips = do
  args     <- concatMapM rep_ip (unLoc ips)
  coreListM varBangTypeTyConName args
    where
      rep_ip (L _ ip) = mapM (rep_one_ip (cd_fld_type ip)) (cd_fld_names ip)

      rep_one_ip :: LBangType GhcRn -> LFieldOcc GhcRn -> MetaM (Core (M TH.VarBangType))
      rep_one_ip t n = do { MkC v  <- lookupOcc (foExt $ unLoc n)
                          ; MkC ty <- repBangTy  t
                          ; rep2 varBangTypeName [v,ty] }

------------ Types -------------------

repTForall :: Core [(M (TH.TyVarBndr TH.Specificity))] -> Core (M TH.Cxt) -> Core (M TH.Type)
           -> MetaM (Core (M TH.Type))
repTForall (MkC tvars) (MkC ctxt) (MkC ty)
    = rep2 forallTName [tvars, ctxt, ty]

repTForallVis :: Core [(M (TH.TyVarBndr ()))] -> Core (M TH.Type)
              -> MetaM (Core (M TH.Type))
repTForallVis (MkC tvars) (MkC ty) = rep2 forallVisTName [tvars, ty]

repTvar :: Core TH.Name -> MetaM (Core (M TH.Type))
repTvar (MkC s) = rep2 varTName [s]

repTapp :: Core (M TH.Type) -> Core (M TH.Type) -> MetaM (Core (M TH.Type))
repTapp (MkC t1) (MkC t2) = rep2 appTName [t1, t2]

repTappKind :: Core (M TH.Type) -> Core (M TH.Kind) -> MetaM (Core (M TH.Type))
repTappKind (MkC ty) (MkC ki) = rep2 appKindTName [ty,ki]

repTapps :: Core (M TH.Type) -> [Core (M TH.Type)] -> MetaM (Core (M TH.Type))
repTapps f []     = return f
repTapps f (t:ts) = do { f1 <- repTapp f t; repTapps f1 ts }

repTSig :: Core (M TH.Type) -> Core (M TH.Kind) -> MetaM (Core (M TH.Type))
repTSig (MkC ty) (MkC ki) = rep2 sigTName [ty, ki]

repTequality :: MetaM (Core (M TH.Type))
repTequality = rep2 equalityTName []

repTPromotedList :: [Core (M TH.Type)] -> MetaM (Core (M TH.Type))
repTPromotedList []     = repPromotedNilTyCon
repTPromotedList (t:ts) = do  { tcon <- repPromotedConsTyCon
                              ; f <- repTapp tcon t
                              ; t' <- repTPromotedList ts
                              ; repTapp f t'
                              }

repTLit :: Core (M TH.TyLit) -> MetaM (Core (M TH.Type))
repTLit (MkC lit) = rep2 litTName [lit]

repTWildCard :: MetaM (Core (M TH.Type))
repTWildCard = rep2 wildCardTName []

repTImplicitParam :: Core String -> Core (M TH.Type) -> MetaM (Core (M TH.Type))
repTImplicitParam (MkC n) (MkC e) = rep2 implicitParamTName [n, e]

repTStar :: MetaM (Core (M TH.Type))
repTStar = rep2 starKName []

repTConstraint :: MetaM (Core (M TH.Type))
repTConstraint = rep2 constraintKName []

--------- Type constructors --------------

repNamedTyCon :: Core TH.Name -> MetaM (Core (M TH.Type))
repNamedTyCon (MkC s) = rep2 conTName [s]

repTInfix :: Core (M TH.Type) -> Core TH.Name -> Core (M TH.Type)
             -> MetaM (Core (M TH.Type))
repTInfix (MkC t1) (MkC name) (MkC t2) = rep2 infixTName [t1,name,t2]

repTupleTyCon :: Int -> MetaM (Core (M TH.Type))
-- Note: not Core Int; it's easier to be direct here
repTupleTyCon i = do platform <- getPlatform
                     rep2 tupleTName [mkIntExprInt platform i]

repUnboxedTupleTyCon :: Int -> MetaM (Core (M TH.Type))
-- Note: not Core Int; it's easier to be direct here
repUnboxedTupleTyCon i = do platform <- getPlatform
                            rep2 unboxedTupleTName [mkIntExprInt platform i]

repUnboxedSumTyCon :: TH.SumArity -> MetaM (Core (M TH.Type))
-- Note: not Core TH.SumArity; it's easier to be direct here
repUnboxedSumTyCon arity = do platform <- getPlatform
                              rep2 unboxedSumTName [mkIntExprInt platform arity]

repArrowTyCon :: MetaM (Core (M TH.Type))
repArrowTyCon = rep2 arrowTName []

repMulArrowTyCon :: MetaM (Core (M TH.Type))
repMulArrowTyCon = rep2 mulArrowTName []

repListTyCon :: MetaM (Core (M TH.Type))
repListTyCon = rep2 listTName []

repPromotedDataCon :: Core TH.Name -> MetaM (Core (M TH.Type))
repPromotedDataCon (MkC s) = rep2 promotedTName [s]

repPromotedTupleTyCon :: Int -> MetaM (Core (M TH.Type))
repPromotedTupleTyCon i = do platform <- getPlatform
                             rep2 promotedTupleTName [mkIntExprInt platform i]

repPromotedNilTyCon :: MetaM (Core (M TH.Type))
repPromotedNilTyCon = rep2 promotedNilTName []

repPromotedConsTyCon :: MetaM (Core (M TH.Type))
repPromotedConsTyCon = rep2 promotedConsTName []

----------------------------------------------------------
--       Type family result signature

repNoSig :: MetaM (Core (M TH.FamilyResultSig))
repNoSig = rep2 noSigName []

repKindSig :: Core (M TH.Kind) -> MetaM (Core (M TH.FamilyResultSig))
repKindSig (MkC ki) = rep2 kindSigName [ki]

repTyVarSig :: Core (M (TH.TyVarBndr ())) -> MetaM (Core (M TH.FamilyResultSig))
repTyVarSig (MkC bndr) = rep2 tyVarSigName [bndr]

----------------------------------------------------------
--              Literals

repLiteral :: HsLit GhcRn -> MetaM (Core TH.Lit)
repLiteral (HsStringPrim _ bs)
  = do word8_ty <- lookupType word8TyConName
       let w8s = unpack bs
           w8s_expr = map (\w8 -> mkCoreConApps word8DataCon
                                  [mkWord8Lit (toInteger w8)]) w8s
       rep2_nw stringPrimLName [mkListExpr word8_ty w8s_expr]
repLiteral lit
  = do lit' <- case lit of
                   HsIntPrim _ i    -> mk_integer i
                   HsWordPrim _ w   -> mk_integer w
                   HsInt _ i        -> mk_integer (il_value i)
                   HsFloatPrim _ r  -> mk_rational r
                   HsDoublePrim _ r -> mk_rational r
                   HsCharPrim _ c   -> mk_char c
                   _ -> return lit
       lit_expr <- lift $ dsLit lit'
       case mb_lit_name of
          Just lit_name -> rep2_nw lit_name [lit_expr]
          Nothing -> notHandled (ThExoticLiteral lit)
  where
    mb_lit_name = case lit of
                 HsInteger _ _ _  -> Just integerLName
                 HsInt _ _        -> Just integerLName
                 HsIntPrim _ _    -> Just intPrimLName
                 HsWordPrim _ _   -> Just wordPrimLName
                 HsFloatPrim _ _  -> Just floatPrimLName
                 HsDoublePrim _ _ -> Just doublePrimLName
                 HsChar _ _       -> Just charLName
                 HsCharPrim _ _   -> Just charPrimLName
                 HsString _ _     -> Just stringLName
                 HsRat _ _ _      -> Just rationalLName
                 _                -> Nothing

mk_integer :: Integer -> MetaM (HsLit GhcRn)
mk_integer  i = return $ HsInteger NoSourceText i integerTy

mk_rational :: FractionalLit -> MetaM (HsLit GhcRn)
mk_rational r = do rat_ty <- lookupType rationalTyConName
                   return $ HsRat noExtField r rat_ty
mk_string :: FastString -> MetaM (HsLit GhcRn)
mk_string s = return $ HsString NoSourceText s

mk_char :: Char -> MetaM (HsLit GhcRn)
mk_char c = return $ HsChar NoSourceText c

repOverloadedLiteral :: HsOverLit GhcRn -> MetaM (Core TH.Lit)
repOverloadedLiteral (OverLit { ol_val = val})
  = do { lit <- mk_lit val; repLiteral lit }
        -- The type Rational will be in the environment, because
        -- the smart constructor 'TH.Syntax.rationalL' uses it in its type,
        -- and rationalL is sucked in when any TH stuff is used

mk_lit :: OverLitVal -> MetaM (HsLit GhcRn)
mk_lit (HsIntegral i)     = mk_integer  (il_value i)
mk_lit (HsFractional f)   = mk_rational f
mk_lit (HsIsString _ s)   = mk_string   s

repNameS :: Core String -> MetaM (Core TH.Name)
repNameS (MkC name) = rep2_nw mkNameSName [name]

--------------- Miscellaneous -------------------

repGensym :: Core String -> MetaM (Core (M TH.Name))
repGensym (MkC lit_str) = rep2 newNameName [lit_str]

repBindM :: Type -> Type        -- a and b
         -> Core (M a) -> Core (a -> M b) -> MetaM (Core (M b))
repBindM ty_a ty_b (MkC x) (MkC y)
  = rep2M bindMName [Type ty_a, Type ty_b, x, y]

repSequenceM :: Type -> Core [M a] -> MetaM (Core (M [a]))
repSequenceM ty_a (MkC list)
  = rep2M sequenceQName [Type ty_a, list]

repUnboundVar :: Core TH.Name -> MetaM (Core (M TH.Exp))
repUnboundVar (MkC name) = rep2 unboundVarEName [name]

repOverLabel :: FastString -> MetaM (Core (M TH.Exp))
repOverLabel fs = do
                    (MkC s) <- coreStringLit $ unpackFS fs
                    rep2 labelEName [s]

repGetField :: Core (M TH.Exp) -> FastString -> MetaM (Core (M TH.Exp))
repGetField (MkC exp) fs = do
  MkC s <- coreStringLit $ unpackFS fs
  rep2 getFieldEName [exp,s]

repProjection :: NonEmpty FastString -> MetaM (Core (M TH.Exp))
repProjection fs = do
  MkC xs <- coreListNonEmpty stringTy <$> mapM (coreStringLit . unpackFS) fs
  rep2 projectionEName [xs]

------------ Lists -------------------
-- turn a list of patterns into a single pattern matching a list

repList :: Name -> (a  -> MetaM (Core b))
                    -> [a] -> MetaM (Core [b])
repList tc_name f args
  = do { args1 <- mapM f args
       ; coreList tc_name args1 }

-- Create a list of m a values
repListM :: Name -> (a  -> MetaM (Core b))
                    -> [a] -> MetaM (Core [b])
repListM tc_name f args
  = do { ty <- wrapName tc_name
       ; args1 <- mapM f args
       ; return $ coreList' ty args1 }

coreListM :: Name -> [Core a] -> MetaM (Core [a])
coreListM tc as = repListM tc return as

coreList :: Name    -- Of the TyCon of the element type
         -> [Core a] -> MetaM (Core [a])
coreList tc_name es
  = do { elt_ty <- lookupType tc_name; return (coreList' elt_ty es) }

coreList' :: Type       -- The element type
          -> [Core a] -> Core [a]
coreList' elt_ty es = MkC (mkListExpr elt_ty (map unC es ))

coreListNonEmpty :: Type -> NonEmpty (Core a) -> Core (NonEmpty a)
coreListNonEmpty ty (MkC x :| xs) = MkC $ mkNonEmptyListExpr ty x (map unC xs)

nonEmptyCoreList :: [Core a] -> Core [a]
  -- The list must be non-empty so we can get the element type
  -- Otherwise use coreList
nonEmptyCoreList []           = panic "coreList: empty argument"
nonEmptyCoreList xs@(MkC x:_) = MkC (mkListExpr (exprType x) (map unC xs))


coreStringLit :: MonadThings m => String -> m (Core String)
coreStringLit s = do { z <- mkStringExpr s; return(MkC z) }

------------------- Maybe ------------------

repMaybe :: Name -> (a -> MetaM (Core b))
                    -> Maybe a -> MetaM (Core (Maybe b))
repMaybe tc_name f m = do
  t <- lookupType tc_name
  repMaybeT t f m

repMaybeT :: Type -> (a -> MetaM (Core b))
                    -> Maybe a -> MetaM (Core (Maybe b))
repMaybeT ty _ Nothing   = return $ coreNothing' ty
repMaybeT ty f (Just es) = coreJust' ty <$> f es

-- | Construct Core expression for Nothing of a given type name
coreNothing :: Name        -- ^ Name of the TyCon of the element type
            -> MetaM (Core (Maybe a))
coreNothing tc_name =
    do { elt_ty <- lookupType tc_name; return (coreNothing' elt_ty) }

coreNothingM :: Name -> MetaM (Core (Maybe a))
coreNothingM tc_name =
    do { elt_ty <- wrapName tc_name; return (coreNothing' elt_ty) }

-- | Construct Core expression for Nothing of a given type
coreNothing' :: Type       -- ^ The element type
             -> Core (Maybe a)
coreNothing' elt_ty = MkC (mkNothingExpr elt_ty)

-- | Store given Core expression in a Just of a given type name
coreJust :: Name        -- ^ Name of the TyCon of the element type
         -> Core a -> MetaM (Core (Maybe a))
coreJust tc_name es
  = do { elt_ty <- lookupType tc_name; return (coreJust' elt_ty es) }

coreJustM :: Name -> Core a -> MetaM (Core (Maybe a))
coreJustM tc_name es = do { elt_ty <- wrapName tc_name; return (coreJust' elt_ty es) }

-- | Store given Core expression in a Just of a given type
coreJust' :: Type       -- ^ The element type
          -> Core a -> Core (Maybe a)
coreJust' elt_ty es = MkC (mkJustExpr elt_ty (unC es))

------------------- Maybe Lists ------------------

coreJustList :: Type -> Core [a] -> Core (Maybe [a])
coreJustList elt_ty = coreJust' (mkListTy elt_ty)

coreNothingList :: Type -> Core (Maybe [a])
coreNothingList elt_ty = coreNothing' (mkListTy elt_ty)

------------ Literals & Variables -------------------

coreIntLit :: Int -> MetaM (Core Int)
coreIntLit i = do platform <- getPlatform
                  return (MkC (mkIntExprInt platform i))

coreVar :: Id -> Core TH.Name   -- The Id has type Name
coreVar id = MkC (Var id)

----------------- Failure -----------------------
notHandledL :: SrcSpan -> ThRejectionReason -> MetaM a
notHandledL loc reason
  | isGoodSrcSpan loc
  = mapReaderT (putSrcSpanDs loc) $ notHandled reason
  | otherwise
  = notHandled reason

notHandled :: ThRejectionReason -> MetaM a
notHandled reason = lift $ failWithDs (DsNotYetHandledByTH reason)
