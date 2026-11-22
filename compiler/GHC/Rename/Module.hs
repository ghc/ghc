{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}


{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Main pass of renamer
-}

module GHC.Rename.Module (
        rnSrcDecls, SourceTableUse(..), splitLocalSourceTables, addTcgDUs, findSplice, rnWarningTxt, rnLWarningTxt
    ) where

import GHC.Prelude hiding ( head )

import {-# SOURCE #-} GHC.Rename.Expr( rnLExpr )
import {-# SOURCE #-} GHC.Rename.Splice ( rnSpliceDecl, rnTopSpliceDecls, rnSpliceTypeWith )

import GHC.Hs
import qualified GHC.Hs.Decls as HsDecls
import GHC.Hs.Type (mkHsImplicitSigType, HsTypeGhcPsExt(HsTypeCase, HsTypeCaseAlias, HsTypeLet, HsTypeSome), HsTypeCaseAlt(..), mkHsSingleTypeLet, splitHsTypeLet, preserveTypeLetSource)
import GHC.Hs.Decls.Overlap ( OverlapMode(..) )
import GHC.Iface.Env ( lookupOrig )

import GHC.Rename.HsType
import GHC.Rename.Bind
import GHC.Rename.Doc
import GHC.Rename.Env
import GHC.Rename.Utils ( mapFvRn, bindLocalNames
                        , checkDupRdrNames, bindLocalNamesFV
                        , warnUnusedTypePatterns, warnPunBindingsAgainst
                        , noNestedForallsContextsErr
                        , addNoNestedForallsContextsErr, checkInferredVars
                        , makeRnValBinds)
import GHC.Rename.Unbound ( notInScopeErr, WhereLooking(WL_Global)
                          , reportUnboundName, WhatLooking(WL_Anything) )
import GHC.Rename.Names

import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Env (isBrackLevel)
import GHC.Tc.Utils.Monad
import GHC.Tc.Types.Origin ( TypedThing(..) )

import GHC.Unit
import GHC.Unit.Module.Warnings
import GHC.Builtin( mkUnboundName )
import GHC.Builtin.KnownKeys
import GHC.Builtin.KnownOccs (consDataCon_RDR)
import GHC.Builtin.WiredIn.Types (liftedTypeKindTyConName)

import GHC.Types.FieldLabel
import GHC.Types.Name.Reader
import GHC.Types.ForeignCall
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Name.Env
import GHC.Types.Basic  ( VisArity,  TyConFlavour(..), TypeOrKind(..), NewOrData(..), RuleName, RecFlag )
import GHC.Types.GREInfo (ConLikeInfo (..), ConInfo, mkConInfo, conInfoFields)
import GHC.Types.Hint (SigLike(..))
import GHC.Types.Unique.Set
import GHC.Types.SrcLoc as SrcLoc

import GHC.Driver.DynFlags
import GHC.Driver.Env ( HscEnv(..), hsc_home_unit)

import GHC.Utils.Misc   ( lengthExceeds )
import GHC.Utils.Panic
import GHC.Utils.Outputable

import GHC.Data.FastString
import GHC.Data.List.SetOps ( findDupsEq, removeDupsOn, equivClasses )
import GHC.Data.Graph.Directed ( SCC, flattenSCC, Node(..)
                               , stronglyConnCompFromEdgedVerticesUniq )
import GHC.Data.OrdList
import qualified GHC.LanguageExtensions as LangExt
import Language.Haskell.Syntax.Specificity (Specificity(SpecifiedSpec))

import Control.Monad
import Data.Bifunctor ( first, second )
import Data.Foldable ( toList, for_ )
import Data.List ( elemIndex, find, mapAccumL, nub, partition, unzip4 )
import Data.List.NonEmpty ( NonEmpty(..), head, nonEmpty )
import Data.Data (Data, cast, gmapQ, gmapT)
import Data.Maybe (catMaybes,  isJust, isNothing, fromMaybe, mapMaybe, maybeToList )
import qualified Data.Set as Set ( delete, difference, disjoint, empty, fromList, member, null, toList, union )

{- Note [Lifting type lambdas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Lower a type lambda to a closed family before declaration dependency
analysis. Keep the original kind annotation so the typechecker checks
the generated application against the source kind. Prefix lexical captures
and give the generated family one more argument for the lambda binder.
Give the lifted family and the synonym the same kind-checking SCC.
Constraints from later arguments can fix the kind of the lambda binder
before kind generalization. The existing family checker checks the
scope, kind and beta equation.

Keep the capture's original header binder and kind. Earlier header
variables scope over later kinds. An equation can give a shadowed
capture a private name while retaining the original name for the new
lambda binder. Pass all captures to a nested family, including private
equation names: their values can still occur in the kind of another
capture.

For example, if a synonym binds (q :: Type) (a :: q), a lambda that
binds q must still carry the original q so its header can bind a.
A nested lambda then passes that original q using the parent
equation's private name, while source occurrences of q still refer
to the nearest lambda.

The ordinary declaration renamer brings implicit kind variables
from the synonym header and its outer kind annotation into scope.
Make those variables explicit parameters of each lifted family.
Keep the original synonym's implicit binders and their order.

If the lambda body is a case on one of its arguments, generate one
family equation per constructor alternative. This leaves the case
stuck on an unknown argument, even when the alternatives agree.
A branch may use the whole matched value. See Note [Type case aliases]
in GHC.Tc.TyCl.

A closed written polymorphic kind on a tail-case lambda also provides
a standalone signature for the family. Check the kind before its
equations, so a constructor pattern cannot fix a kind variable for
every branch. Preserve the written forall binders. Give closed captures
parameters before that telescope. Keep the normal inference path when
a capture's kind depends on the enclosing context.

A retained dependent sum uses the same lifting. Lower
some (x :: A) | B to Sigma A with the lifted lambda for B. The
name Sigma comes from GHC.Sigma in the configured base unit.
The witness domain stays outside the lambda scope. Sigma's
payload parameter checks the kind of the lifted family.

With explicitly kinded captures, a later capture or the witness domain
can depend on an earlier capture. For example, in
  type Keep (n :: N) (m :: N) (x :: Proxy (Add n m)) =
    some (tag :: Bool) | (x :~: x)
the helper needs a kind of the form
  forall (n :: N) (m :: N) -> Proxy (Add n m) -> Bool -> Type
The anonymous binders of the provisional family kind do not substitute
the actual arguments for n and m in the kind of x. Give the hidden
family a standalone kind signature when all the written kinds determine
the telescope. Keep the inference path for incomplete kinds and do not
copy source splices into the signature.

A nested sum also captures the enclosing witness. Keep the written
domain on the witness header when the same syntax checks as a type
and a kind. Otherwise, infer the domain from the Sigma application.
A written domain scopes over the enclosing equation's variables.
Restore those names with type lets if the header has private names.
For example, a domain of Proxy x after some (x :: A) must refer to
the witness header, even when it shadows a synonym parameter x.
See Note [Provisional kinds of sum payloads] in GHC.Tc.TyCl.

A closed type family equation can use a type lambda anywhere that a
synonym body can use one. Give each equation its own lifted families.
A type family equation can also return a sum. In both cases, take
captures from the equation binders. An implicit equation binds the
variables in its left-hand side, as in rnFamEqn.
Keep the binders that a capture's written kind needs. Keep the entire
scope when the result contains a splice. The free-variable traversal
cannot see the variables in the result of a splice. The original equation
still goes through the ordinary scope, kind, and termination checks.
For a top-level type instance, allocate a stable occurrence before lifting
the payload. The same open family can have multiple instances, including
instances in separate declaration groups. The payload helper belongs to
the instance module even if that module imports the family.
Check whether the equation would generate a helper before allocating
its owner. An ordinary equation must not consume a unique occurrence.
Use a separate occurrence for a lambda in an open family instance.
Its lexical captures come from that equation. The renamed instance
still goes through the usual type family validity and overlap checks.

In an associated instance, an explicit equation forall can use variables
from the enclosing instance head. Keep those captures as arguments to the
helper. rnFamEqn still checks that these variables occur on the left-hand
side. Copy only their names. An annotation on the instance head can
mention another variable outside the family equation. For a splice, pass
only enclosing variables written in the family pattern. The splice's
source does not show which variables its result uses. A class default has
only the scope of its own equation.

A type let binds a name only in its body. Lift callbacks in the RHS
with the outer captures. A callback in the body can capture the checked
let result as another argument. Give this argument a fresh family
parameter. When the let shadows an outer capture, save the outer value
before the let. Another capture can still depend on it in its kind.

An unused capture gets a wildcard in the generated equation. The
family still receives the capture because other arguments can depend
on it. Keep named patterns for written lambda binders so the renamer
can diagnose unused binders at the source.
-}
-- | Lift type lambdas and sum payloads from declaration right-hand sides.
-- See Note [Lifting type lambdas].
liftTypeLambdas :: RdrName -> HsGroup GhcPs
                -> RnM (HsGroup GhcPs, [(OccName, OccName)], OccEnv (OccName, Bool))
liftTypeLambdas sigma group = do
  (groups, hidden, scopes) <- unzip3 <$> traverse liftGroup (hs_tyclds group)
  return (group { hs_tyclds = groups }, concat hidden, mkOccEnv (concat scopes))
  where
    liftGroup decls = do
      (instances, lifted_insts) <- unzip <$> traverse liftInst (group_instds decls)
      (defs, lifted) <- unzip <$> traverse liftDecl (group_tyclds decls)
      let (families, hidden, signatures, scopes) = unzip4 (concat lifted ++ concat lifted_insts)
      return (decls { group_tyclds = defs ++ families
                    , group_kisigs = group_kisigs decls ++ catMaybes signatures
                    , group_instds = instances }
             , hidden, catMaybes scopes)

    liftInst (L loc inst@ClsInstD
                  { cid_inst = decl@ClsInstDecl{cid_poly_ty = header, cid_decls = nested} }) = do
      (nested', lifted) <- liftNested (instanceVars header) nested
      return (L loc inst{cid_inst = decl{cid_decls = nested'}}, lifted)
    liftInst inst = liftTypeInst [] inst

    liftNested inherited nested = do
      (nested', lifted) <- unzip <$> traverse liftDeclInst nested
      return (nested', concat lifted)
      where
        liftDeclInst (L loc (InstD ext inst)) = do
          (L _ inst', lifted) <- liftTypeInst inherited (L loc inst)
          return (L loc (InstD ext inst'), lifted)
        liftDeclInst decl = return (decl, [])

    instanceVars (L _ HsSig{sig_bndrs = outer, sig_body = body}) =
      case outer of
        HsOuterImplicit{} -> nubN (extractHsTyRdrTyVars body)
        HsOuterExplicit{hso_bndrs = binders} -> mapMaybe hsLTyVarLocName binders

    liftTypeInst inherited (L loc inst@TyFamInstD
                                { tfid_inst = decl@TyFamInstDecl
                                { tfid_eqn = eqn@FamEqn{feqn_tycon = owner, feqn_rhs = rhs} } })
      | let liftFor name = liftEquation inherited name (0 :: Int) (L loc eqn)
            (_, (_, candidates)) = liftFor owner
      , not (null candidates) = do
          let family = occNameString (rdrNameOcc (unLoc owner))
              stem | sumResult rhs = "$sum:" ++ family
                   | otherwise = "$callback:" ++ family ++ ":instance"
          occ <- chooseUniqueOccTc (mkInstTyTcOcc stem)
          let instance_owner = L (getLoc owner) (mkRdrUnqual occ)
              (_, (L _ eqn', lifted)) = liftFor instance_owner
          return (L loc inst{tfid_inst = decl{tfid_eqn = eqn'}}, lifted)
    liftTypeInst _ inst = return (inst, [])

    liftDecl (L loc syn@SynDecl{tcdLName = owner, tcdRhs = rhs,
                                tcdTyVars = HsQTvs{hsq_explicit = captures}})
      | Just scoped <- traverse capture captures
      = let (kind_occs, _) = hsQTyVarKindOccs captures (extractHsTyRdrTyVarsKindVars rhs)
            kinds = nubN kind_occs
            implicit = [(liftedTypeVarBinder k, k) | k <- kinds]
            (rhs', families, _) = liftRhs True Nothing owner (implicit ++ scoped) rhs (0 :: Int)
        in return (L loc syn{tcdRhs = rhs'}, families)
    liftDecl (L loc decl@FamDecl{tcdFam = family@FamilyDecl
                  { fdLName = owner, fdInfo = ClosedTypeFamily ann (Just rows) }}) =
      let (_, results) = mapAccumL (liftEquation [] owner) (0 :: Int) rows
          (rows', lifted) = unzip results
          family' = family{fdInfo = ClosedTypeFamily ann (Just rows')}
      in return (L loc decl{tcdFam = family'}, concat lifted)
    liftDecl (L loc cls@ClassDecl{tcdDecls = nested}) = do
      (nested', lifted) <- liftNested [] nested
      return (L loc cls{tcdDecls = nested'}, lifted)
    liftDecl decl = return (decl, [])

    liftEquation inherited owner salt (L loc eqn@FamEqn{feqn_rhs = rhs}) =
      let captures = equationCaptures inherited eqn
          free = Set.fromList (map unLoc (extractHsTyRdrTyVars rhs))
          used | spliceFree rhs = fst (foldr selectCapture ([], free) captures)
               | otherwise = captures
          (rhs', families, next) = liftRhs (sumResult rhs) Nothing owner used rhs salt
      in (next, (L loc eqn{feqn_rhs = rhs'}, families))

    sumResult (L _ (HsParTy _ body)) = sumResult body
    sumResult (L _ (HsKindSig _ body _)) = sumResult body
    sumResult (L _ (XHsType HsTypeSome{})) = True
    sumResult _ = False

    equationCaptures inherited FamEqn{feqn_bndrs = outer, feqn_pats = pats, feqn_rhs = rhs} =
      [(liftedTypeVarBinder v, v) | v <- inherited
                                , not (any (sameBinder v . snd) local)
                                , without_splice || any (sameBinder v) lhs] ++ local
      where
        without_splice = spliceFree rhs
        lhs = nubN (extractHsTyArgRdrKiTyVars pats)
        local = case outer of
          HsOuterImplicit{} -> [(liftedTypeVarBinder v, v) | v <- lhs]
          HsOuterExplicit{hso_bndrs = binders} ->
            mapMaybe (capture . fmap (setHsTyVarBndrFlag (HsBndrRequired noExtField))) binders

    selectCapture c@(binder, v) (used, free)
      | Set.member (unLoc v) free =
          let kinds = case unLoc binder of
                HsTvb{tvb_kind = HsBndrKind _ kind} ->
                  Set.fromList (map unLoc (extractHsTyRdrTyVars kind))
                _ -> Set.empty
          in (c : used, Set.union kinds (Set.delete (unLoc v) free))
      | otherwise = (used, free)

    liftRhs _ scoped owner captures (L loc (HsKindSig ann body kind)) salt
      | L _ HsForAllTy{} <- stripParens kind
      , closedSpliceFreeKind kind
      , Just capture_kinds <- traverse (closedCaptureKind . fst) captures
      , L lam_loc (XHsType (HsTypeLambda _ vars rhs)) <- stripParens body
      , (_, result) <- collectLambdas salt vars rhs
      , L _ (XHsType HsTypeCase{}) <- stripParens result
      = let (body', families, next) =
                liftLambdaRhs scoped Nothing owner captures lam_loc vars rhs salt
                  (Just (foldr nlHsFunTy kind capture_kinds))
        in (L loc (HsKindSig ann body' kind), families, next)
    liftRhs lift_sum scoped owner captures (L loc (HsKindSig ann body kind)) salt =
      let (body', families, next) = liftRhs lift_sum scoped owner captures body salt
      in (L loc (HsKindSig ann body' kind), families, next)
    liftRhs _ scoped owner captures (L loc (XHsType let_ty@(HsTypeLet ann _ _ binds result_ty))) salt =
      let (binder, rhs, body) = splitHsTypeLet binds result_ty
          (rhs', rhs_families, next) = liftRhs False scoped owner captures rhs salt
          saved = privateName "let-outer" next 0 binder
          header = liftedTypeVarBinder (privateName "let-header" next 0 binder)
          shadowed (_, name) = sameBinder binder name
          save c@(param, _) | shadowed c = (param, saved)
                            | otherwise  = c
          scope = map save captures ++ [(header, binder)]
          (body', body_families, end) = liftRhs False scoped owner scope body (next + 1)
          local = L loc (XHsType (mkHsSingleTypeLet ann binder rhs' body'))
          result | not (null body_families), any shadowed captures
                 = L loc (XHsType (mkHsSingleTypeLet noAnn saved (typeName binder) local))
                 | otherwise = local
      in (preserveTypeLetSource let_ty result, rhs_families ++ body_families, if null body_families then next else end)
    liftRhs _ scoped owner captures (L loc (XHsType (HsTypeLambda _ vars body))) salt =
      liftLambdaRhs scoped Nothing owner captures loc vars body salt Nothing
    liftRhs True scoped owner captures (L loc (XHsType (HsTypeSome _ binder domain body))) salt =
      let signature = sumPayloadKind captures domain
          (schema, families, next) = liftLambdaRhs scoped (Just domain) owner captures loc [binder] body salt signature
          sigma_ty = typeName (L (l2l loc) sigma)
          app f arg = L loc (HsAppTy noExtField f arg)
      in (app (app sigma_ty domain) schema, families, next)
    liftRhs lift_sum scoped owner captures ty salt =
      liftTypeChildren (liftRhs lift_sum scoped owner captures) ty salt

    -- A standalone kind signature quantifies the invisible prefix before
    -- checking any equation. If the first row could fix that prefix, GADT
    -- matches with different result indices would become monomorphic.
    -- 'capture' has already removed kinds that contain a splice.
    closedCaptureKind (L _ (HsTvb _ _ _ (HsBndrKind _ kind)))
      | null (extractHsTyRdrTyVars kind) = Just kind
    closedCaptureKind _ = Nothing

    -- The written annotation and capture binders remain in the synonym.
    -- A copied kind must not cause another execution of a source splice.
    closedSpliceFreeKind kind =
      null (extractHsTyRdrTyVars kind) && spliceFree kind

    spliceFree :: Data a => a -> Bool
    spliceFree node
      | Just (L _ HsSpliceTy{}) <- (cast node :: Maybe (LHsType GhcPs)) = False
      | otherwise = and (gmapQ spliceFree node)

    -- See Note [Lifting type lambdas]. Bind only captures that occur in a
    -- later kind. Check closure after binding the captures in source order.
    -- The written domain must refer to the same names in this signature.
    sumPayloadKind captures domain
      | Just binders <- traverse explicitCapture captures
      , completeKind domain
      , (kind, free, True) <- foldr addCapture (result, kindVars domain, False) binders
      , Set.null free
      = Just kind
      | otherwise = Nothing
      where
        result = nlHsFunTy domain (nlHsTyVar NotPromoted (Exact (ExactName liftedTypeKindTyConName)))
        kindVars = Set.fromList . map unLoc . extractHsTyRdrTyVars

        explicitCapture (binder@(L _ (HsTvb _ _ (HsBndrVar _ name) (HsBndrKind _ kind))), source)
          | completeKind kind
          , sameBinder name source || not (unLoc source `Set.member` kindVars domain)
          = Just (binder, unLoc name, kind)
        explicitCapture _ = Nothing

        addCapture (binder, name, kind) (body, free, dependent) =
          let used = Set.member name free
              kind' | used = L (getLoc kind) (HsForAllTy noExtField
                                 (mkHsForAllVisTele noAnn [fmap (setHsTyVarBndrFlag ()) binder]) body)
                    | otherwise = nlHsFunTy kind body
          in (kind', kindVars kind `Set.union` Set.delete name free, used || dependent)

    completeKind :: Data a => a -> Bool
    completeKind node =
      case cast node :: Maybe (LHsType GhcPs) of
        Just (L _ HsSpliceTy{})    -> False
        Just (L _ HsWildCardTy{})  -> False
        Just (L _ XHsType{})      -> False
        _ -> and (gmapQ completeKind node)

    -- See Note [Lifting type lambdas].
    witnessKind captures domain
      | completeKind domain
      , sameDomainMode domain
      , Set.null (free `Set.difference` scope)
      = Just (foldr restore domain captures)
      | otherwise = Nothing
      where
        free = Set.fromList (map unLoc (extractHsTyRdrTyVars domain))
        scope = Set.fromList (map (unLoc . snd) captures)
        restore (L _ HsTvb{tvb_var = HsBndrVar _ header}, source) body
          | not (sameBinder header source)
          , unLoc source `Set.member` free
          = L (getLoc body) (XHsType (mkHsSingleTypeLet noAnn source (typeName header) body))
        restore _ body = body

    -- For example, a bare arrow can change matchability in a kind.
    sameDomainMode :: Data a => a -> Bool
    sameDomainMode node = case cast node :: Maybe (LHsType GhcPs) of
      Just (L _ HsFunTy{}) -> False
      Just (L _ HsForAllTy{}) -> False
      Just (L _ HsForEachTy{}) -> False
      _ -> and (gmapQ sameDomainMode node)

    liftLambdaRhs outer_scope domain owner captures loc vars body salt signature =
      let owner_occ = rdrNameOcc (unLoc owner)
          is_sum = isJust domain
          scoped
            | Just scope@(_, sum_scope) <- outer_scope, sum_scope || not is_sum = scope
            | otherwise = (lambdaOcc owner_occ salt is_sum, is_sum)
          (binders, result) = collectLambdas salt vars body
          -- Keep source spellings in a malformed list so the normal family
          -- binder check diagnoses a duplicate at the written binder.
          header_names
            | hasDuplicates binders = binders
            | otherwise = zipWith (privateName "header" salt) [0..] binders
          local_binders = map liftedTypeVarBinder header_names
          locals = zip (annotateWitness local_binders) binders
          annotateWitness (L b_loc binder : rest)
            | Just kind <- domain >>= witnessKind captures
            = L b_loc binder{tvb_kind = HsBndrKind noExtField kind} : rest
          annotateWitness bs = bs
          equation_captures = zipWith (hideCapture binders salt) [0..] captures
          equation_scope = equation_captures ++ locals
          (body', families, next) =
            liftRhs True (Just scoped) owner equation_scope result (salt + 1)
          (applied, lifted) = liftLambda scoped is_sum owner loc captures equation_scope body' salt signature
      in (applied, lifted : families, next)
    liftLambda (root, sum_scope) is_sum owner loc captures equation_scope body salt signature =
      let owner_occ = rdrNameOcc (unLoc owner)
          occ = lambdaOcc owner_occ salt sum_scope
          name = L (getLoc owner) (mkRdrUnqual occ)
          equations
            | L _ (XHsType (HsTypeCase (case_kw, _) _ scrutinee alts)) <- stripParens body
            , L _ (HsTyVar _ NotPromoted scrut) <- stripParens scrutinee
            , [selected] <- [i | (i, (_, v)) <- zip [0 :: Int ..] equation_scope
                               , sameBinder v scrut]
            , selected >= length captures
            , Just branches <- traverse classifyBranch alts
            = map (caseEquation owner name selected equation_scope case_kw scrut) branches
            | otherwise
            = [equation loc name (capture_pats ++ binder_pats) body]
          (capture_names, binder_names) = splitAt (length captures) (map snd equation_scope)
          capture_pats = map capturePat capture_names
          binder_pats = map typeName binder_names
          free = Set.fromList (map unLoc (extractHsTyRdrTyVars body))
          splice_free = spliceFree body
          capturePat v
            | splice_free, not (unLoc v `Set.member` free)
            = L (l2l (getLoc v)) (HsWildCardTy (HoleVar (noLocA unnamedHoleRdrName)))
            | otherwise = typeName v
          family = L loc (FamDecl noExtField
                   (FamilyDecl noAnn (ClosedTypeFamily noAnn (Just equations)) TopLevel
                       name (HsQTvs noExtField (map fst equation_scope)) Prefix
                       (L (l2l loc) (NoSig noExtField)) Nothing))
          applied = foldl' (\f (_, n) -> L loc (HsAppTy noExtField f (typeName n)))
                           (typeName name) captures
          sig kind = L loc (StandaloneKindSig noAnn name
                       (L (getLoc kind) (mkHsImplicitSigType kind)))
      in (applied, (family, (occ, owner_occ), sig <$> signature, Just (occ, (root, is_sum))))

    lambdaOcc owner salt scoped =
      mkTcOcc ("$lambda:" ++ occNameString owner ++ ":" ++ show salt
                ++ if scoped then "-sigma" else "")

    -- A tail case on a lambda argument can be represented directly by
    -- the lambda's equations. Matching remains deferred until application.
    classifyBranch (L loc (HsTypeCaseAlt pat _ rhs)) = do
      (pat', bound) <- classifyTypeCasePattern pat
      pure (loc, pat', bound, rhs)

    caseEquation owner name selected scope case_kw scrut (loc, pat, bound, rhs) =
      equation loc name (zipWith argument [0..] scope) body
      where
        body = L (noAnnSrcSpan (getEpTokenSrcSpan case_kw))
                 (XHsType (HsTypeCaseAlias generated (owner, selected, alias) rhs))
        alias
          | unLoc scrut `notElem` bound
          , any ((== unLoc scrut) . unLoc) (extractHsTyRdrTyVars rhs)
          = Just scrut
          | otherwise = Nothing

        generated = [unLoc (captureName index v)
                    | (index, (_, v)) <- zip [0..] scope, index /= selected]
        argument index (_, v)
          | index == selected = pat
          | otherwise = typeName (captureName index v)
        captureName index v
          | unLoc v `elem` bound = privateName "case-capture" 0 index v
          | otherwise = v

    equation loc name params =
      L loc . FamEqn ([], [], noAnn) name mkHsOuterImplicit
                     (map (HsValArg noExtField) params) Prefix

    -- An adjacent lambda shares the current telescope, so the inner
    -- closure is just a partial application of the same family.
    collectLambdas salt vars body
      | not (hasDuplicates vars)
      , L _ (XHsType (HsTypeLambda _ more inner)) <- stripParens body
      , not (hasDuplicates more)
      = collectLambdas salt (zipWith (hideEarlier more salt) [0..] vars ++ more) inner
      | otherwise = (vars, body)

    stripParens (L _ (HsParTy _ body)) = stripParens body
    stripParens ty = ty

    sameBinder a b = unLoc a == unLoc b
    hasDuplicates = not . null . findDupsEq sameBinder

    hideEarlier binders salt index var
      | any (sameBinder var) binders = privateName "argument" salt index var
      | otherwise = var

    hideCapture binders salt index (header, var)
      | any (sameBinder var) binders = (header, privateName "capture" salt index var)
      | otherwise = (header, var)

    privateName role salt index (L loc _) = L loc (mkRdrUnqual
      (mkTyVarOcc ("$lambda-" ++ role ++ ":" ++ show (salt :: Int)
                              ++ ":" ++ show (index :: Int))))

    capture original@(L loc (HsTvb ann vis@(HsBndrRequired _) var@(HsBndrVar _ n) kind)) =
      Just (capture_binder, n)
      where
        capture_binder
          | HsBndrKind _ written_kind <- kind
          , not (spliceFree written_kind)
          -- The synonym checks the written splice. Infer this generated
          -- parameter from the capture passed by the synonym in the same SCC.
          = L loc (HsTvb ann vis var (HsBndrNoKind noExtField))
          | otherwise = original
    capture _ = Nothing

    typeName n = L (l2l (getLoc n)) (HsTyVar noAnn NotPromoted n)

-- | Form a required parameter for a lifted type expression.
liftedTypeVarBinder :: LocatedN RdrName -> LHsTyVarBndr (HsBndrVis GhcPs) GhcPs
liftedTypeVarBinder v =
  L (l2l (getLoc v)) (HsTvb noAnn (HsBndrRequired noExtField)
    (HsBndrVar noExtField v) (HsBndrNoKind noExtField))

{- Note [Lifting types from signatures and fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Give each lifted expression a private synonym. For a retained sum,
the synonym checks the witness domain and fixes the missing argument
of the payload family. A type case in a value signature uses the
same closed family translation as a case in a written synonym.
The family keeps its lexical owner. See Note [Lifting type lambdas]
and Note [Lifting type cases].

Name the synonym from the value name and the position of the expression in its
signature. Count from left to right in the argument and result types.
Different values use separate counters. The private type namespace also
separates a function operator from a same-spelled type operator. A
signature shared by several values uses its first name.

Within one signature telescope, identical cases can share a synonym.
This keeps two uses equal when their common scrutinee is unknown.
Compare the parsed names, applications and ordered rows. A row binder
has the same scope at both occurrences. Skip comparison for syntax
that can contain a nested binder. Keep the first location for checking
the shared case.

In a value signature, a nested erased forall can also provide captures.
Require written kinds for its local variables and keep the forall at
the original occurrence. Start a new comparison table there. Remove a
shadowed outer name from the capture scope. The capture check still
requires the kind of each selected variable to refer to earlier
captures. If that requires a shadowed variable, leave the case in the
source so the normal context check rejects it.

For a lambda in a top-level value signature, give the lambda expression
its own private synonym. Its parameters are the free variables of the
lambda and the variables in their kinds. Keep the application of the
synonym in the signature. The existing lambda pass then lifts the
synonym body. See Note [Lifting type lambdas].

A foreach telescope starts a new table too. Lift a case on an
application tree with retained variables among its operands. Each
application head can name a type constructor or a checked source function
in scope. See Note [Resolving retained case heads].
The synonym binds an erased index for each retained variable. It uses
the written domain as the kind. The application receives the original
runtime indices. Ordinary source calls in the copied tree use the
existing checked source equations.
See Note [Source retained signatures] in GHC.Tc.Gen.HsType.

Check copied runtime domains for forms whose checking differs between
types and kinds. A bare arrow defaults to unmatchable in a retained
domain and can default to matchable in an erased binder kind. Leave
the corresponding case at the written occurrence.

Two retained cases can use one family when all their captures are
retained indices and their closed domains, application trees, and
ordered rows agree. Compare the index leaves by position in the
telescope. Compare the other names in the same declaration group.
Each application passes its own nominal indices in telescope order.
Retain the source synonym for each signature so diagnostics can name it.

For a constructor field, use the constructor's name and count sums
from left to right. Use a separate prefix for constructor fields.
The constructor name is unique in the module even when record labels
repeat. A Haskell98 field can use the type header's erased variables.
A GADT constructor field uses the constructor signature's outer binders
and any earlier retained arguments on the constructor's argument spine.
Use the same free-variable order as rnConDecl for implicit binders.
Leave the constructor result and any context to ordinary constructor
checking. Leave inner forall groups to the ordinary renamer.

The parser stores the argument spine from the first retained binder in
con_res_ty. Walk that spine in source order. Reuse the normal checking of
the retained application to each lifted synonym. A later binder does not
scope over earlier fields. Stop at shadowing because the copied kind of
an earlier parameter may need a name that the shadowing binder hides.

A GADT field can itself start with a retained telescope. Its binders
scope over that field only. Remove shadowed captures before lifting the
body. Check the remaining written kinds for hidden names. In a group of
retained binders, a binder's kind is checked before that binder enters
scope, so check the kinds in order.

Copy only the parameters used by the lifted expression and the parameters
used in their written kinds. Keep their original order. Require each
free variable to be in scope in the source signature or field. Otherwise
kind inference for the synonym could bind a variable that is out of scope
at the source occurrence.
Skip types that contain splices or holes. A wildcard in a case pattern
is part of the match. Leave nested binders outside these field telescopes
to the ordinary renamer.

For a type case in an ordinary constructor field, use the same
lexical capture check as for a value signature. Give each written
case its own helper, including cases in different fields of one
constructor. A case under a retained binder can inspect that binder,
a closed name, or a checked application to the binder. Keep the original foreach in the field. Apply the
same restriction on copying written retained domains as in value
signatures. For example, a copied arrow can change matchability.

Keep an inner erased forall and put the helper application in its body. Save any shadowed variables that a capture
kind uses with type lets before the forall. Give the saved variables
formal parameters in the helper. Use type lets in the copied kinds
to bind the original names to those parameters. The renamer then
checks the original kind syntax with its own local scopes. Include
the dependencies of the saved variables in telescope order.
-}
-- | Select the expressions that can be lifted at a declaration type position.
data LiftedTypeSite
  = FieldTypeSite
  | RetainedFieldTypeSite [RdrName]
  | SignatureTypeSite
  | ForallTypeSite
  | RetainedTypeSite [RdrName]
  deriving Eq

-- | Rewrite the declarations and return their new private synonyms separately.
-- See Note [Lifting types from signatures and fields].
liftDeclarationTypes :: [(RdrName, Maybe (GlobalRdrElt, Name))]
                     -> [(RetainedCaseKey, RdrName)] -> (Int -> String)
                     -> Bool -> Bool -> HsGroup GhcPs
                     -> (HsGroup GhcPs, [TyClGroup GhcPs], LiftedDecls,
                         [(LocatedN RdrName, GlobalRdrElt)], [LocatedN RdrName],
                         [(RetainedCaseKey, OccName)])
liftDeclarationTypes source_heads earlier helper_suffix lift_cases named_wildcards group
  | not (null synonyms)
  = (group{hs_valds = values, hs_tyclds = type_groups}, [anchors], names, concat uses, missing, helpers)
  | otherwise = (group, [], [], [], [], [])
  where
    (values, signature_types) = case hs_valds group of
      ValBinds ann binds ->
        let (_, lifted) = mapAccumL liftBind emptyOccEnv binds
            (binds', types) = unzip lifted
        in (ValBinds ann binds', concat types)
      binds -> (binds, [])
    (type_groups, groups_sums) = unzip (map liftGroup (hs_tyclds group))
    (seen, shared) = mapAccumL shareRetainedCase earlier signature_types
    helpers = [(key, rdrNameOcc name) | (key, name) <- seen, isUnqual name]
    (shared_types, uses) = unzip shared
    field_types = concat groups_sums
    (synonyms, names) = unzip (shared_types ++ [entry | (entry, _, _) <- field_types])
    missing = concat [unbound | (_, _, unbound) <- field_types]
    anchors = TyClGroup noExtField synonyms [] [] []

    shareRetainedCase seen (entry@(L loc alias@SynDecl{}, names@(occ, _)), Just (key, captures, heads), _)
      | Just first <- lookup key seen
      = let rhs = applyCaseAlias loc first captures
        in (seen, ((L loc alias{tcdRhs = rhs}, names), heads))
      | otherwise
      = ((key, mkRdrUnqual occ) : seen, (entry, []))
    shareRetainedCase seen (entry, _, _) = (seen, (entry, []))

    liftGroup decls =
      let (decls', sums) = unzip (map liftData (group_tyclds decls))
      in (decls{group_tyclds = decls'}, concat sums)

    liftData (L loc decl@DataDecl{tcdTyVars = hsq,
                                 tcdDataDefn = defn@HsDataDefn{dd_cons = cons}}) =
      let binders = hsQTvExplicit hsq
          (kind_occs, _) = hsQTyVarKindOccs binders []
          scope = implicitCaptures kind_occs ++ explicitCaptures binders
          (cons', sums) = case cons of
            DataTypeCons False fields ->
              let (fields', lifted) = unzip (map (liftConstructor scope) fields)
              in (DataTypeCons False fields', concat lifted)
            NewTypeCon con -> first NewTypeCon (liftConstructor scope con)
            _ -> (cons, [])
      in (L loc decl{tcdDataDefn = defn{dd_cons = cons'}}, sums)
    liftData decl = (decl, [])

    liftConstructor scope (L loc con@ConDeclH98{con_telescope = [], con_name = owner, con_args = args}) =
      let (args', sums) = liftFields (liftField (liftFieldAt FieldTypeSite owner scope)) args
      in (L loc con{con_args = args'}, sums)
    liftConstructor _ (L loc con@ConDeclGADT
      { con_names = names, con_outer_bndrs = L _ outer, con_telescope = telescope
      , con_g_args = args, con_res_ty = result })
      | all without_inner_forall telescope
      = let occurrences = extractHsGadtTelescopes telescope $
                          extractConDeclGADTDetailsTyVars args $
                          extractHsTysRdrTyVars [result] []
            owner = head names
            call_sites
              | null source_heads = Set.empty
              | otherwise = Set.fromList
                  [(loc, unLoc v) | v <- retainedFieldCallHeads args result
                                  , RealSrcSpan loc _ <- [getLocA v]
                                  , isJust (lookup (unLoc v) source_heads)
                                  , not (namedWildcard (unLoc v)) ]
            is_call_head v = case getLocA v of
              RealSrcSpan loc _ -> Set.member (loc, unLoc v) call_sites
              _ -> False
            scope = outerCaptures outer (filter (not . is_call_head) occurrences)
            visit = liftField (liftFieldType FieldTypeSite owner scope)
            (args', sums) = case args of
              PrefixConGADT ann fields -> first (PrefixConGADT ann) (liftFieldList visit fields)
              RecConGADT ann fields -> first (RecConGADT ann) (liftRecordFields visit fields)
            (result', retained_types, _) = liftRetainedFields owner scope [] result (length sums)
        in (L loc con{con_g_args = args', con_res_ty = result'}, sums ++ retained_types)
      where
        without_inner_forall (L _ HsGadtForAll{}) = False
        without_inner_forall _ = True
    liftConstructor _ con = (con, [])

    liftRetainedFields owner scope retained ty@(L loc node) salt = case node of
      HsForEachTy{hst_bndrs = binders, hst_body = body}
        | let locals = explicitCaptures binders
              scope' = scope ++ locals
        , length locals == length binders
        , null (findDupsEq (==) (map (unLoc . snd) scope')) ->
            let retained' = map (unLoc . snd) locals ++ retained
                (body', sums, next) = liftRetainedFields owner scope' retained' body salt
            in (L loc node{hst_body = body'}, sums, next)
      HsFunTy ann arrow arg result ->
        let (arg', sums, next) = liftFieldType (RetainedFieldTypeSite retained) owner scope arg salt
            (result', later, end) = liftRetainedFields owner scope retained result next
        in (L loc (HsFunTy ann arrow arg' result'), sums ++ later, end)
      HsParTy ann body ->
        let (body', sums, next) = liftRetainedFields owner scope retained body salt
        in (L loc (HsParTy ann body'), sums, next)
      _ -> (ty, [], salt)

    liftFieldType site owner scope ty@(L loc node) salt = case node of
      HsForEachTy{hst_bndrs = binders, hst_body = body}
        | let locals = explicitCaptures binders
              names = map (unLoc . snd) locals
              (hidden, outer) = partition ((`elem` names) . unLoc . snd) scope
        , length locals == length binders
        , null (findDupsEq (==) names)
        , fieldKindsInScope hidden outer locals ->
            let inner = RetainedFieldTypeSite (names ++ retainedNames site)
                (body', sums, next) = liftFieldType inner owner (outer ++ locals) body salt
            in (L loc node{hst_body = body'}, sums, next)
      HsParTy ann body ->
        let (body', sums, next) = liftFieldType site owner scope body salt
        in (L loc (HsParTy ann body'), sums, next)
      _ -> liftFieldAt site owner scope ty salt

    fieldKindsInScope hidden outer locals =
      all (Set.disjoint shadowed . kindNames . fst) outer
        && localKindsInScope shadowed locals
      where
        shadowed = Set.fromList (map (unLoc . snd) hidden)

    -- Preserve the capture kinds across an erased forall's shadowing.
    rankedFieldScope scope hidden outer locals loc salt
      | fieldKindsInScope hidden outer locals = Just (outer ++ locals, [])
      | otherwise = do
          guard (localKindsInScope hidden_names locals)
          copied <- traverse copy scope
          pure (catMaybes copied ++ locals, saved)
      where
        hidden_names = Set.fromList (map (unLoc . snd) hidden)
        (required, _) = foldr (select []) ([], Set.fromList (map (unLoc . snd) outer)) scope
        needed = [v | (_, v) <- required, unLoc v `Set.member` hidden_names]
        occupied = Set.fromList (map (unLoc . snd) (scope ++ locals))
        fresh = [L (l2l loc) name
                | i <- [0 :: Int ..]
                , let name = mkRdrUnqual (mkTyVarOcc
                             ("$field-kind:" ++ show salt ++ ":" ++ show i))
                , not (Set.member name occupied)]
        saved = zip needed fresh
        saved_names = [(unLoc old, new) | (old, new) <- saved]

        copy (old_binder@(L binder_loc binder@HsTvb{tvb_var = HsBndrVar ann _}), v)
          | Just new <- lookup (unLoc v) saved_names
          = Just (Just (L binder_loc binder{tvb_var = HsBndrVar ann new,
                                           tvb_kind = restoreKind old_binder}, new))
          | unLoc v `Set.member` hidden_names = Just Nothing
          | otherwise = Just (Just (L binder_loc binder{tvb_kind = restoreKind old_binder}, v))
        copy _ = Nothing

        restoreKind binder@(L _ HsTvb{tvb_kind = HsBndrKind ann kind}) =
          HsBndrKind ann (foldr restore kind saved)
          where
            free = kindNames binder
            restore (old, new) body
              | unLoc old `Set.member` free = bindTypeName old new body
              | otherwise = body
        restoreKind (L _ HsTvb{tvb_kind = kind}) = kind

    bindTypeName binder rhs body =
      L (getLoc body) (XHsType (mkHsSingleTypeLet noAnn binder (typeName rhs) body))

    localKindsInScope _ [] = True
    localKindsInScope hidden ((binder, name):locals) =
      Set.disjoint hidden (kindNames binder) && localKindsInScope (Set.delete (unLoc name) hidden) locals

    liftFields visit (PrefixCon ann fields) =
      first (PrefixCon ann) (liftFieldList visit fields)
    liftFields visit (RecCon ann fields) =
      first (RecCon ann) (liftRecordFields visit fields)
    liftFields visit (InfixCon ann left right) =
      let (next, (left', left_sums)) = visit 0 left
          (_, (right', right_sums)) = visit next right
      in (InfixCon ann left' right', left_sums ++ right_sums)
    liftFields _ fields = (fields, [])

    liftRecordFields visit (L loc fields) =
      first (L loc) (liftFieldList visitRecord fields)
      where
        visitRecord salt (L field_loc field@HsConDeclRecField{cdrf_spec = spec}) =
          let (next, (spec', sums)) = visit salt spec
          in (next, (L field_loc field{cdrf_spec = spec'}, sums))
        visitRecord salt field = (salt, (field, []))

    liftFieldList visit fields =
      let (_, lifted) = mapAccumL visit 0 fields
          (fields', sums) = unzip lifted
      in (fields', concat sums)

    liftField visit salt field@CDF{cdf_type = ty} =
      let (ty', sums, next) = visit ty salt
      in (next, (field{cdf_type = ty'}, sums))

    liftBind counters (VbSig (L loc (TypeSig sig_ann mods names@(owner:_) wc@HsWC
                  {hswc_body = L sig_loc sig@HsSig{sig_bndrs = outer, sig_body = body}}))) =
      let occ = rdrNameOcc (unLoc owner)
          first = fromMaybe 0 (lookupOccEnv counters occ)
          (body', aliases, (next, _)) = liftBody SignatureTypeSite owner
                                    (signatureCaptures outer body) body (first, [])
          sig' = L sig_loc (sig{sig_body = body'})
      in (extendOccEnv counters occ next, (VbSig (L loc (TypeSig sig_ann mods names
                                                       wc{hswc_body = sig'})), aliases))
    liftBind counters bind = (counters, (bind, []))

    signatureCaptures outer body = outerCaptures outer
      [v | v <- extractHsTyRdrTyVars body, isNothing (join (lookup (unLoc v) source_heads))]

    outerCaptures HsOuterImplicit{} occurrences = implicitCaptures occurrences
    outerCaptures HsOuterExplicit{hso_bndrs = binders} _ = explicitCaptures binders

    implicitCaptures occurrences =
      [(liftedTypeVarBinder v, v) | v <- nubN occurrences
                                , not (namedWildcard (unLoc v))]

    namedWildcard = (named_wildcards &&) . startsWithUnderscore . rdrNameOcc

    explicitCaptures :: [LHsTyVarBndr flag GhcPs]
                     -> [(LHsTyVarBndr (HsBndrVis GhcPs) GhcPs, LocatedN RdrName)]
    explicitCaptures binders =
      [(fmap (updateHsTyVarBndrFlag (const (HsBndrRequired noExtField))) b, v)
      | b <- binders, Just v <- [hsLTyVarLocName b]]

    liftFieldAt site owner scope ty salt =
      let (ty', sums, (next, _)) = liftBody site owner scope ty (salt, [])
      in (ty', sums, next)

    liftedAnchor SignatureTypeSite _ ty@(L _ (XHsType HsTypeLambda{}))
      | lift_cases, copyable ty = Just ("$signature-lambda:", Nothing)
    liftedAnchor site _ ty@(L _ (XHsType HsTypeSome{}))
      | fieldSite site, copyable ty = Just ("$field-sum:", Nothing)
    liftedAnchor SignatureTypeSite _ ty@(L _ (XHsType HsTypeSome{}))
      | copyable ty = Just ("$signature-sum:", Nothing)
    liftedAnchor site scope (L _ (XHsType (HsTypeCase _ _ scrutinee alts)))
      | lift_cases
      , caseScrutinee site scope scrutinee
      , copyable scrutinee
      , all copyableAlt alts
      = if fieldSite site
          then Just ("$field-case:", Nothing)
          else Just ("$signature-case:", signatureCaseKey scrutinee alts)
    liftedAnchor _ _ _ = Nothing

    fieldSite FieldTypeSite = True
    fieldSite RetainedFieldTypeSite{} = True
    fieldSite _ = False

    caseScrutinee (RetainedFieldTypeSite names) scope scrutinee
      | L _ (HsTyVar _ _ name) <- ignoreParens scrutinee
      , let rdr = unLoc name
      = not (isTvOcc (rdrNameOcc rdr)) || rdr `elem` names
      | otherwise = checkedRetainedScrutinee names True scope scrutinee
    caseScrutinee (RetainedTypeSite names) scope scrutinee =
      checkedRetainedScrutinee names False scope scrutinee
    caseScrutinee _ _ _ = True

    checkedRetainedScrutinee names diagnose scope scrutinee =
      isJust (signatureCasePart scrutinee >>= retainedCaseScrutinee names (resolvePart diagnose scope))

    resolveHead scope name
      | all ((/= name) . unLoc . snd) scope = lookup name source_heads
      | otherwise = Nothing

    resolvePart diagnose scope name = case resolveHead scope name of
      Just (Just (_, table)) -> Just (CasePartSource table)
      Just Nothing | diagnose, not (namedWildcard name) -> Just (CasePartName NotPromoted name)
      _ -> Nothing

    retainedHeadUses site scope (L _ (XHsType (HsTypeCase _ _ scrutinee _)))
      | not (null (retainedNames site)) =
          let heads = [(v, entry) | v <- sourceCallHeads scrutinee
                                  , Just entry <- [resolveHead scope (unLoc v)]]
          in ( [(v, gre) | (v, Just (gre, _)) <- heads]
             , [v | RetainedFieldTypeSite{} <- [site], (v, Nothing) <- heads
                  , not (namedWildcard (unLoc v))] )
    retainedHeadUses _ _ _ = ([], [])

    retainedNames (RetainedTypeSite names) = names
    retainedNames (RetainedFieldTypeSite names) = names
    retainedNames _ = []

    copyableAlt (L _ (HsTypeCaseAlt pat _ rhs)) =
      isJust (classifyTypeCasePattern pat) && copyable rhs

    forallCaptures HsForAllInvis{hsf_invis_bndrs = binders} = writtenCaptures binders
    forallCaptures HsForAllVis{hsf_vis_bndrs = binders} = writtenCaptures binders

    writtenCaptures :: [LHsTyVarBndr flag GhcPs]
                    -> Maybe [(LHsTyVarBndr (HsBndrVis GhcPs) GhcPs, LocatedN RdrName)]
    writtenCaptures binders
      | all hasWrittenKind binders = Just (explicitCaptures binders)
      | otherwise = Nothing

    hasWrittenKind (L _ (HsTvb _ _ HsBndrVar{} (HsBndrKind _ kind))) = copyable kind
    hasWrittenKind _ = False

    innerScope site HsForAllTy{hst_tele = tele, hst_body = body}
      | site == SignatureTypeSite || site == ForallTypeSite
      , Just locals <- forallCaptures tele
      = Just (ForallTypeSite, locals, body)
      | site == FieldTypeSite
      , HsForAllInvis{hsf_matchability = Nothing} <- tele
      , Just locals <- forallCaptures tele
      = Just (FieldTypeSite, locals, body)
    innerScope site HsForEachTy{hst_bndrs = binders, hst_body = body}
      | not (fieldSite site)
      , Just locals <- writtenCaptures binders
      , not (null locals)
      = Just (RetainedTypeSite (map (unLoc . snd) locals ++ retainedNames site), locals, body)
    innerScope _ _ = Nothing

    liftBody site owner scope whole@(L loc _) (salt, seen)
      | Just (prefix, key) <- liftedAnchor site scope whole
      , let exact_retained = filter (isJust . rdrNameExactName_maybe) (retainedNames site)
            free = caseCaptures exact_retained whole
            (heads, unbound) = retainedHeadUses site scope whole
            written = Set.fromList (map (unLoc . fst) heads ++ map unLoc unbound)
            (used, missing) = foldr (select exact_retained) ([], free `Set.difference` written) scope
      , Set.null missing
      , all (copyableCapture site whole) used
      = case key >>= (`lookup` seen) of
          Just occ -> (applyAnchor loc occ used, [], (salt + 1, seen))
          Nothing ->
            let occ = mkTcOcc (prefix ++ occNameString (rdrNameOcc (unLoc owner))
                                 ++ ":" ++ helper_suffix salt)
                name = L (l2l loc) (mkRdrUnqual occ)
                -- The signature applies the helper to the original binders.
                -- Bind fresh type variables in the copy of a quoted case.
                copied = [ (rdr, mkRdrUnqual (mkTyVarOcc ("$quoted-case-capture:" ++ show i)))
                         | (i, (_, v)) <- zip [0 :: Int ..] used
                         , let rdr = unLoc v
                         , isJust (rdrNameExactName_maybe rdr) ]
                header = map (renameCaptures copied . fst) used
                rhs = renameCaptures copied whole
                alias = L loc (SynDecl noAnn name (mkHsQTvs header) Prefix rhs)
                seen' = maybe seen (\k -> (k, occ) : seen) key
                shared = do
                  (share_key, captures, call_uses) <- sharedRetainedCaseKey site scope used whole key heads
                  -- A shared alias also applies the family to its private formals.
                  pure (share_key, renameCaptures copied captures, call_uses)
            in (applyAnchor loc occ used, [((alias, (occ, Nothing)), shared, unbound)], (salt + 1, seen'))
    liftBody site owner scope (L loc quantified) (salt, seen)
      | Just (inner, locals, body) <- innerScope site quantified
      , let names = map (unLoc . snd) locals
      , null (findDupsEq (==) names)
      , let (hidden, outer) = partition ((`elem` names) . unLoc . snd) scope
      , Just (scope', saved) <- if inner == FieldTypeSite
                                then rankedFieldScope scope hidden outer locals loc salt
                                else Just (outer ++ locals, [])
      = let (body', aliases, (next, _)) = liftBody inner owner scope' body (salt, [])
            uses = Set.fromList (map unLoc (extractHsTyRdrTyVars body'))
            save (old, new) ty
              | unLoc new `Set.member` uses = bindTypeName new old ty
              | otherwise = ty
            result = foldr save (L loc quantified{hst_body = body'}) saved
        in (result, aliases, (next, seen))
    liftBody site owner scope (L loc (HsFunTy ann arrow arg result)) state =
      let (arg', arg_aliases, next) = liftBody site owner scope arg state
          (result', result_aliases, end) = liftBody site owner scope result next
      in (L loc (HsFunTy ann arrow arg' result'), arg_aliases ++ result_aliases, end)
    liftBody site owner scope (L loc (HsQualTy ann ctxt body)) state =
      let (body', aliases, next) = liftBody site owner scope body state
      in (L loc (HsQualTy ann ctxt body'), aliases, next)
    liftBody site owner scope (L loc (HsKindSig ann body kind)) state =
      let (body', aliases, next) = liftBody site owner scope body state
      in (L loc (HsKindSig ann body' kind), aliases, next)
    liftBody site owner scope ty state = liftTypeChildren (liftBody site owner scope) ty state

    applyAnchor loc occ = applyCaseAlias loc (mkRdrUnqual occ)
    applyCaseAlias loc name = foldl' (\f (_, v) -> L loc (HsAppTy noExtField f (typeName v)))
                                    (typeName (L (l2l loc) name))

    select exact_retained pair@(binder, name) (captures, used)
      | unLoc name `Set.member` used
      = (pair : captures, Set.delete (unLoc name) used `Set.union` kindNamesWith exact_retained binder)
      | otherwise = (captures, used)

    kindNames binder = kindNamesWith [] binder

    kindNamesWith exact_retained (L _ HsTvb{tvb_kind = HsBndrKind _ kind}) =
      caseCaptures exact_retained kind
    kindNamesWith _ _ = Set.empty

    -- TH IndexVarT supplies an exact term Name. The ordinary type-variable
    -- extractor omits that namespace. Recover only names in this telescope.
    caseCaptures exact_retained ty =
      Set.fromList (map unLoc (extractHsTyRdrTyVars ty) ++ exactUses ty)
      where
        exactUses :: Data a => a -> [RdrName]
        exactUses node
          | null exact_retained = []
          | Just rdr <- cast node = [rdr | rdr `elem` exact_retained]
          | otherwise = concat (gmapQ exactUses node)

    renameCaptures :: Data a => [(RdrName, RdrName)] -> a -> a
    renameCaptures [] node = node
    renameCaptures copied node
      | Just rdr <- cast node
      , Just replacement <- lookup rdr copied
      = fromMaybe node (cast replacement)
      | otherwise = gmapT (renameCaptures copied) node

    copyable :: Data a => a -> Bool
    copyable node = case cast node :: Maybe (LHsType GhcPs) of
      Just (L _ HsSpliceTy{}) -> False
      Just (L _ HsWildCardTy{}) -> False
      _ -> and (gmapQ copyable node)

    copyableCapture site (L _ ty) (binder, name) =
      copyable binder && case ty of
        XHsType HsTypeCase{}
          | unLoc name `elem` retainedNames site -> hasWrittenKind binder && copyableDomain binder
        _ -> True

    copyableDomain :: Data a => a -> Bool
    copyableDomain node = case cast node :: Maybe (LHsType GhcPs) of
      Just (L _ HsFunTy{}) -> False
      Just (L _ HsOpTy{}) -> False
      Just (L _ HsForAllTy{}) -> False
      Just (L _ HsForEachTy{}) -> False
      _ -> and (gmapQ copyableDomain node)

    sharedRetainedCaseKey (RetainedTypeSite names) scope used
      (L _ (XHsType (HsTypeCase _ _ _ alts))) (Just (scrutinee, rows)) heads
      | let indices = map (unLoc . snd) used
      , all (`elem` names) indices
      , Just domains <- traverse (closedDomain . fst) used
      , Just scrutinee_key <- retainedCaseScrutinee indices (resolvePart False scope) scrutinee
      , all closedRow alts
      = Just ((domains, scrutinee_key, rows), used, heads)
    sharedRetainedCaseKey _ _ _ _ _ _ = Nothing

    closedDomain (L _ HsTvb{tvb_kind = HsBndrKind _ domain})
      | null (extractHsTyRdrTyVars domain) = signatureCasePart domain
    closedDomain _ = Nothing

    closedRow (L _ (HsTypeCaseAlt pat _ rhs)) =
      case classifyTypeCasePattern pat of
        Just (_, []) -> null (extractHsTyRdrTyVars rhs)
        _ -> False

    typeName n = L (l2l (getLoc n)) (HsTyVar noAnn NotPromoted n)

-- | The case syntax compared within a single signature.
data SignatureCasePart
  = CasePartName PromotionFlag RdrName
  | CasePartApp SignatureCasePart SignatureCasePart
  | CasePartCapture Int
  | CasePartSource Name
  | CasePartWildcard
  deriving Eq

-- | Admit applications of global names and normalize retained operands.
retainedCaseScrutinee :: [RdrName] -> (RdrName -> Maybe SignatureCasePart)
                      -> SignatureCasePart -> Maybe SignatureCasePart
retainedCaseScrutinee names resolve scrutinee = do
  checked <- operand scrutinee
  guard (hasCapture checked)
  pure checked
  where
    operand (CasePartName NotPromoted name)
      | Just i <- elemIndex name names = Just (CasePartCapture i)
    operand (CasePartApp fun arg) = CasePartApp <$> application fun <*> operand arg
    operand name = global name

    application (CasePartApp fun arg) = CasePartApp <$> application fun <*> operand arg
    application (CasePartName NotPromoted name)
      | Just part <- resolve name = Just part
    application name = global name

    global part@(CasePartName _ name)
      | isQual name || isRdrTc name || isRdrDataCon name = Just part
    global _ = Nothing

    hasCapture CasePartCapture{} = True
    hasCapture (CasePartApp fun arg) = hasCapture fun || hasCapture arg
    hasCapture _ = False

-- | Locate the written heads of applications in a case scrutinee.
sourceCallHeads :: LHsType GhcPs -> [LocatedN RdrName]
sourceCallHeads = spine []
  where
    spine args ty = case unLoc (ignoreParens ty) of
      HsAppTy _ fun arg -> spine (arg : args) fun
      HsTyVar _ NotPromoted name | not (null args) -> name : concatMap sourceCallHeads args
      _ -> []

-- | Find a checked source family or an absent unqualified field head.
retainedSourceHeads :: [LSig GhcPs] -> [TyClGroup GhcPs]
                    -> RnM [(RdrName, Maybe (GlobalRdrElt, Name))]
retainedSourceHeads sigs type_groups = do
  envs@(global, local) <- getRdrEnvs
  let signature_heads = Set.fromList
        [unLoc v | L _ (TypeSig _ _ _ sig) <- sigs, v <- caseCallHeads sig]
      field_heads = Set.fromList
        [unLoc v | grp <- type_groups
                 , L _ DataDecl{tcdDataDefn = HsDataDefn{dd_cons = cs}} <- group_tyclds grp
                 , L _ ConDeclGADT{con_g_args = args, con_res_ty = res} <- toList cs
                 , v <- retainedFieldCallHeads args res]
  fmap catMaybes $ forM (Set.toList (Set.union signature_heads field_heads)) $ \rdr ->
    case demoteRdrNameTv rdr of
      Just term
        | not (elemLocalRdrEnv rdr local || elemLocalRdrEnv term local)
        , null (lookupGRE global (LookupRdrName rdr (RelevantGREsFOS WantNormal)))
        -> do
          -- Use the same lookup as ordinary calls. Keep an existing name
          -- without a checked table separate from an absent name.
          candidates <- lookupSourceTables False envs term
          pure $ case candidates of
            [(gre, Just table)] -> Just (rdr, Just (gre, table))
            [] | isUnqual rdr, Set.member rdr field_heads -> Just (rdr, Nothing)
            _ -> Nothing
      _ -> pure Nothing

-- | Inspect case expressions only in constructor types with a retained binder.
retainedFieldCallHeads :: HsConDeclGADTDetails GhcPs -> LHsType GhcPs -> [LocatedN RdrName]
retainedFieldCallHeads args result = concatMap heads (result : fields)
  where
    heads ty | hsHasForEachTy (unLoc ty) = caseCallHeads ty
             | otherwise = []
    fields = case args of
      PrefixConGADT _ xs -> map cdf_type xs
      RecConGADT _ xs -> [cdf_type (cdrf_spec field) | L _ field <- unLoc xs]

-- | Collect the written application heads in type case scrutinees.
caseCallHeads :: Data a => a -> [LocatedN RdrName]
caseCallHeads node = here ++ concat (gmapQ caseCallHeads node)
  where
    here = case cast node :: Maybe (LHsType GhcPs) of
      Just (L _ (XHsType (HsTypeCase _ _ scrutinee _))) -> sourceCallHeads scrutinee
      _ -> []

-- | The scrutinee followed by the pattern and result in each source row.
type SignatureCaseKey = (SignatureCasePart, [(SignatureCasePart, SignatureCasePart)])

-- | Compare capture domains, the scrutinee and the ordered rows.
type RetainedCaseKey = ([SignatureCasePart], SignatureCasePart,
                        [(SignatureCasePart, SignatureCasePart)])

-- | Only reuse a retained capture case across splice expansions when its
-- names have fixed identities. See Note [Lifting cases from signature splices].
stableSplicedCase :: RetainedCaseKey -> Bool
stableSplicedCase (domains, CasePartCapture _, rows) =
  all stable domains && all (\(pat, rhs) -> stable pat && stable rhs) rows
  where
    stable (CasePartName _ name) =
      isJust (isOrig_maybe name) || maybe False isExternalName (rdrNameExactName_maybe name)
    stable (CasePartApp fun arg) = stable fun && stable arg
    stable CasePartWildcard = True
    stable _ = False
stableSplicedCase _ = False

-- | Compare spellings and branch order, ignoring source locations and parentheses.
signatureCaseKey :: LHsType GhcPs -> [LocatedA HsTypeCaseAlt] -> Maybe SignatureCaseKey
signatureCaseKey scrutinee alts = (,) <$> signatureCasePart scrutinee <*> traverse alternative alts
  where
    alternative (L _ (HsTypeCaseAlt pat _ rhs)) =
      (,) <$> signatureCasePart pat <*> signatureCasePart rhs

-- | Compare parsed names and applications in a signature case or closed domain.
signatureCasePart :: LHsType GhcPs -> Maybe SignatureCasePart
signatureCasePart (L _ (HsTyVar _ promotion name)) = Just (CasePartName promotion (unLoc name))
signatureCasePart (L _ (HsAppTy _ f x)) = CasePartApp <$> signatureCasePart f <*> signatureCasePart x
signatureCasePart (L _ (HsParTy _ ty)) = signatureCasePart ty
signatureCasePart (L _ HsWildCardTy{}) = Just CasePartWildcard
signatureCasePart _ = Nothing

{- Note [Lifting type cases]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Use a checked closed family to match a type case. Each source branch
becomes one equation. A constructor pattern tests the scrutinee. A
variable or wildcard accepts it when the earlier tests permit that row.
See Note [Publishing ordered type cases] in GHC.Tc.TyCl. Keep lexical captures as
additional arguments. Hide a captured argument's name in any branch
where a pattern binds that spelling. Put the family and its owning
synonym in the same kind-checking SCC.

An untyped declaration quote keeps the case in its source signature,
synonym or field. The quote emits CaseT with its local names. Lift the
case when the declarations enter the splice's checking group.

For a case in a let body, put the RHS in one private type synonym.
The original let and the branch rows use that checked synonym at the
corresponding lexical captures. Keep earlier captures needed by the
kinds of those variables. Each row uses its own capture parameters,
even if a pattern binds a variable with the same source name. Give
cases in the scrutinee the same alias environment. A new let extends
it only after checking its RHS in the original lexical scope. The
helper fragment accepts references to the synonym's parameters.
An inner let can refer to an earlier let through its existing helper.
Copy the earlier helper's capture arguments into the inner helper's
telescope. Use the nearest alias for each written name. A restored
alias can shadow a capture that another alias needs, so save that
capture under a fresh name before restoring the aliases.
An inferred kind parameter must be determined by a checked capture.
For an infix RHS, a backticked operator can itself be a bound type
variable. Keep the parsed application so the renamer still resolves
the operator's fixity in the original scope. A captured operator can have a
function kind. Include variables from arrow modifiers when collecting
the captures of that kind or of a function RHS.
See Note [Checked RHSs for cases in type lets] in GHC.Tc.TyCl.

Both sides of a function arrow have the arrow's lexical scope.
Lift each side with the same captures. Keep the original arrow and
its modifiers for the arrow checker.

Keep an invisible forall in place and copy its binders, in order, to
the family header. The application uses the original variables inside
the forall, so their skolems stay in scope.

A dependent binder needs the generalized family kind when checking the
owner's application. Use the actual declaration dependencies for that
family. If there is no cycle, normal kind checking generalizes the family
before the owner uses it. A real cycle keeps the usual restrictions on
polymorphic recursion. Copy the written kind of the scrutinee from its
binder. A kind annotation on the case also gives the family its result
kind. See Note [Result kinds of dependent type cases] in GHC.Tc.TyCl.

An ordinary case can use a written result kind, including a kind
signature on a let RHS. Copy that kind to the family so its equations
check at the written kind. Use the actual declaration dependencies
so a result kind with a captured variable can generalize before use.
The kind can use earlier let bindings. Restore those aliases using
the same capture substitution as an ordinary let RHS. The family
header needs the preceding helpers before it checks the equations.
Use the actual declaration dependencies for this annotated let chain.
An unannotated case keeps the shared kind inference of its owner.

A named scrutinee can also depend on an earlier binder of the synonym.
Generalize the family before checking the synonym when the captures
form a telescope. Its initial monomorphic kind would leave the capture
variables free. For `(b :: Bool) (d :: Gate (Index b))`, checking
the application would then require equality of family applications at
different `b` variables. The generalized family substitutes the
supplied `b`. Infer the scrutinee kind from the rows as before.
This lets the patterns match at an abstract index and keeps `Index b`
out of their constructor arguments.
See Note [No polymorphic recursion in type decls] in GHC.Tc.TyCl.

Stop if a binder shadows a capture, including a kind variable in its
header. The name in the copied header must keep its original meaning.
Stop in an active case-let alias environment. The alias bindings belong
to the outer case and can shadow names in the copied telescope.
-}
-- | Private names with optional edges back to their kind-checking owners.
type LiftedDecls = [(OccName, Maybe OccName)]

liftTypeCases :: HsGroup GhcPs -> (HsGroup GhcPs, LiftedDecls)
liftTypeCases group =
  let (groups, hidden) = unzip (map liftGroup (hs_tyclds group))
  in (group { hs_tyclds = groups }, concat hidden)
  where
    liftGroup decls =
      let (defs, lifted) = unzip (map liftDecl (group_tyclds decls))
          (families, hidden) = unzip (concat lifted)
      in (decls { group_tyclds = defs ++ families }, hidden)

    liftDecl (L loc syn@SynDecl{tcdLName = owner, tcdRhs = rhs,
                                tcdTyVars = HsQTvs{hsq_explicit = params}})
      | Just captures <- traverse capture params
      = let (rhs', cases, _) = liftRhs owner False captures [] rhs (0 :: Int)
        in (L loc syn{tcdRhs = rhs'}, cases)
    liftDecl decl = (decl, [])

    liftRhs owner independent captures aliases (L loc (XHsType (HsTypeCase _ _ scrutinee alts))) salt
      | Just patterns <- traverse checkAlt alts
      , Just scrut_kind <- caseScrutineeKind independent captures scrutinee
      = liftCase owner independent captures aliases loc (scrutinee, scrut_kind) Nothing patterns salt
    liftRhs owner independent captures aliases (L loc (HsKindSig ann body kind)) salt
      | L case_loc (XHsType (HsTypeCase _ _ scrutinee alts)) <- ignoreParens body
      , Just patterns <- traverse checkAlt alts
      , Just scrut_kind <- caseScrutineeKind independent captures scrutinee
      , Just result_kind <- caseResultKind captures aliases kind
      = let (body', cases, next) = liftCase owner independent captures aliases case_loc
                                   (scrutinee, scrut_kind) (Just result_kind) patterns salt
        in (L loc (HsKindSig ann body' kind), cases, next)
    liftRhs owner independent captures aliases
      (L loc (XHsType let_ty@(HsTypeLet ann _ _ binds result_ty))) salt
      | not independent
      , let (binder, rhs, body) = splitHsTypeLet binds result_ty
      , Just annotated <- letCaseBody body
      , Just (used, references) <- rhsCaptures captures aliases rhs
      , let owner_occ = rdrNameOcc (unLoc owner)
            occ = mkTcOcc ("$case-let-rhs:" ++ occNameString owner_occ ++ ":" ++ show salt)
            name = L (l2l (getLoc rhs)) (mkRdrUnqual occ)
            helper_rhs = restoreRhs ann used references rhs
            helper = L loc (SynDecl noAnn name
                       (HsQTvs noExtField [header | (_, (header, _, _)) <- used]) Prefix helper_rhs)
            rhs_ref = applyType loc name [typeName v | (_, (_, _, v)) <- used]
            saved = L (getLoc binder) (mkRdrUnqual (mkTyVarOcc ("$case-let-outer:" ++ show salt)))
            shadowed (_, _, current) = unLoc current == unLoc binder
            hide capture@(header, v, _)
              | shadowed capture = (header, v, saved)
              | otherwise = capture
            branch_binding = (ann, binder, name, map fst used)
            (body', generated, next) = liftRhs owner False (map hide captures)
                (aliases ++ [branch_binding]) body (salt + 1)
      , not (null generated)
      = let result = letTy loc ann binder rhs_ref body'
            result'
              | any shadowed captures = letTy loc ann saved (typeName binder) result
              | otherwise = result
            helper_owner = if annotated then Nothing else Just owner_occ
        in (preserveTypeLetSource let_ty result', (helper, (occ, helper_owner)) : generated, next)
    liftRhs owner False captures aliases (L loc (XHsType let_ty@(HsTypeLet ann _ _ binds result_ty))) salt =
      let (v, rhs, body) = splitHsTypeLet binds result_ty
          (rhs', cases, next) = liftRhs owner False captures aliases rhs salt
      in (preserveTypeLetSource let_ty (letTy loc ann v rhs' body), cases, next)
    liftRhs owner independent captures aliases (L loc (HsFunTy ann arrow domain result)) salt =
      let (domain', domain_cases, next) = liftRhs owner independent captures aliases domain salt
          (result', result_cases, end) = liftRhs owner independent captures aliases result next
      in (L loc (HsFunTy ann arrow domain' result'), domain_cases ++ result_cases, end)
    liftRhs owner independent captures aliases
      (L loc quantified@HsForAllTy
        { hst_tele = HsForAllInvis { hsf_matchability = Nothing, hsf_invis_bndrs = binders }
        , hst_body = body }) salt
      | null aliases
      , Just locals <- traverse (capture . fmap (updateHsTyVarBndrFlag (const (HsBndrRequired noExtField)))) binders
      , let (implicit, _) = hsQTyVarKindOccs [h | (h, _, _) <- captures] []
            kind_scope = [unLoc name | (_, name, _) <- captures]
            mentions = concatMap (fromMaybe [] . captureKindNames) locals
      , all (`notElem` map unLoc implicit) [unLoc name | (_, name, _) <- locals]
      , all (`elem` mentions) [unLoc name | (L _ HsTvb{tvb_kind = HsBndrNoKind{}}, name, _) <- locals]
      , Just _ <- foldM extendForall kind_scope locals
      = let separate = independent || not (null mentions)
            (body', cases, next) = liftRhs owner separate (captures ++ locals) aliases body salt
        in (L loc quantified{hst_body = body'}, cases, next)
    liftRhs owner independent captures aliases ty salt =
      liftTypeChildren (liftRhs owner independent captures aliases) ty salt

    liftCase owner independent captures aliases loc (scrutinee, scrut_kind) result_kind patterns salt =
      let owner_occ = rdrNameOcc (unLoc owner)
          suffix = if independent then "-forall" else ""
          occ = mkTcOcc ("$case:" ++ occNameString owner_occ ++ ":" ++ show salt ++ suffix)
          name = L (getLoc owner) (mkRdrUnqual occ)
          scrut_var = L (getLoc owner) (mkRdrUnqual (mkTyVarOcc "$scrutinee"))
          scrut_binder = L loc (HsTvb noAnn (HsBndrRequired noExtField)
                         (HsBndrVar noExtField scrut_var) scrut_kind)
          eqns = map (branch owner name captures aliases) patterns
          fam = L loc (FamDecl noExtField
                  (FamilyDecl noAnn (ClosedTypeFamily noAnn (Just eqns)) TopLevel
                     name (HsQTvs noExtField ([h | (h, _, _) <- captures] ++ [scrut_binder]))
                     Prefix (L (l2l loc) (maybe (NoSig noExtField) (KindSig noExtField) result_kind)) Nothing))
          (scrutinee', nested, next) = liftRhs owner independent captures aliases scrutinee (salt + 1)
          result = applyType loc name ([typeName v | (_, _, v) <- captures] ++ [scrutinee'])
          separate = independent || isJust result_kind ||
                     (null aliases && dependentScrutinee captures scrutinee)
      in (result, (fam, (occ, if separate then Nothing else Just owner_occ)) : nested, next)

    dependentScrutinee captures scrutinee
      | Just (HsBndrKind _ kind) <- caseScrutineeKind True captures scrutinee
      , Just (_:_) <- lexicalNames kind
      , Just _ <- foldM extendForall [] captures
      = True
    dependentScrutinee _ _ = False

    caseScrutineeKind False _ _ = Just (HsBndrNoKind noExtField)
    caseScrutineeKind True captures scrutinee
      | L _ (HsTyVar _ NotPromoted name) <- ignoreParens scrutinee
      , [kind] <- [kind | (L _ HsTvb{tvb_kind = HsBndrKind _ kind}, _, bound) <- captures
                        , unLoc name == unLoc bound]
      = Just (HsBndrKind noExtField kind)
    caseScrutineeKind _ _ _ = Nothing

    letTy loc ann v rhs body = L loc (XHsType (mkHsSingleTypeLet ann v rhs body))
    applyType loc name = foldl' (\f x -> L loc (HsAppTy noExtField f x)) (typeName name)

    -- Follow an ordered chain to a case. A written result kind needs
    -- the preceding helpers before checking the family's header.
    letCaseBody ty
      | orderedCase ty = Just False
      | annotatedCase ty = Just True
      | L _ (XHsType (HsTypeLet _ _ _ binds result_ty)) <- ignoreParens ty
      = let (_, rhs, body) = splitHsTypeLet binds result_ty
        in if annotatedCase rhs
           then Just True
           else lexicalNames rhs >> letCaseBody body
      | otherwise = Nothing

    orderedCase ty
      | L _ (XHsType (HsTypeCase _ _ _ alts)) <- ignoreParens ty
      = all (isJust . checkAlt) alts
      | otherwise = False

    annotatedCase ty
      | L _ (HsKindSig _ body _) <- ignoreParens ty = orderedCase body
      | otherwise = False

    caseResultKind captures [] kind = do
      mentions <- lexicalNames kind
      guard (all (`elem` [unLoc name | (_, name, _) <- captures]) mentions)
      pure kind
    caseResultKind captures aliases kind = do
      (used, references) <- rhsCaptures captures aliases kind
      pure (restoreRhs noAnn used references kind)

    -- Include the captures of every referenced alias, and their kinds.
    rhsCaptures captures aliases rhs = do
      names <- lexicalNames rhs
      let named alias (_, v, _, _) = alias == unLoc v
          resolve n = find (named n) (reverse aliases)
          references = mapMaybe resolve (nub names)
          transitive = concat [indices | (_, _, _, indices) <- references]
          direct = [n | n <- names, not (any (named n) references)]
          numbered = zip [0 :: Int ..] captures
          names' = direct ++ [unLoc v | (i, (_, v, _)) <- numbered, i `elem` transitive]
      needed <- foldr captureKind (Just names') captures
      let available = [unLoc n | (_, n, _) <- captures]
      guard (all (`elem` available) needed)
      pure ([pair | pair@(_, (_, v, _)) <- numbered, unLoc v `elem` needed], references)

    -- Keep capture variables visible to later alias applications.
    restoreRhs ann used references rhs = foldr save restored arguments
      where
        loc = getLoc rhs
        alias_names = [unLoc v | (_, v, _, _) <- references]
        arguments = [(i, v, argument i v) | (i, (_, v, _)) <- used]
        argument i v
          | unLoc v `elem` alias_names =
              L (getLoc v) (mkRdrUnqual (mkTyVarOcc ("$case-let-argument:" ++ show i)))
          | otherwise = v
        save (_, original, saved) body
          | unLoc original /= unLoc saved = letTy loc ann saved (typeName original) body
          | otherwise = body
        restored = foldr restore rhs references
        restore (alias_ann, binder, helper, indices) =
          letTy loc alias_ann binder
            (applyType loc helper [typeName v | (i, _, v) <- arguments, i `elem` indices])

    captureKind (L _ HsTvb{tvb_kind = HsBndrKind _ kind}, v, _) inside = do
      names <- inside
      if unLoc v `elem` names then (names ++) <$> lexicalNames kind else pure names
    captureKind _ inside = inside

    lexicalNames (L _ (HsTyVar _ _ n))
      | isTvOcc (rdrNameOcc (unLoc n)) = Just [unLoc n]
      | otherwise = Just []
    lexicalNames (L _ (HsAppTy _ f x)) = (++) <$> lexicalNames f <*> lexicalNames x
    lexicalNames (L _ (HsOpTy _ left op right)) = concat <$> traverse lexicalNames [left, op, right]
    lexicalNames (L _ (HsFunTy _ (HsModifiedFunArr _ mods _) domain result)) =
      concat <$> traverse lexicalNames (domain : [ty | L _ (HsModifier _ ty) <- mods] ++ [result])
    lexicalNames (L _ HsStarTy{}) = Just []
    lexicalNames (L _ (HsParTy _ ty)) = lexicalNames ty
    lexicalNames (L _ (HsExplicitListTy _ _ tys)) = concat <$> traverse lexicalNames tys
    lexicalNames (L _ (HsTupleTy _ HsBoxedOrConstraintTuple tys)) = concat <$> traverse lexicalNames tys
    lexicalNames (L _ (HsExplicitTupleTy _ _ tys)) = concat <$> traverse lexicalNames tys
    lexicalNames (L _ (HsKindSig _ ty kind)) = (++) <$> lexicalNames ty <*> lexicalNames kind
    lexicalNames (L _ HsTyLit{}) = Just []
    lexicalNames _ = Nothing

    capture b@(L _ (HsTvb _ (HsBndrRequired _) (HsBndrVar _ n) _)) = Just (b, n, n)
    capture _ = Nothing

    captureKindNames (L _ HsTvb{tvb_kind = HsBndrKind _ kind}, _, _) = lexicalNames kind
    captureKindNames _ = Just []

    extendForall names binder@(_, name, _)
      | unLoc name `notElem` names
      , Just mentions <- captureKindNames binder
      , all (`elem` names) mentions
      = Just (unLoc name : names)
    extendForall _ _ = Nothing

    typeName n = L (l2l (getLoc n)) (HsTyVar noAnn NotPromoted n)

    checkAlt (L loc (HsTypeCaseAlt pat _ rhs)) = do
      (pat', bound) <- classifyTypeCasePattern pat
      pure (loc, pat', bound, rhs)

    branch owner name captures aliases (loc, pat, bound, rhs) =
      let alias_names = [unLoc v | (_, v, _, _) <- aliases]
          pat_capture index (_, n@(L name_loc var), _)
            | var `elem` bound || var `elem` alias_names =
                typeName (L name_loc (mkRdrUnqual (mkTyVarOcc ("$case-capture:" ++ show index))))
            | otherwise = typeName n
          captured = zipWith pat_capture [0 :: Int ..] captures
          pats = captured ++ [pat]
          branch_rhs = foldr restore rhs aliases
          body = L (getLoc branch_rhs) (XHsType (HsTypeCaseAlias [] (owner, length captures, Nothing) branch_rhs))
          restore (ann, binder, helper, indices) rest
            | unLoc binder `notElem` bound =
                letTy (getLoc rhs) ann binder
                  (applyType (getLoc rhs) helper
                    [v | (i, v) <- zip [0 :: Int ..] captured, i `elem` indices]) rest
            | otherwise = rest
      in L loc (FamEqn ([], [], noAnn) name mkHsOuterImplicit
                       (map (HsValArg noExtField) pats) Prefix body)

-- | Visit type expression children that share their enclosing lexical scope.
-- The caller handles binders and kind annotations. Thread its state
-- through the children in source order.
liftTypeChildren :: (LHsType GhcPs -> s -> (LHsType GhcPs, [a], s))
                 -> LHsType GhcPs -> s -> (LHsType GhcPs, [a], s)
liftTypeChildren visit ty@(L loc node) salt = case node of
  HsParTy ann body -> one (HsParTy ann) body
  HsAppTy ann f x -> two (HsAppTy ann) f x
  HsAppKindTy ann f kind -> one (\f' -> HsAppKindTy ann f' kind) f
  HsOpTy ann l op r -> two (\l' r' -> HsOpTy ann l' op r') l r
  HsListTy ann t -> one (HsListTy ann) t
  HsTupleTy ann sort ts -> many (HsTupleTy ann sort) ts
  HsExplicitListTy ann promoted ts -> many (HsExplicitListTy ann promoted) ts
  HsExplicitTupleTy ann promoted ts -> many (HsExplicitTupleTy ann promoted) ts
  _ -> (ty, [], salt)
  where
    one rebuild t =
      let (t', lifted, next) = visit t salt
      in (L loc (rebuild t'), lifted, next)

    two rebuild l r =
      let (l', left, next) = visit l salt
          (r', right, end) = visit r next
      in (L loc (rebuild l' r'), left ++ right, end)

    many rebuild ts =
      let step next t = case visit t next of
            (t', lifted, end) -> (end, (t', lifted))
          (end, results) = mapAccumL step salt ts
          (ts', lifted) = unzip results
      in (L loc (rebuild ts'), concat lifted, end)

{- | @rnSourceDecl@ "renames" declarations.
It simultaneously performs dependency analysis and precedence parsing.
It also does the following error checks:

* Checks that tyvars are used properly. This includes checking
  for undefined tyvars, and tyvars in contexts that are ambiguous.
  (Some of this checking has now been moved to module @TcMonoType@,
  since we don't have functional dependency information at this point.)

* Checks that all variable occurrences are defined.

* Checks the @(..)@ etc constraints in the export list.

Brings the binders of the group into scope in the appropriate places;
does NOT assume that anything is in scope already
-}
rnSrcDecls :: HsGroup GhcPs -> RnM (TcGblEnv, HsGroup GhcRn)
-- Rename a top-level HsGroup; used for normal source files *and* hs-boot files
rnSrcDecls group = do
  enabled <- xoptM LangExt.ExperimentalDependentTypes
  named_wildcards <- xoptM LangExt.NamedWildCards
  level <- getThLevel
  dflags <- getDynFlags
  let lift_cases = case level of
        UntypedBrack{} -> False
        _ -> enabled
      sigma_mod = mkModule (RealUnit (Definite (baseUnitId dflags))) (mkModuleName "GHC.Sigma")
      sigma = mkOrig sigma_mod (mkTcOcc "Sigma")
      (with_cases, case_names) = if lift_cases then liftTypeCases group else (group, [])
  (expanded, lambda_names, scopes) <-
    if enabled then liftTypeLambdas sigma with_cases else return (group, [], emptyOccEnv)
  rnSrcDeclsWithLambdas sigma named_wildcards lift_cases
                        (case_names ++ [(fam, Just owner) | (fam, owner) <- lambda_names])
                        scopes expanded

-- | Rename the declarations and record the kind-inference edges of private families.
rnSrcDeclsWithLambdas :: RdrName -> Bool -> Bool -> LiftedDecls -> OccEnv (OccName, Bool) -> HsGroup GhcPs
                      -> RnM (TcGblEnv, HsGroup GhcRn)
rnSrcDeclsWithLambdas sigma named_wildcards lift_cases early_hidden early_scopes group@(HsGroup { hs_valds   = val_decls,
                            hs_splcds  = splice_decls,
                            hs_tyclds  = tycl_decls,
                            hs_derivds = deriv_decls,
                            hs_fixds   = fix_decls,
                            hs_warnds  = warn_decls,
                            hs_annds   = ann_decls,
                            hs_fords   = foreign_decls,
                            hs_defds   = default_decls,
                            hs_ruleds  = rule_decls,
                            hs_docs    = docs })
 = do {
   -- (A) Process the top-level fixity declarations, creating a mapping from
   --     FastStrings to FixItems. Also checks for duplicates.
   --     See Note [Top-level fixity signatures in an HsGroup] in GHC.Hs.Decls
   local_fix_env <- makeMiniFixityEnv $ hsGroupTopLevelFixitySigs group ;

   -- See Note [Pun binding warnings] in GHC.Rename.Utils.
   warn_puns <- woptM Opt_WarnPunBindings ;
   earlier_pun_scope <- if warn_puns
     then do { level <- getThLevel
             ; interactive <- getIsGHCi
             ; if interactive || isBrackLevel level
               then pure Nothing
               else Just <$> getGlobalRdrEnv }
     else pure Nothing ;

   -- (B) Bring top level binders (and their fixities) into scope,
   --     *except* for the value bindings, which get done in step (D)
   --     with collectHsIdBinders. However *do* include
   --
   --        * Class ops, data constructors, and record fields,
   --          because they do not have value declarations.
   --
   --        * For hs-boot files, include the value signatures
   --          Again, they have no value declarations
   --
   (tc_envs, tc_bndrs) <- getLocalNonValBinders local_fix_env group ;


   restoreEnvs tc_envs $ do {

   failIfErrsM ; -- No point in continuing if (say) we have duplicate declarations

   -- (D1) Bring pattern synonyms into scope.
   --      Need to do this before (D2) because rnTopBindsLHS
   --      looks up those pattern synonyms (#9889)

   dup_fields_ok <- xopt_DuplicateRecordFields <$> getDynFlags ;
   has_sel <- xopt_FieldSelectors <$> getDynFlags ;
   extendPatSynEnv dup_fields_ok has_sel val_decls local_fix_env $ \pat_syn_bndrs -> do {

   -- (D2) Rename the left-hand sides of the value bindings.
   --     This depends on everything from (B) being in scope.
   --     It uses the fixity env from (A) to bind fixities for view patterns.

   -- We need to throw an error on such value bindings when in a boot file.
   is_boot <- tcIsHsBootOrSig ;
   (binds', sigs') <- if is_boot
    then rnTopBindsLHSBoot local_fix_env val_decls
    else rnTopBindsLHS     local_fix_env val_decls ;

   -- Bind the LHSes (and their fixities) in the global rdr environment
   let { id_bndrs = collectHsIdBinders' CollNoDictBinders binds' } ;
                    -- Excludes pattern-synonym binders
                    -- They are already in scope
   traceRn "rnSrcDecls" (ppr id_bndrs) ;
   (gbl_env, lcl_env) <- extendGlobalRdrEnvRn (map (mkLocalVanillaGRE NoParent) id_bndrs) local_fix_env ;
   warnPunBindingsAgainst earlier_pun_scope
     (nonDetEltsUniqSet (tc_bndrs `unionNameSet` mkNameSet (pat_syn_bndrs ++ id_bndrs))) ;
   -- See Note [Ordering local source tables].
   dependent <- xoptM LangExt.ExperimentalDependentTypes ;
   tables <- if dependent then localSourceTableBinders binds' sigs' else pure [] ;
   let { table_names = map snd tables
       ; rdr_env = foldl' extendGlobalRdrEnv (tcg_rdr_env gbl_env)
                     (map (mkLocalVanillaGRE NoParent) table_names) } ;
   restoreEnvs (gbl_env { tcg_rdr_env = rdr_env }, lcl_env) $ do {

   --  Now everything is in scope, as the remaining renaming assumes.
   --  See Note [Resolving retained case heads].
   (sig_types, updated_types, sigs, hidden, scopes) <-
     if dependent && not is_boot
     then do { heads <- if lift_cases then retainedSourceHeads sigs' tycl_decls else pure []
             ; let input = emptyRdrGroup{hs_valds = val_decls, hs_tyclds = tycl_decls}
                   (updated, anchors, aliases, uses, unbound, _) = liftDeclarationTypes heads [] show lift_cases named_wildcards input
                   (cases, case_names) = liftTypeCases emptyRdrGroup{hs_tyclds = anchors}
             ; (late, lambdas, late_scopes) <- liftTypeLambdas sigma cases
             ; let late_hidden = aliases ++ case_names ++ [(fam, Just owner) | (fam, owner) <- lambdas]
             ; forM_ unbound $ \v ->
                 forM_ (demoteRdrNameTv (unLoc v)) $ \term ->
                   setSrcSpanA (getLoc v) (void (reportUnboundName WL_Anything term))
             ; unless (null unbound) failIfErrsM
             ; forM_ uses $ \(loc, gre) -> setSrcSpanA (getLoc loc) (addUsedGRE AllDeprecationWarnings gre)
             ; return (hs_tyclds late, hs_tyclds updated, case hs_valds updated of
                                         ValBinds _ entries -> val_sigs entries
                                         _ -> sigs',
                       early_hidden ++ late_hidden, plusOccEnv early_scopes late_scopes) }
     else pure ([], tycl_decls, sigs', early_hidden, early_scopes) ;
   (sig_envs, sig_bndrs) <- if null sig_types
     then (, emptyNameSet) <$> getEnvs
     else getLocalNonValBinders local_fix_env emptyRdrGroup{hs_tyclds = sig_types} ;
   restoreEnvs sig_envs $ do {

   -- (E) Rename type and class decls
   --     (note that value LHSes need to be in scope for default methods)
   --
   -- You might think that we could build proper def/use information
   -- for type and class declarations, but they can be involved
   -- in mutual recursion across modules, and we only do the SCC
   -- analysis for them in the type checker.
   -- So we content ourselves with gathering uses only; that
   -- means we'll only report a declaration as unused if it isn't
   -- mentioned at all.  Ah well.
   traceRn "Start rnTyClDecls" (ppr tycl_decls) ;
   (rn_source_types, source_type_fvs) <- rnTyClDecls hidden scopes (sig_types ++ updated_types) ;

   -- (F) Rename Value declarations right-hand sides
   traceRn "Start rnmono" empty ;
   let { val_bndr_set = mkNameSet id_bndrs `unionNameSet` mkNameSet pat_syn_bndrs } ;
   let { new_lhs = makeRnValBinds noExtField binds' sigs } ;
   ((rn_val_decls@(XValBindsLR (HsVBG rn_binds sigs')), bind_dus), rn_splice_types) <-
     if lift_cases && not is_boot
     then do { splice_types <- newTcRef []
             ; splice_cases <- newTcRef (0, [])
             ; let rename_splice = rnSplicedSignatureCases sigma named_wildcards local_fix_env splice_types splice_cases
             ; binds <- rnValBindsRHSWith (rnHsSigWcTypeWithSplice rename_splice)
                          (TopSigCtxt val_bndr_set) new_lhs
             ; types <- reverse <$> readTcRef splice_types
             ; pure (binds, types) }
     else (, []) <$> if is_boot
       -- The signatures bind the boot names. There are no value bindings.
       then rnTopBindsBoot tc_bndrs new_lhs
       else rnValBindsRHS (TopSigCtxt val_bndr_set) new_lhs ;
   let { rn_tycl_decls = rn_source_types ++ concatMap fst rn_splice_types
       ; src_fvs1 = plusFNs (source_type_fvs : map snd rn_splice_types) } ;
   traceRn "finish rnmono" (ppr rn_val_decls) ;

   -- (G) Rename Fixity and deprecations

   -- Rename fixity declarations and error if we try to
   -- fix something from another module (duplicates were checked in (A))
   let { all_bndrs = tc_bndrs `unionNameSet` sig_bndrs `unionNameSet` val_bndr_set } ;
   traceRn "rnSrcDecls fixity" $
     vcat [ text "all_bndrs:" <+> ppr all_bndrs ] ;
   rn_fix_decls <- mapM (mapM (rnSrcFixityDecl (TopSigCtxt all_bndrs)))
                        fix_decls ;

   -- Rename deprec decls;
   -- check for duplicates and ensure that deprecated things are defined locally
   -- at the moment, we don't keep these around past renaming
   rn_decl_warns <- rnSrcWarnDecls all_bndrs warn_decls ;

   -- (H) Rename Everything else

   (rn_rule_decls,    src_fvs2) <- setXOptM LangExt.ScopedTypeVariables $
                                   rnList rnHsRuleDecls rule_decls ;
                           -- Inside RULES, scoped type variables are on
   (rn_foreign_decls, src_fvs3) <- rnList rnHsForeignDecl foreign_decls ;
   (rn_ann_decls,     src_fvs4) <- rnList rnAnnDecl       ann_decls ;
   (rn_default_decls, src_fvs5) <- rnList rnDefaultDecl   default_decls ;
   (rn_deriv_decls,   src_fvs6) <- rnList rnSrcDerivDecl  deriv_decls ;
   (rn_splice_decls,  src_fvs7) <- rnList rnSpliceDecl    splice_decls ;
   rn_docs <- traverse rnLDocDecl docs ;

   -- Update the TcGblEnv with renamed COMPLETE pragmas from the current
   -- module, for pattern irrefutability checking in do notation.
   last_tcg_env0 <- getGblEnv ;
   let { last_tcg_env =
            last_tcg_env0
              { tcg_complete_matches = tcg_complete_matches last_tcg_env0 ++ localCompletePragmas sigs'
                                      , tcg_complete_match_env = tcg_complete_match_env last_tcg_env0 ++ localCompletePragmas sigs'}
       } ;
   -- (I) Compute the results and return
   let {rn_group = HsGroup { hs_ext     = noExtField,
                             hs_valds   = rn_val_decls,
                             hs_splcds  = rn_splice_decls,
                             hs_tyclds  = rn_tycl_decls,
                             hs_derivds = rn_deriv_decls,
                             hs_fixds   = rn_fix_decls,
                             hs_warnds  = [], -- warns are returned in the tcg_env
                                             -- (see below) not in the HsGroup
                             hs_fords  = rn_foreign_decls,
                             hs_annds  = rn_ann_decls,
                             hs_defds  = rn_default_decls,
                             hs_ruleds = rn_rule_decls,
                             hs_docs   = rn_docs } ;

        tcf_bndrs = hsTyClForeignBinders rn_tycl_decls rn_foreign_decls ;
        other_def  = (Just (mkNameSet tcf_bndrs), emptyNameSet) ;
        other_fvs  = plusFNs [src_fvs1, src_fvs2, src_fvs3, src_fvs4,
                              src_fvs5, src_fvs6, src_fvs7] ;
                -- It is tiresome to gather the binders from type and class decls

        table_fvs = mkNameEnv [ (b, fvs) | (_, bs) <- rn_binds
                            , L _ FunBind{fun_id = L _ b, fun_ext = fvs} <- bs ] ;
        table_dus = mkDUs [ (unitFN table, extendNameSet (lookupNameEnv_NF table_fvs term) term)
                          | (term, table) <- tables ] ;
        src_dus = unitOL other_def `plusDU` table_dus `plusDU` bind_dus `plusDU` usesOnly other_fvs ;
                -- Instance decls may have occurrences of things bound in bind_dus
                -- so we must put other_fvs last

        final_tcg_env = let tcg_env' = (last_tcg_env `addTcgDUs` src_dus)
                        in -- we return the deprecs in the env, not in the HsGroup above
                        tcg_env' { tcg_warns = insertWarnDecls (tcg_warns tcg_env') rn_decl_warns
                                 , tcg_rdr_env = delListFromOccEnv (tcg_rdr_env tcg_env')
                                                   (map fst hidden ++ map nameOccName table_names) };
       } ;
   traceRn "finish rnSrc" (ppr rn_group) ;
   traceRn "finish Dus" (ppr src_dus ) ;
   return (final_tcg_env, rn_group)
                    }}}}}

-- | Lift cases from the parsed result of a top-level signature splice.
-- See Note [Lifting cases from signature splices].
rnSplicedSignatureCases
  :: RdrName -> Bool -> MiniFixityEnv -> TcRef [([TyClGroup GhcRn], FreeNames)]
  -> TcRef (Int, [(RetainedCaseKey, Name)])
  -> SignatureSpliceSite -> HsDocContext -> HsUntypedSplice GhcPs
  -> RnM (HsType GhcRn, FreeNames)
rnSplicedSignatureCases sigma named_wildcards fix_env generated splice_cases site doc =
  rnSpliceTypeWith indices $ \raw -> do
    local <- getLocalRdrEnv
    case doc of
      TypeSigCtx names@(_:_)
        | contains_case raw
        , Just (outer, retained) <- splice_scope local raw
        -> do
        let rename ty = rnLHsType (SpliceTypeCtx raw) ty
            sig = HsSig noExtField outer (wrap_retained retained raw)
            wc = HsWC noExtField (L (getLoc raw) sig)
            input_sig = noLocA (TypeSig noAnn [] names wc)
            input = emptyRdrGroup{hs_valds = ValBinds noExtField [VbSig input_sig]}
        heads <- retainedSourceHeads [input_sig] []
        (ordinal, complete_helpers) <- readTcRef splice_cases
        let (earlier, suffix) = case site of
              CompleteSignatureSplice -> (complete_helpers, show)
              NestedSignatureSplice{} -> ([], \salt -> "splice" ++ show ordinal ++ "-" ++ show salt)
        let (updated, anchors, aliases, uses, _, helpers) =
              liftDeclarationTypes heads (map (fmap nameRdrName) earlier) suffix True named_wildcards input
        case (anchors, hs_valds updated) of
          (_:_, ValBinds _ [VbSig (L _ (TypeSig _ _ _ HsWC{hswc_body = L _ HsSig{sig_body = ty}}))])
            | Just result <- unwrap_retained retained ty -> do
            let (cases, case_names) = liftTypeCases emptyRdrGroup{hs_tyclds = anchors}
            (late, lambdas, scopes) <- liftTypeLambdas sigma cases
            let hidden = aliases ++ case_names ++ [(fam, Just owner) | (fam, owner) <- lambdas]
                declarations = hs_tyclds late
            forM_ uses $ \(loc, gre) -> setSrcSpanA (getLoc loc) (addUsedGRE AllDeprecationWarnings gre)
            (envs@(global, local), _) <- getLocalNonValBinders fix_env emptyRdrGroup{hs_tyclds = declarations}
            let rdr_env = foldl' extendGlobalRdrEnv (tcg_rdr_env global)
                            [mkExactGRE name Vanilla | (_, name) <- earlier]
            types <- restoreEnvs (global{tcg_rdr_env = rdr_env}, local) $
              rnTyClDecls hidden scopes declarations
            restoreEnvs envs $ do
              case site of
                CompleteSignatureSplice -> do
                  new_helpers <- forM (filter (stableSplicedCase . fst) helpers) $ \(key, occ) -> do
                    name <- getName <$> lookupTopBndrRn WL_Anything (mkRdrUnqual occ)
                    pure (key, name)
                  updTcRef splice_cases (second (new_helpers ++))
                NestedSignatureSplice{} ->
                  updTcRef splice_cases (first (+ 1))
              updTcRef generated (types :)
              rename result
          _ -> rename raw
      _ -> rnLHsType (SpliceTypeCtx raw) raw
  where
    indices = case site of
      CompleteSignatureSplice -> []
      NestedSignatureSplice _ retained -> mapMaybe hsLTyVarName retained

    wrap_retained [] ty = ty
    wrap_retained binders ty =
      L (getLoc ty) (HsForEachTy (noEpTok, Left noEpUniTok) binders ty)

    unwrap_retained [] ty = Just ty
    unwrap_retained _ (L _ HsForEachTy{hst_body = body}) = Just body
    unwrap_retained _ _ = Nothing

    contains_case :: Data a => a -> Bool
    contains_case node
      | Just (L _ (XHsType HsTypeCase{})) <- cast node :: Maybe (LHsType GhcPs)
      = True
      | otherwise = or (gmapQ contains_case node)

    splice_scope local raw = case site of
      CompleteSignatureSplice -> Just (mkHsOuterImplicit, [])
      NestedSignatureSplice outer retained -> do
        let binders = case outer of
              HsOuterImplicit{} -> []
              HsOuterExplicit{hso_bndrs = bound} -> mapMaybe (closedSpliceCapture namedSpliceType) bound
            copied = mapMaybe (closedSpliceCapture retainedSpliceDomain) retained
            available = mapMaybe hsLTyVarName binders ++ mapMaybe hsLTyVarName copied
        guard (all ((`elem` available) . unLoc) (extractHsTyRdrTyVars raw))
        guard (not (uses_other_local local available raw))
        pure (mkHsOuterExplicit noAnn binders, copied)

    uses_other_local :: Data a => LocalRdrEnv -> [RdrName] -> a -> Bool
    uses_other_local local available node
      | Just (HsBndrVar _ name) <- cast node :: Maybe (HsBndrVar GhcPs)
      , unLoc name `elem` available
      = True
      | Just (HsTypeCaseAlt pat _ _) <- cast node :: Maybe HsTypeCaseAlt
      , any ((`elem` available) . unLoc) (extractHsTyRdrTyVars pat)
      = True
      | Just rdr <- cast node :: Maybe RdrName
      = rdr `notElem` available &&
        (elemLocalRdrEnv rdr local ||
         maybe False (`elemLocalRdrEnv` local) (demoteRdrNameTv rdr))
      | otherwise = or (gmapQ (uses_other_local local available) node)

-- | Copy an exact binder and its supported domain to the helper scope.
closedSpliceCapture :: (LHsType GhcRn -> Maybe (LHsType GhcPs))
                    -> LHsTyVarBndr flag GhcRn -> Maybe (LHsTyVarBndr flag GhcPs)
closedSpliceCapture copy_type (L loc HsTvb{tvb_flag = flag, tvb_var = HsBndrVar _ name,
                                          tvb_kind = HsBndrKind _ kind}) = do
  exact_kind <- copy_type kind
  pure (L loc (HsTvb noAnn flag (HsBndrVar noExtField (fmap nameRdrName name))
                     (HsBndrKind noExtField exact_kind)))
closedSpliceCapture _ _ = Nothing

-- | Retain the original Name of a type or promoted data constructor.
namedSpliceType :: LHsType GhcRn -> Maybe (LHsType GhcPs)
namedSpliceType ty
  | L loc (HsTyVar _ promoted con) <- ignoreParens ty
  , let resolved = getName (unLoc con)
        occ = nameOccName resolved
  , isExternalName resolved
  , isTcOcc occ || isDataOcc occ
  = Just (L loc (HsTyVar noAnn promoted (nameRdrName resolved <$ con)))
  | otherwise = Nothing

-- | Copy a closed constructor application in a retained capture's domain.
retainedSpliceDomain :: LHsType GhcRn -> Maybe (LHsType GhcPs)
retainedSpliceDomain ty = case ignoreParens ty of
  L loc (HsAppTy _ fun arg) -> do
    fun' <- retainedSpliceDomain fun
    arg' <- retainedSpliceDomain arg
    pure (L loc (HsAppTy noExtField fun' arg'))
  ty -> namedSpliceType ty

{- Note [Lifting cases from signature splices]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A splice at the root of a value signature can return a type case. The
initial declaration lifting pass has only the source splice. Run the splice
in the ordinary signature renamer. Use its parsed result to construct the
same private declarations as for a written signature. Register the names
only while renaming these declarations and the result. Add the renamed
declarations to the owning group for kind checking and interface writing.

The splice runs once, in signature order. The regular splice path carries
its free names and module finalizers. A nested type splice can refer to an
enclosing signature binder by its exact name. For an erased binder, copy
a single resolved type or promoted constructor. For a retained binder,
also copy a closed constructor application. The copied domain needs no
local bindings. Present these retained binders to the ordinary lifting
pass as an enclosing telescope. Remove that synthetic telescope from the
result so the helper application refers to the original runtime binders.
Give each nested splice a distinct helper suffix so it does not collide
with a written case or another splice in the signature.
The new private declarations use the same capture and ordered case checks
as in Note [Lifting types from signatures and fields].

Two signature splices in the same declaration group can reuse one private
helper for the same case on a retained capture. Compare the formal capture's
position, the closed capture domains and every row in order. Require original
or exact external names in the domains and rows. A splice can construct an
unqualified name whose interpretation depends on the scope at its expansion.
Only the generated alias refers to the earlier helper's exact name. Register
it again for that renaming pass. End reuse at the declaration group boundary.
-}

{- Note [Resolving retained case heads]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Prepare value signature and constructor field aliases after the declaration
group's type and term binders enter scope. Register the new synonyms and
families before renaming the rewritten constructor fields. The original
constructors already have names.

In an implicitly quantified GADT constructor, a written application
head such as `not` in `case not b of ...` can name a checked function.
Exclude that occurrence from the implicit binder list when lookup finds
the function. Keep other occurrences of the same spelling and all explicit
binders. A field always keeps its own helper and original call.

An unqualified head with no binding in the current declaration group can
also reach the retained-case shape check. If the case can be lifted and
the head has no separate type occurrence or lexical binder, report the
ordinary unbound-name diagnostic at the head before renaming the helper.
Keep a separate type occurrence as evidence for an implicit constructor
binder. This lookup uses the current group and earlier checked groups.

The R16 lookup sees a local source definition or a visible import.
Each signature's written telescope can
shadow that lookup. A shared case compares resolved source family Names
and the formal index positions. Keep the original expression in the
checked helper. The regular renamer and type checker still check its
source equation.

An alias that uses a shared helper drops its own scrutinee expression.
Record the original imports of its resolved application heads at their
source locations. This keeps each qualifier and its deprecations.
-}

{- Note [Ordering local source tables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A value signature and a type declaration in one HsGroup may both use a
source table. For example:

  type Switched = flipBit 'False
  flipBit :: Bool -> Bool
  flipBit False = True
  flipBit True  = False

The renamer introduces a private type name while it renames this group.
This name records a dependency. Remove its GRE before processing exports.
For unused-binding warnings, a use of the private name counts as a use of
the term and its local constructors. Put this edge before the value groups.
A value signature in any of them can depend on the private name.
Use of the private name in a type schedules an early check of the term. The early group
contains its ordinary binding, signature, and any independent data
declarations that its signature needs. All three may follow the use in the
source file. The typechecker checks the term once and creates equations
from checked matches before checking the rest of the group.

A retained operand can use the value without mentioning the table in a type.
For example, atBit (flipBit False) can have the expected type Mark True.
Check independent candidates referenced by other bindings after checking the
group's type declarations and instances, before checking the other values.
At this point an ordinary use can have implicit dependencies on instances.
For example, an imported constructor can require a dictionary. Check the source
rows after solving the group's constraints. If a candidate has no equations,
its ordinary term uses still work. Only a retained use needs the certificate.

The early group cannot depend on the rest of the group. An H98 datatype
with no parameters may refer to itself in a field, as in data Nat = Z | S Nat.
Require each field type to be a single type constructor. A dependency on
another type in this group appears in group_ext. A private source-table
name does not, so exclude it explicitly. Keep deriving clauses in the
original group because they can use instances from later groups. A SPECIALISE
pragma can also mention arbitrary local types and values. Keep such bindings
in the original group.

A worker signature can name a saturated type synonym. GHC.Tc.Module checks
the written applications against the synonym arity. A same-group synonym
can refer to a bare type constructor or one of its own parameters. Follow
the renamed group's dependencies and check the synonym before the worker.
Each prerequisite must be a ready type or another eligible synonym. A
previous group can supply an already checked family-free synonym.

A table such as listCount :: [Bool] -> Nat can refer to itself. Move its
singleton SCC with its original recursion flag. For a list domain the
element type supplies the declaration dependency. In
sourceLength :: forall a. [a] -> Nat, the signature binds a, so only Nat
can add a dependency. Compare the renamed element Name with the binder.
Only schedule a lifted type binder with no further kind dependencies.

For a binary function such as plus (S n) m = S (plus n m), the second
domain is also a declaration dependency. The second pattern binds a
variable that the recursive call must pass unchanged. Only fields of the
first pattern can supply the decreasing argument. Check each recursive operand
in the typed producer before constructing an axiom. Return all requested
names so the caller can report an uncertified table at the source binding.

A call to another signed source function introduces a dependency between
the equations. For example, times (S n) m = plus m (times n m) needs plus
before the equations for times. Walk the existing binding SCCs in dependency
order. Admit a singleton only when its other local value dependencies are
already available. Then walk back from the requested functions to collect
their prerequisites. Keep the original SCCs for typechecking. The typed
producer checks the callee's identity and the decrease of each self call.
-}

-- | Allocate private names for signed function bindings with constructor rows.
-- The producer checks the constructors and their coverage after typechecking.
localSourceTableBinders :: [LHsBindLR GhcRn GhcPs] -> [LSig GhcPs] -> RnM [(Name, Name)]
localSourceTableBinders binds sigs =
  mapM (\n -> (n,) <$> lookupOrig (nameModule n) (mkSourceTableOcc (nameOccName n)))
    [ n | L _ FunBind{fun_id = L _ n, fun_matches = MG{mg_alts = L _ rows}} <- binds
        , isExternalName n, nameOccName n `elemOccSet` signed
        , all (constructor_row n False) rows ||
          (any matches_second rows && all (constructor_row n True) rows) ]
  where
    signed = mkOccSet [ rdrNameOcc n | L _ (TypeSig _ [] [L _ n] _) <- sigs ]
    constructor_row :: Name -> Bool -> LMatch GhcPs (LHsExpr GhcPs) -> Bool
    constructor_row self structural (L _ Match{m_pats = L _ (pat : rest), m_grhss = rhss})
      | Just fields <- constructor_fields (unparen pat)
      , all plain_field fields
      , Just carried <- carried_binders structural rest
      , GRHSs _ (L _ (GRHS _ [] rhs) :| []) (L _ EmptyLocalBinds{}) <- rhss
      = row_result self (collectPatsBinders CollNoDictBinders fields) carried rhs
    constructor_row _ _ _ = False

    -- The typed checker checks the whole matrix, including the constructor
    -- identities and the order of both columns. See Note [Local structural
    -- case equations] in GHC.Tc.Gen.SourceTable.
    matches_second (L _ Match{m_pats = L _ [_, p]}) =
      isJust (constructor_fields (unparen p))
    matches_second _ = False

    carried_binders :: Bool -> [LPat GhcPs] -> Maybe [RdrName]
    carried_binders False [] = Just []
    carried_binders _ [p] | VarPat _ (L _ v) <- unparen p = Just [v]
    carried_binders True [p]
      | WildPat{} <- unparen p = Just []
      | Just fields <- constructor_fields (unparen p)
      , all plain_field fields = Just (collectPatsBinders CollNoDictBinders fields)
    carried_binders _ _ = Nothing

    constructor_fields :: Pat GhcPs -> Maybe [LPat GhcPs]
    constructor_fields ConPat{pat_args = PrefixCon _ fields} = Just fields
    constructor_fields ConPat{pat_con = L _ con, pat_args = InfixCon _ x xs}
      | con == consDataCon_RDR = Just [x, xs]
    constructor_fields (ListPat _ []) = Just []
    constructor_fields _ = Nothing

    row_result :: Name -> [RdrName] -> [RdrName] -> LHsExpr GhcPs -> Bool
    row_result self fields carried = check []
      where
        check :: [LHsExpr GhcPs] -> LHsExpr GhcPs -> Bool
        check args expr = case unLoc (stripParensLHsExpr expr) of
          HsApp _ fun arg -> check (arg : args) fun
          OpApp _ left op right -> check (left : right : args) op
          HsAppType _ fun _
            | null carried, Just r <- variable fun, is_self r -> check args fun
          HsVar _ (L _ r)
            | isRdrDataCon r -> all (check []) args
            | r `elem` fields || r `elem` carried -> null args
            | is_self r, Just (field : rest) <- traverse variable args
            -> field `elem` fields && rest == carried
            | not (is_self r), not (null args) -> all (check []) args
          _ -> False
        variable :: LHsExpr GhcPs -> Maybe RdrName
        variable e = case unLoc (stripParensLHsExpr e) of
          HsVar _ (L _ r) -> Just r
          _ -> Nothing
        is_self r = let occ = nameOccName self in
          r == mkRdrUnqual occ ||
          r == mkRdrQual (moduleName (nameModule self)) occ ||
          rdrNameExactName_maybe r == Just self

    unparen :: LPat GhcPs -> Pat GhcPs
    unparen (L _ (ParPat _ p)) = unparen p
    unparen (L _ p) = p

    plain_field :: LPat GhcPs -> Bool
    plain_field p = case unparen p of
      VarPat{} -> True
      WildPat{} -> True
      _ -> False

-- | A type use requires equations before type declarations; a value use can wait.
data SourceTableUse = TableInType ((Name -> Bool) -> [(Name, Int)] -> Maybe NameSet)
                    | TableInTerm

-- | Schedule independent local source functions. See Note [Ordering local source tables].
splitLocalSourceTables :: SourceTableUse -> DefUses -> HsGroup GhcRn
                       -> Maybe ([(Name, Name)], HsGroup GhcRn, HsGroup GhcRn)
splitLocalSourceTables use dus group@HsGroup{hs_valds = vals@(XValBindsLR (HsVBG binds sigs)),
                                         hs_tyclds = tycls}
  | n : _ <- bound_names
  , let tables = case use of { TableInType{} -> allUses dus; TableInTerm -> duDefs dus }
        used = mkOccEnv [ (nameOccName f, f) | f <- nameSetElemsStable tables
                         , nameModule_maybe f == nameModule_maybe n ]
        wanted = [ (b, f) | b <- bound_names
                          , Just f <- [lookupOccEnv used (mkSourceTableOcc (nameOccName b))]
                          , requested b f ]
  , not (null wanted)
  = let wanted_set = mkNameSet (map fst wanted)
        specialised = mkNameSet [ n | L _ s <- sigs, n <- spec_name s ]
        signatures = mkNameEnv
          [ (b, domain ++ rest) | L _ (TypeSig _ [] [L _ b] sig) <- sigs
            , L _ HsSig{sig_bndrs = outer, sig_body = body} <- [dropWildCards sig]
            , HsFunTy _ (HsModifiedFunArr _ [] HsStandardArr{}) arg res <- [unLoc (ignoreParens body)]
            , Just domain <- [domain_dependencies outer arg], Just rest <- [result_dependencies outer res] ]
        declared = mkOccSet [ nameOccName f | f <- nameSetElemsStable (duDefs dus)
                                            , nameModule_maybe f == nameModule_maybe n ]
        candidates = [ (b, deps, values) | (_, [L _ FunBind{fun_id = L _ b, fun_ext = fvs}]) <- binds
                     , mkSourceTableOcc (nameOccName b) `elemOccSet` declared
                     , not (b `elemNameSet` specialised), Just kinds <- [lookupNameEnv signatures b]
                     , let values = (fvs `intersectNameSet` local_values) `delFromNameSet` b
                     , Just deps <- [independent_types kinds (fvs `minusNameSet` local_values)] ]
        ready = foldl' schedule emptyNameSet candidates
        schedule done (b, _, deps) | isEmptyNameSet (deps `minusNameSet` done) = extendNameSet done b
                                   | otherwise = done
        needed = foldr include wanted_set candidates
        include (b, _, deps) rest | b `elemNameSet` rest, b `elemNameSet` ready = unionNameSet deps rest
                                  | otherwise = rest
        early = [ (b, kinds) | (b, kinds, _) <- candidates
                             , b `elemNameSet` ready, b `elemNameSet` needed ]
        early_names = mkNameSet (map fst early)
        early_types = unionNameSets (map snd early)
        (first_binds, rest_binds) = partition early_bind binds
        early_bind :: (RecFlag, LHsBinds GhcRn) -> Bool
        early_bind (_, [L _ FunBind{fun_id = L _ b}]) = b `elemNameSet` early_names
        early_bind _ = False
        (first_sigs, rest_sigs) = partition (any (`elemNameSet` early_names) . sig_names . unLoc) sigs
        (first_tys, rest_tys) = partition (any ((`elemNameSet` early_types) . tcdName . unLoc) . group_tyclds) tycls
        first = emptyRnGroup { hs_valds = XValBindsLR (HsVBG first_binds first_sigs), hs_tyclds = first_tys }
        rest = group { hs_valds = XValBindsLR (HsVBG rest_binds rest_sigs), hs_tyclds = rest_tys }
    in case use of
         TableInTerm | null first_binds -> Nothing
         _ -> Just (wanted, first, rest)
  where
    bound_names = [ b | (_, bs) <- binds, L _ FunBind{fun_id = L _ b} <- bs, isExternalName b ]
    used_names = allUses dus
    requested b f = case use of
      TableInType{} -> True
      TableInTerm -> b `elemNameSet` term_uses && not (f `elemNameSet` used_names)

    -- Exclude the generated table-to-term edge and a function's own recursion.
    term_uses = unionNameSets
      [ maybe uses (uses `minusNameSet`) defs | (defs, uses) <- fromOL dus
      , not (maybe False (nameSetAny (isJust . sourceTableTermOcc . nameOccName)) defs) ]
    local_values = mkNameSet (collectHsValBinders CollNoDictBinders vals)
    local_binders = mkNameSet (hsGroupBinders group)
    independent_types kinds fvs = case use of
      TableInTerm
        | all ((== 0) . snd) kinds -> Just (mkNameSet (map fst kinds))
        | otherwise -> Nothing
      TableInType typeDeps -> do
        needed <- typeDeps (`elemNameEnv` early_datatypes) kinds
        let cons = mkNameSet (concatMap (fromMaybe [] . lookupNameEnv early_datatypes)
                                        (nameSetElemsStable needed))
        guard (nameSetAll (`elemNameSet` cons) (fvs `intersectNameSet` local_binders))
        pure needed
    type_name :: LHsType GhcRn -> Maybe Name
    type_name ty = case unLoc (ignoreParens ty) of
      HsTyVar _ _ (L _ (WithUserRdr _ name))
        | isTyConName name, isNothing (sourceTableTermOcc (nameOccName name)) -> Just name
      _ -> Nothing

    -- Count only the written visible arguments. The typechecker verifies the
    -- arity of the synonym and each dependency before moving the declarations.
    type_dependencies :: LHsType GhcRn -> Maybe [(Name, Int)]
    type_dependencies = go []
      where
        go args ty
          | L _ (HsAppTy _ fun arg) <- ignoreParens ty = go (arg : args) fun
          | otherwise = do
              name <- type_name ty
              nested <- traverse type_dependencies args
              pure ((name, length args) : concat nested)

    domain_dependencies :: HsOuterSigTyVarBndrs GhcRn -> LHsType GhcRn -> Maybe [(Name, Int)]
    domain_dependencies outer ty = case (outer_vars outer, list_element ty) of
      (Just [], element) -> type_dependencies (fromMaybe ty element)
      (Just [a], Just element)
        | L _ (HsTyVar _ _ (L _ (WithUserRdr _ name))) <- ignoreParens element
        , name == a -> Just []
      _ -> Nothing

    result_dependencies :: HsOuterSigTyVarBndrs GhcRn -> LHsType GhcRn -> Maybe [(Name, Int)]
    result_dependencies outer ty = case unLoc (ignoreParens ty) of
      HsFunTy _ (HsModifiedFunArr _ [] HsStandardArr{}) carried result
        | Just [] <- outer_vars outer -> do
            deps <- domain_dependencies outer carried
            res <- type_dependencies result
            pure (deps ++ res)
      _ -> type_dependencies ty

    list_element :: LHsType GhcRn -> Maybe (LHsType GhcRn)
    list_element ty = case unLoc (ignoreParens ty) of
      HsListTy _ element -> Just element
      HsAppTy _ list element
        | Just con <- type_name list, nameUnique con == listTyConKey -> Just element
      _ -> Nothing

    outer_vars :: HsOuterSigTyVarBndrs GhcRn -> Maybe [Name]
    outer_vars (HsOuterImplicit names) = Just names
    outer_vars (HsOuterExplicit _ Nothing []) = Just []
    outer_vars (HsOuterExplicit _ Nothing [L _ (HsTvb _ SpecifiedSpec (HsBndrVar _ (L _ a)) kind)])
      | plain_kind kind = Just [a]
    outer_vars _ = Nothing

    plain_kind :: HsBndrKind GhcRn -> Bool
    plain_kind HsBndrNoKind{} = True
    plain_kind (HsBndrKind _ k)
      | HsStarTy{} <- unLoc (ignoreParens k) = True
      | Just n <- type_name k = nameUnique n == liftedTypeKindTyConKey
    plain_kind _ = False

    early_datatypes = mkNameEnv
      [ (t, cons) | TyClGroup{group_ext = deps, group_kisigs = [], group_tyclds =
                     [L _ DataDecl{tcdLName = L _ t, tcdModifiers = [],
                                   tcdTyVars = HsQTvs{hsq_explicit = []},
                                   tcdDataDefn = HsDataDefn{dd_derivs = [], dd_ctxt = Nothing,
                                     dd_kindSig = Nothing, dd_cons = DataTypeCons False cs}}]} <- tycls
                  , isEmptyNameSet deps
                  , Just cons <- [traverse prefix_con cs] ]
    prefix_con :: LConDecl GhcRn -> Maybe Name
    prefix_con (L _ ConDeclH98{con_name = L _ c, con_modifiers = [], con_telescope = [],
                               con_args = PrefixCon _ fields})
      | all independent_field fields = Just c
    prefix_con _ = Nothing

    independent_field :: HsConDeclField GhcRn -> Bool
    independent_field CDF{cdf_type = ty, cdf_multiplicity = HsModifiedFunArr _ [] HsStandardArr{}}
      = isJust (type_name ty)
    independent_field _ = False

    sig_names :: Sig GhcRn -> [Name]
    sig_names (TypeSig _ _ [L _ b] _) = [b]
    sig_names (InlineSig _ (L _ b) _) = [b]
    sig_names (SCCFunSig _ (L _ b) _) = [b]
    sig_names _ = []
    spec_name :: Sig GhcRn -> [Name]
    spec_name (SpecSig _ (L _ b) _ _) = [b]
    spec_name (SpecSigE b _ _ _) = [b]
    spec_name _ = []
splitLocalSourceTables _ _ _ = Nothing

addTcgDUs :: TcGblEnv -> DefUses -> TcGblEnv
-- This function could be defined lower down in the module hierarchy,
-- but there doesn't seem anywhere very logical to put it.
addTcgDUs tcg_env dus = tcg_env { tcg_dus = tcg_dus tcg_env `plusDU` dus }

rnList :: (a -> RnM (b, FreeNames)) -> [LocatedA a] -> RnM ([LocatedA b], FreeNames)
rnList f xs = mapFvRn (wrapLocFstMA f) xs

{-
*********************************************************
*                                                       *
        Source-code deprecations declarations
*                                                       *
*********************************************************

Check that the deprecated names are defined, are defined locally, and
that there are no duplicate deprecations.

It's only imported deprecations, dealt with in RnIfaces, that we
gather them together.
-}

-- checks that the deprecations are defined locally, and that there are no duplicates
rnSrcWarnDecls :: NameSet -> [LWarnDecls GhcPs] -> RnM (DeclWarnOccNames GhcRn)
rnSrcWarnDecls _ []
  = return []

rnSrcWarnDecls bndr_set decls'
  = do { -- check for duplicates
       ; mapM_ (\ dups -> let ((L loc rdr) :| (lrdr':_)) = fmap snd dups
                          in addErrAt (locA loc) (TcRnDuplicateWarningDecls lrdr' rdr))
               warn_rdr_dups
       ; pairs_s <- mapM (addLocM rn_deprec) decls
       ; return $ concat pairs_s }
 where
   decls = concatMap (wd_warnings . unLoc) decls'

   sig_ctxt = TopSigCtxt bndr_set

   rn_deprec w@(Warning _ ns_spec rdr_names txt)
       -- ensures that the names are defined locally
     = do { names <- concatMapM (lookupLocalTcNames sig_ctxt SigLikeDeprecation ns_spec . unLoc)
                                rdr_names
          ; unlessXOptM LangExt.ExplicitNamespaces $
            case ns_spec of
              NoNamespaceSpecifier{} -> return ()
              _ -> addErr (TcRnNamespacedWarningPragmaWithoutFlag w)
          ; txt' <- rnWarningTxt txt
          ; return [(nameOccName nm, txt') | (_, nm) <- names] }
  -- Use the OccName from the Name we looked up, rather than from the RdrName,
  -- as we might hit multiple different NameSpaces when looking up
  -- (e.g. deprecating both a variable and a record field).

   warn_rdr_dups = find_dup_warning_names
                   $ concatMap (\(L _ (Warning _ ns_spec ns _)) -> (ns_spec,) <$> ns) decls

   find_dup_warning_names :: [(NamespaceSpecifier GhcPs, LocatedN RdrName)] -> [NonEmpty (NamespaceSpecifier GhcPs, LocatedN RdrName)]
   find_dup_warning_names = findDupsEq (\ (spec1, x) -> \ (spec2, y) ->
                              overlappingNamespaceSpecifiers spec1 spec2 &&
                              rdrNameOcc (unLoc x) == rdrNameOcc (unLoc y))

rnWarningTxt :: WarningTxt GhcPs -> RnM (WarningTxt GhcRn)
rnWarningTxt (WarningTxt st mb_cat wst) = do
  mb_cat' <- case mb_cat of
    Nothing -> pure Nothing
    Just (L x (InWarningCategory y (L loc cat))) -> do
      unless (validWarningCategory cat) $
        addErrAt (locA loc) (TcRnInvalidWarningCategory cat)
      pure . Just $ L x (InWarningCategory y (L loc cat))
  wst' <- traverse (traverse rnHsDocIdentifiersOnly) wst
  pure . WarningTxt st mb_cat' $ fmap rnWithHsDocIdentifiers <$> wst'

rnWarningTxt (DeprecatedTxt st wst) = do
  wst' <- traverse (traverse rnHsDocIdentifiersOnly) wst
  pure . DeprecatedTxt st $ fmap rnWithHsDocIdentifiers <$> wst'

rnWithHsDocIdentifiers ::
  WithHsDocIdentifiers (StringLiteral GhcPs) GhcRn ->
  WithHsDocIdentifiers (StringLiteral GhcRn) GhcRn
rnWithHsDocIdentifiers ids = WithHsDocIdentifiers
  { hsDocString      = rnStringLit $ hsDocString ids
  , hsDocIdentifiers = hsDocIdentifiers ids
  }

rnLWarningTxt :: LWarningTxt GhcPs -> RnM (LWarningTxt GhcRn)
rnLWarningTxt (L loc warn) = L loc <$> rnWarningTxt warn

-- look for duplicates among the OccNames;
-- we check that the names are defined above
-- invt: the lists returned by findDupsEq always have at least two elements

{-
*********************************************************
*                                                      *
\subsection{Annotation declarations}
*                                                      *
*********************************************************
-}

rnAnnDecl :: AnnDecl GhcPs -> RnM (AnnDecl GhcRn, FreeNames)
rnAnnDecl ann@(HsAnnotation (_, s) provenance expr)
  = addErrCtxt (AnnCtxt ann) $
    do { (provenance', provenance_fvs) <- rnAnnProvenance provenance
       ; cur_level <- getThLevel
       ; (expr', expr_fvs) <- setThLevel (Splice Untyped cur_level) $
                              rnLExpr expr
       ; return (HsAnnotation (noAnn, s) provenance' expr',
                 provenance_fvs `plusFN` expr_fvs) }

rnAnnProvenance :: AnnProvenance GhcPs
                -> RnM (AnnProvenance GhcRn, FreeNames)
rnAnnProvenance provenance = do
    provenance' <- case provenance of
      ValueAnnProvenance n -> ValueAnnProvenance
                          <$> lookupLocatedTopBndrRnN WL_Term n
      TypeAnnProvenance n  -> TypeAnnProvenance
                          <$> lookupLocatedTopBndrRnN WL_Type n
      ModuleAnnProvenance  -> return ModuleAnnProvenance
    return (provenance', maybe emptyFNs unitFN (annProvenanceName_maybe provenance'))

{-
*********************************************************
*                                                      *
\subsection{Default declarations}
*                                                      *
*********************************************************
-}

rnDefaultDecl :: DefaultDecl GhcPs -> RnM (DefaultDecl GhcRn, FreeNames)
rnDefaultDecl (DefaultDecl _ mods mb_cls tys)
  = do {
       ; mb_cls' <- traverse (traverse $ lookupOccRn WL_TyCon) mb_cls
       ; (tys', ty_fvs) <- rnLHsTypes doc_str tys
       ; (mods', mods_fvs) <- rnModifiersContextAndWarn doc_str mods
       ; return (DefaultDecl noExtField mods' mb_cls' tys', ty_fvs `plusFN` mods_fvs) }
  where
    doc_str = DefaultDeclCtx

{-
*********************************************************
*                                                      *
\subsection{Foreign declarations}
*                                                      *
*********************************************************
-}

rnHsForeignDecl :: ForeignDecl GhcPs -> RnM (ForeignDecl GhcRn, FreeNames)
rnHsForeignDecl (ForeignImport { fd_name = name, fd_sig_ty = ty, fd_fi = spec, fd_modifiers = modifiers })
  = do { topEnv :: HscEnv <- getTopEnv
       ; name' <- lookupLocatedTopBndrRnN WL_TermVariable name
       ; let ctxt = ForeignDeclCtx name
       ; (ty', fvs) <- rnHsSigType ctxt TypeLevel ty

        -- Mark any PackageTarget style imports as coming from the current package
       ; let home_unit = hsc_home_unit topEnv
             spec'  = patchForeignImport (homeUnitAsUnit home_unit) spec

       ; (modifiers', mods_fvs) <- rnModifiersContext ctxt modifiers

       ; return (ForeignImport { fd_i_ext = noExtField
                               , fd_name = name', fd_sig_ty = ty'
                               , fd_fi = spec', fd_modifiers = modifiers' }
                , fvs `plusFN` mods_fvs) }

rnHsForeignDecl (ForeignExport { fd_name = name, fd_sig_ty = ty, fd_fe = spec, fd_modifiers = modifiers })
  = do { name' <- lookupLocatedOccRn WL_TermVariable name
       ; let ctxt = ForeignDeclCtx name
       ; (ty', fvs) <- rnHsSigType (ForeignDeclCtx name) TypeLevel ty
       ; (modifiers', mods_fvs) <- rnModifiersContext ctxt modifiers
       ; return (ForeignExport { fd_e_ext = noExtField
                               , fd_name = name', fd_sig_ty = ty'
                               , fd_fe = (\(CExport x c) -> CExport x c) spec
                               , fd_modifiers = modifiers' }
                , fvs `plusFN` mods_fvs `addOneFN` unLoc name') }
        -- NB: a foreign export is an *occurrence site* for name, so
        --     we add it to the free-variable list.  It might, for example,
        --     be imported from another module

-- | For Windows DLLs we need to know what packages imported symbols are from
--      to generate correct calls. Imported symbols are tagged with the current
--      package, so if they get inlined across a package boundary we'll still
--      know where they're from.
--
patchForeignImport :: Unit -> (ForeignImport GhcPs) -> (ForeignImport GhcRn)
patchForeignImport unit (CImport ext cconv safety fs spec)
        = CImport ext cconv safety (renameHeader <$> fs) (patchCImportSpec unit spec)

patchCImportSpec :: Unit -> CImportSpec GhcPs -> CImportSpec GhcRn
patchCImportSpec unit = \case
    CFunction callTarget -> CFunction $ patchCCallTarget unit callTarget
    CLabel    cLabel     -> CLabel cLabel
    CWrapper             -> CWrapper

patchCCallTarget :: Unit -> CCallTarget GhcPs -> CCallTarget GhcRn
patchCCallTarget unit = \case
    DynamicTarget x -> DynamicTarget x
    StaticTarget sTxt label targetKind ->
      let ext = StaticTargetGhc
            { staticTargetLabel = sTxt
            , staticTargetUnit  = TargetIsInThat unit
            }
      in  StaticTarget ext label targetKind

{-
*********************************************************
*                                                      *
\subsection{Instance declarations}
*                                                      *
*********************************************************
-}

rnSrcInstDecl :: InstDecl GhcPs -> RnM (InstDecl GhcRn, FreeNames)
rnSrcInstDecl (TyFamInstD { tfid_inst = tfi })
  = do { (tfi', fvs) <- rnTyFamInstDecl (NonAssocTyFamEqn NotClosedTyFam) tfi
       ; return (TyFamInstD { tfid_ext = noExtField, tfid_inst = tfi' }, fvs) }

rnSrcInstDecl (DataFamInstD { dfid_inst = dfi })
  = do { (dfi', fvs) <- rnDataFamInstDecl (NonAssocTyFamEqn NotClosedTyFam) dfi
       ; return (DataFamInstD { dfid_ext = noExtField, dfid_inst = dfi' }, fvs) }

rnSrcInstDecl (ClsInstD { cid_inst = cid })
  = do { traceRn "rnSrcIstDecl {" (ppr cid)
       ; (cid', fvs) <- rnClsInstDecl cid
       ; traceRn "rnSrcIstDecl end }" empty
       ; return (ClsInstD { cid_d_ext = noExtField, cid_inst = cid' }, fvs) }

-- | Warn and error for non-canonical typeclass instance declarations
--
-- A "non-canonical" instance definition can occur for instances of a
-- class which redundantly defines an operation its superclass
-- provides as well (c.f. `return`/`pure`). In such cases, a canonical
-- instance is one where the subclass inherits its method
-- implementation from its superclass instance (usually the subclass
-- has a default method implementation to that effect). Consequently,
-- a non-canonical instance occurs when this is not the case.
--
-- See also descriptions of 'checkCanonicalMonadInstances' and
-- 'checkCanonicalMonoidInstances'
checkCanonicalInstances :: Name -> LHsSigType GhcRn -> LHsBinds GhcRn -> RnM ()
checkCanonicalInstances cls poly_ty mbinds = do
    checkCanonicalMonadInstances
    checkCanonicalMonoidInstances
  where
    -- Raise errors or warnings for non-canonical 'Applicative'/'Monad' 'pure'/'return',
    -- and warn for non-canonical '(>>)' declarations. Specifically, the
    -- following conditions are verified:
    --
    -- In 'Monad' instances declarations:
    --
    --  * Warn if @return = pure@, error if return is otherwise overridden
    --  * If '(>>)' is overridden it must be canonical (i.e. @(>>) = (*>)@)
    --
    -- In 'Applicative' instance declarations:
    --
    --  * Error if 'pure' is defined backwards (i.e. @pure = return@).
    --  * Warn if '(*>)' is defined backwards (i.e. @(*>) = (>>)@).
    --
    checkCanonicalMonadInstances
      | cls `hasKnownKey` applicativeClassKey =
          forM_ mbinds $ \(L loc mbind) -> setSrcSpanA loc $
              case mbind of
                  FunBind { fun_id = L _ name
                          , fun_matches = mg }
                      | name `hasKnownKey` pureAClassOpKey
                      , isAliasMG mg returnMClassOpKey
                      -> addErrNonCanonicalMonad NonCanonical_Pure

                      | name `hasKnownKey` thenAClassOpKey
                      , isAliasMG mg thenMClassOpKey
                      -> whenWOptM Opt_WarnNonCanonicalMonadInstances $
                          addWarnNonCanonicalMonad NonCanonical_ThenA

                  _ -> return ()

      | cls `hasKnownKey` monadClassKey =
          forM_ mbinds $ \(L loc mbind) -> setSrcSpanA loc $
              case mbind of
                  FunBind { fun_id = L _ name
                          , fun_matches = mg }
                      | name `hasKey` returnMClassOpKey
                      ->
                        if isAliasMG mg pureAClassOpKey then
                          redundantCanonicalMethod Canonical_Return
                        else
                          addErrNonCanonicalMonad NonCanonical_Return

                      -- we currently always allow canonical `(>>)` definitions
                      -- and just warn on non-canonical definitions
                      | name `hasKey` thenMClassOpKey
                      , not (isAliasMG mg thenAClassOpKey)
                      -> whenWOptM Opt_WarnNonCanonicalMonadInstances $
                          addWarnNonCanonicalMonad NonCanonical_ThenM

                  _ -> return ()

      | otherwise = return ()

    -- Raise errors or warnings for non-canonical 'Semigroup'/'Monoid' '(<>)'/'mappend'
    -- declarations. Specifically, the following conditions are verified:
    --
    -- In 'Monoid' instances declarations:
    --
    --  * Warn if @mappend = (<>)@, error if 'mappend' is otherwise overridden
    --
    -- In 'Semigroup' instance declarations:
    --
    --  * Error if '(<>)' is defined backwards (i.e. @(<>) = mappend@).
    --
    checkCanonicalMonoidInstances
      | cls `hasKnownKey`semigroupClassKey =
          forM_ mbinds $ \(L loc mbind) -> setSrcSpanA loc $
              case mbind of
                  FunBind { fun_id      = L _ name
                          , fun_matches = mg }
                      | name `hasKnownKey` sappendClassOpKey
                      , isAliasMG mg mappendClassOpKey
                      -> addErrNonCanonicalMonoid NonCanonical_Sappend

                  _ -> return ()

      | cls `hasKnownKey` monoidClassKey =
          forM_ mbinds $ \(L loc mbind) -> setSrcSpanA loc $
              case mbind of
                  FunBind { fun_id = L _ name
                          , fun_matches = mg }
                      | name `hasKnownKey` mappendClassOpKey
                      ->
                        if isAliasMG mg sappendClassOpKey then
                          redundantCanonicalMethod Canonical_Mappend
                        else
                          addErrNonCanonicalMonoid NonCanonical_Mappend

                  _ -> return ()

      | otherwise = return ()

    -- test whether MatchGroup represents a trivial \"lhsName = rhsName\"
    -- binding, and return @Just rhsName@ if this is the case
    isAliasMG :: MatchGroup GhcRn (LHsExpr GhcRn) -> KnownKey -> Bool
    isAliasMG (MG {mg_alts = (L _ [L _ (Match { m_pats = L _ []
                                              , m_grhss = grhss })])}) key
        | GRHSs _ (L _ (GRHS _ [] body) :| []) lbinds <- grhss
        , L _ (EmptyLocalBinds _) <- lbinds
        , HsVar _ (L _ rhsName)  <- unLoc body
        = getName rhsName `hasKnownKey` key
    isAliasMG _ _ = False

    addErrNonCanonicalMonoid reason =
      addErrNonCanonicalDefinition (NonCanonicalMonoid reason)

    addErrNonCanonicalMonad reason =
      addErrNonCanonicalDefinition (NonCanonicalMonad reason)

    addErrNonCanonicalDefinition reason =
      addErr (TcRnNonCanonicalDefinition reason poly_ty)

    addWarnNonCanonicalMonad reason =
      addWarnNonCanonicalDefinition (NonCanonicalMonad reason)

    addWarnNonCanonicalDefinition reason =
      addDiagnostic (TcRnNonCanonicalDefinition reason poly_ty)

    redundantCanonicalMethod reason =
      whenWOptM Opt_WarnRedundantCanonicalMethods $
        addDiagnostic (TcRnRedundantCanonicalMethod reason poly_ty)

rnClsInstDecl :: ClsInstDecl GhcPs -> RnM (ClsInstDecl GhcRn, FreeNames)
rnClsInstDecl (ClsInstDecl { cid_ext = (inst_warn_ps, _)
                           , cid_poly_ty = inst_ty
                           , cid_decls = decls
                           , cid_overlap_mode = omode
                           , cid_modifiers = modifiers })
  = do { rec { let HsNestedGroup { ng_meths = mbinds, ng_sigs = uprags
                                 , ng_tyfam_insts = ats
                                 , ng_datafam_insts = adts} = partitionBindsAndSigs decls
             ; let ctxt = ClassInstanceCtx head_ty'
             ; checkInferredVars ctxt inst_ty
             ; (inst_ty', inst_fvs) <- rnHsSigType ctxt TypeLevel inst_ty
             ; let (ktv_names, _, head_ty') = splitLHsInstDeclTy inst_ty'
             }
      ; env <- getGlobalRdrEnv
      ; let
          -- Check if there are any nested `forall`s or contexts, which are
          -- illegal in the type of an instance declaration (see
          -- Note [No nested foralls or contexts in instance types] in
          -- GHC.Hs.Type)...
          mb_nested_msg = noNestedForallsContextsErr NFC_InstanceHead head_ty'
          -- ...then check if the instance head is actually headed by a
          -- class type constructor...
          instance_head :: Either IllegalInstanceHeadReason Name
          instance_head =
           case hsTyGetAppHead_maybe head_ty' of
             Just (L _ nm_with_rdr) ->
               let nm = getName nm_with_rdr in
               case lookupGRE_Name env nm of
                 Just (GRE { gre_info = IAmTyCon flav }) ->
                   if
                     | flav == ClassFlavour
                     -> Right nm
                     | flav == AbstractTypeFlavour
                     -> Left $ InstHeadAbstractClass nm_with_rdr
                     | otherwise
                     -> Left $ InstHeadNonClassHead $ InstNonClassTyCon nm_with_rdr flav
                 _ ->
                  -- The head of the instance head is out of scope;
                  -- we'll deal with that later. Continue for now.
                  Right nm

             Nothing ->
               Left $ InstHeadNonClassHead InstNonTyCon
          eith_cls =
            case instance_head of
              Right cls -> Right cls
              Left illegal_head_reason ->
                 Left
                  ( getLocA head_ty'
                  , TcRnIllegalInstance $
                      IllegalClassInstance (HsTypeRnThing $ unLoc head_ty') $
                      IllegalInstanceHead illegal_head_reason
                  )
         -- ...finally, attempt to retrieve the class type constructor, failing
         -- with an error message if there isn't one. To avoid excessive
         -- amounts of error messages, we will only report one of the errors
         -- from mb_nested_msg or eith_cls at a time.
       ; cls <- case (mb_nested_msg, eith_cls) of
           (Nothing,   Right cls) -> pure cls
           (Just err1, _)         -> bail_out ctxt err1
           (_,         Left err2) -> bail_out ctxt err2

          -- Rename the bindings
          -- The typechecker (not the renamer) checks that all
          -- the bindings are for the right class
          -- (Slightly strangely) when scoped type variables are on, the
          -- forall-d tyvars scope over the method bindings too
       ; (mbinds', uprags', meth_fvs) <- rnMethodBinds False cls ktv_names mbinds uprags

       ; checkCanonicalInstances cls inst_ty' mbinds'

       -- Rename the associated types, and type signatures
       -- Both need to have the instance type variables in scope
       ; traceRn "rnSrcInstDecl" (ppr inst_ty' $$ ppr ktv_names)
       ; ((ats', adts'), more_fvs)
             <- bindLocalNamesFV ktv_names $
                do { (ats',  at_fvs)  <- rnATInstDecls rnTyFamInstDecl cls ktv_names ats
                   ; (adts', adt_fvs) <- rnATInstDecls rnDataFamInstDecl cls ktv_names adts
                   ; return ( (ats', adts'), at_fvs `plusFN` adt_fvs) }

       ; let omode'  = rnOverlapMode omode
       ; (modifiers', mods_fvs) <- rnModifiersContextAndWarn ctxt modifiers
       ; let all_fvs = meth_fvs `plusFN` more_fvs
                                `plusFN` inst_fvs
                                `plusFN` mods_fvs
       ; inst_warn_rn <- mapM rnLWarningTxt inst_warn_ps
       ; return (ClsInstDecl { cid_poly_ty = inst_ty'
                             , cid_decls = [] -- See Note [Pass-sensitive decls for ClassDecls/ClsInstDecls]
                             , cid_ext = (inst_warn_rn, HsNestedGroup
                                            { ng_meths = mbinds'
                                            , ng_sigs = uprags'
                                            , ng_tyfam_insts = ats'
                                            , ng_datafam_insts = adts'
                                            -- Next two not used for ClsInstDecl
                                            , ng_ats = []
                                            , ng_docs = []})
                             , cid_overlap_mode = omode'
                             , cid_modifiers = modifiers' },
                 all_fvs) }
             -- We return the renamed associated data type declarations so
             -- that they can be entered into the list of type declarations
             -- for the binding group, but we also keep a copy in the instance.
             -- The latter is needed for well-formedness checks in the type
             -- checker (eg, to ensure that all ATs of the instance actually
             -- receive a declaration).
             -- NB: Even the copies in the instance declaration carry copies of
             --     the instance context after renaming.  This is a bit
             --     strange, but should not matter (and it would be more work
             --     to remove the context).
  where
    -- The instance is malformed. We'd still like to make *some* progress
    -- (rather than failing outright), so we report an error and continue for
    -- as long as we can. Importantly, this error should be thrown before we
    -- reach the typechecker, lest we encounter different errors that are
    -- hopelessly confusing (such as the one in #16114).
    bail_out ctxt (l, err_msg) = do
      addErrAt l $ TcRnWithHsDocContext ctxt err_msg
      pure $ mkUnboundName (mkTcOccFS (fsLit "<class>"))

rnOverlapMode :: Maybe (XRec GhcPs (OverlapMode GhcPs))
              -> Maybe (XRec GhcRn (OverlapMode GhcRn))
rnOverlapMode =
    let advancePass = \case
          NoOverlap    s -> NoOverlap    s
          Overlappable s -> Overlappable s
          Overlapping  s -> Overlapping  s
          Overlaps     s -> Overlaps     s
          Incoherent   s -> Incoherent   s
          NonCanonical s -> NonCanonical s
    in  fmap (fmap advancePass)

rnFamEqn :: HsDocContext
         -> AssocTyFamInfo
         -> FamEqn GhcPs rhs
         -> (HsDocContext -> rhs -> RnM (rhs', FreeNames))
         -> RnM (FamEqn GhcRn rhs', FreeNames)
rnFamEqn doc atfi
    (FamEqn { feqn_tycon  = tycon
            , feqn_bndrs  = outer_bndrs
            , feqn_pats   = pats
            , feqn_fixity = fixity
            , feqn_rhs    = payload }) rn_payload
  = do { tycon' <- lookupFamInstName mb_cls tycon

         -- all_imp_vars represent the implicitly bound type variables. This is
         -- empty if we have an explicit `forall` (see
         -- Note [forall-or-nothing rule] in GHC.Hs.Type), which means
         -- ignoring pat_kity_vars, the free variables mentioned in the type patterns
         -- on the LHS of the equation
         --
         -- Some examples:
         --
         -- @
         -- type family F a b
         -- type instance forall a b c. F [(a, b)] c = a -> b -> c
         --   -- all_imp_vars = []
         -- type instance F [(a, b)] c = a -> b -> c
         --   -- all_imp_vars = [a, b, c]
         --
         -- type family G :: Maybe a
         -- type instance forall a. G = (Nothing :: Maybe a)
         --   -- all_imp_vars = []
         --
         -- data family H :: k -> Type
         -- data instance forall k. H :: k -> Type where ...
         --   -- all_imp_vars = []
         -- data instance H :: k -> Type where ...
         --   -- all_imp_vars = [k]
         -- @
         --
         -- For associated type family instances, exclude the type variables
         -- bound by the instance head with filterInScopeM (#19649).
       ; all_imp_vars <- filterInScopeM $ pat_kity_vars

       ; bindHsOuterTyVarBndrs doc mb_cls all_imp_vars outer_bndrs $ \rn_outer_bndrs ->
    do { (pats', pat_fvs) <- rnLHsTypeArgs (FamPatCtx tycon) pats
       ; (payload', rhs_fvs) <- rn_payload doc payload

          -- Report unused binders on the LHS
          -- See Note [Unused type variables in family instances]
       ; let -- The SrcSpan that bindHsOuterFamEqnTyVarBndrs will attach to each
             -- implicitly bound type variable Name in outer_bndrs' will
             -- span the entire type family instance, which will be reflected in
             -- -Wunused-type-patterns warnings. We can be a little more precise
             -- than that by pointing to the LHS of the instance instead, which
             -- is what lhs_loc corresponds to.
             rn_outer_bndrs' = mapHsOuterImplicit (map (`setNameLoc` lhs_loc))
                                                  rn_outer_bndrs

             groups :: [NonEmpty (LocatedN RdrName)]
             groups = equivClasses cmpLocated pat_kity_vars
       ; nms_dups <- mapM (lookupOccRn WL_TyVar . unLoc) $
                        [ tv | (tv :| (_:_)) <- groups ]
             -- Add to the used variables
             --  a) any variables that appear *more than once* on the LHS
             --     e.g.   F a Int a = Bool
             --  b) for associated instances, the variables
             --     of the instance decl.  See
             --     Note [Unused type variables in family instances]
       ; let nms_used = extendNameSetList rhs_fvs $
                           nms_dups {- (a) -} ++ inst_head_tvs {- (b) -}
             all_nms = hsOuterTyVarNames rn_outer_bndrs'
       ; warnUnusedTypePatterns all_nms nms_used

         -- For associated family instances, if a type variable from the
         -- parent instance declaration is mentioned on the RHS of the
         -- associated family instance but not bound on the LHS, then reject
         -- that type variable as being out of scope.
         -- See Note [Renaming associated types].
         -- Per that Note, the LHS type variables consist of the variables
         -- mentioned in the instance's type patterns (pat_fvs)
       ; let improperly_scoped cls_tkv =
                  cls_tkv `elemNameSet` rhs_fvs
                    -- Mentioned on the RHS...
               && not (cls_tkv `elemNameSet` pat_fvs)
                    -- ...but not bound on the LHS.
             bad_tvs = filter improperly_scoped inst_head_tvs
       ; for_ (nonEmpty bad_tvs) $ \ ne_bad_tvs ->
           addErr $
           TcRnIllegalInstance $ IllegalFamilyInstance $
             FamInstRHSOutOfScopeTyVars Nothing ne_bad_tvs

       ; let eqn_fvs = rhs_fvs `plusFN` pat_fvs
             -- See Note [Type family equations and occurrences]
             all_fvs = case atfi of
                         NonAssocTyFamEqn ClosedTyFam
                           -> eqn_fvs
                         _ -> eqn_fvs `addOneFN` unLoc tycon'

       ; return (FamEqn { feqn_ext    = noAnn
                        , feqn_tycon  = tycon'
                          -- Note [Wildcards in family instances]
                        , feqn_bndrs  = rn_outer_bndrs'
                        , feqn_pats   = pats'
                        , feqn_fixity = fixity
                        , feqn_rhs    = payload' },
                 all_fvs) } }
  where
    -- The parent class, if we are dealing with an associated type family
    -- instance.
    mb_cls = case atfi of
      NonAssocTyFamEqn _   -> Nothing
      AssocTyFamDeflt cls  -> Just cls
      AssocTyFamInst cls _ -> Just cls

    -- The type variables from the instance head, if we are dealing with an
    -- associated type family instance.
    inst_head_tvs = case atfi of
      NonAssocTyFamEqn _             -> []
      AssocTyFamDeflt _              -> []
      AssocTyFamInst _ inst_head_tvs -> inst_head_tvs

    pat_kity_vars = extractHsTyArgRdrKiTyVars pats
             -- It is crucial that extractHsTyArgRdrKiTyVars return
             -- duplicate occurrences, since they're needed to help
             -- determine unused binders on the LHS.

    -- The SrcSpan of the LHS of the instance. For example, lhs_loc would be
    -- the highlighted part in the example below:
    --
    --   type instance F a b c = Either a b
    --                   ^^^^^
    lhs_loc = case map lhsTypeArgSrcSpan pats of
      []         -> panic "rnFamEqn.lhs_loc"
      [loc]      -> loc
      (loc:locs) -> loc `combineSrcSpans` last locs

rnTyFamInstDecl :: AssocTyFamInfo
                -> TyFamInstDecl GhcPs
                -> RnM (TyFamInstDecl GhcRn, FreeNames)
rnTyFamInstDecl atfi (TyFamInstDecl { tfid_xtn = x, tfid_eqn = eqn })
  = do { (eqn', fvs) <- rnTyFamInstEqn atfi eqn
       ; return (TyFamInstDecl { tfid_xtn = x, tfid_eqn = eqn' }, fvs) }

-- | Tracks whether we are renaming:
--
-- 1. A type family equation that is not associated
--    with a parent type class ('NonAssocTyFamEqn'). Examples:
--
--    @
--    type family F a
--    type instance F Int = Bool  -- NonAssocTyFamEqn NotClosed
--
--    type family G a where
--       G Int = Bool             -- NonAssocTyFamEqn Closed
--    @
--
-- 2. An associated type family default declaration ('AssocTyFamDeflt').
--    Example:
--
--    @
--    class C a where
--      type A a
--      type instance A a = a -> a  -- AssocTyFamDeflt C
--    @
--
-- 3. An associated type family instance declaration ('AssocTyFamInst').
--    Example:
--
--    @
--    instance C a => C [a] where
--      type A [a] = Bool  -- AssocTyFamInst C [a]
--    @
data AssocTyFamInfo
  = NonAssocTyFamEqn
      ClosedTyFamInfo -- Is this a closed type family?
  | AssocTyFamDeflt
      Name            -- Name of the parent class
  | AssocTyFamInst
      Name            -- Name of the parent class
      [Name]          -- Names of the tyvars of the parent instance decl

-- | Tracks whether we are renaming an equation in a closed type family
-- equation ('ClosedTyFam') or not ('NotClosedTyFam').
data ClosedTyFamInfo
  = NotClosedTyFam
  | ClosedTyFam

rnTyFamInstEqn :: AssocTyFamInfo
               -> TyFamInstEqn GhcPs
               -> RnM (TyFamInstEqn GhcRn, FreeNames)
rnTyFamInstEqn atfi eqn@(FamEqn { feqn_tycon = tycon })
  = rnFamEqn (TySynCtx tycon) atfi eqn rnTySyn


rnTyFamDefltDecl :: Name
                 -> TyFamDefltDecl GhcPs
                 -> RnM (TyFamDefltDecl GhcRn, FreeNames)
rnTyFamDefltDecl cls = rnTyFamInstDecl (AssocTyFamDeflt cls)

rnDataFamInstDecl :: AssocTyFamInfo
                  -> DataFamInstDecl GhcPs
                  -> RnM (DataFamInstDecl GhcRn, FreeNames)
rnDataFamInstDecl atfi (DataFamInstDecl { dfid_eqn =
                    eqn@(FamEqn { feqn_tycon = tycon })})
  = do { (eqn', fvs) <-
           rnFamEqn (TyDataCtx tycon) atfi eqn rnDataDefn
       ; return (DataFamInstDecl { dfid_eqn = eqn' }, fvs) }

-- Renaming of the associated types in instances.

-- Rename associated type family decl in class
rnATDecls :: Name      -- Class
          -> [Name]    -- Class variables. See Note [Class variables and filterInScope] in GHC.Rename.HsType
          -> [LFamilyDecl GhcPs]
          -> RnM ([LFamilyDecl GhcRn], FreeNames)
rnATDecls cls cls_tvs at_decls
  = rnList (rnFamDecl (Just (cls, cls_tvs))) at_decls

rnATInstDecls :: (AssocTyFamInfo ->           -- The function that renames
                  decl GhcPs ->               -- an instance. rnTyFamInstDecl
                  RnM (decl GhcRn, FreeNames)) -- or rnDataFamInstDecl
              -> Name      -- Class
              -> [Name]
              -> [LocatedA (decl GhcPs)]
              -> RnM ([LocatedA (decl GhcRn)], FreeNames)
-- Used for data and type family defaults in a class decl
-- and the family instance declarations in an instance
--
-- NB: We allow duplicate associated-type decls;
--     See Note [Associated type instances] in GHC.Tc.TyCl.Instance
rnATInstDecls rnFun cls tv_ns at_insts
  = rnList (rnFun (AssocTyFamInst cls tv_ns)) at_insts
    -- See Note [Renaming associated types]

{- Note [Wildcards in family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Wild cards can be used in type/data family instance declarations to indicate
that the name of a type variable doesn't matter. Each wild card will be
replaced with a new unique type variable. For instance:

    type family F a b :: *
    type instance F Int _ = Int

is the same as

    type family F a b :: *
    type instance F Int b = Int

This is implemented as follows: Unnamed wildcards remain unchanged after
the renamer, and then given fresh meta-variables during typechecking, and
it is handled pretty much the same way as the ones in partial type signatures.
We however don't want to emit hole constraints on wildcards in family
instances, so we turn on PartialTypeSignatures and turn off warning flag to
let typechecker know this.
See related Note [Wildcards in visible kind application] in GHC.Tc.Gen.HsType

Note [Unused type variables in family instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the flag -fwarn-unused-type-patterns is on, the compiler reports
warnings about unused type variables in type-family instances. A
tpye variable is considered used (i.e. cannot be turned into a wildcard)
when

 * it occurs on the RHS of the family instance
   e.g.   type instance F a b = a    -- a is used on the RHS

 * it occurs multiple times in the patterns on the LHS
   e.g.   type instance F a a = Int  -- a appears more than once on LHS

 * it is one of the instance-decl variables, for associated types
   e.g.   instance C (a,b) where
            type T (a,b) = a
   Here the type pattern in the type instance must be the same as that
   for the class instance, so
            type T (a,_) = a
   would be rejected.  So we should not complain about an unused variable b

As usual, the warnings are not reported for type variables with names
beginning with an underscore.

Extra-constraints wild cards are not supported in type/data family
instance declarations.

Relevant tickets: #3699, #10586, #10982 and #11451.

Note [Renaming associated types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When renaming a type/data family instance, be it top-level or associated with
a class, we must check that all of the type variables mentioned on the RHS are
properly scoped. Specifically, the rule is this:

  Every variable mentioned on the RHS of a type instance declaration
  (whether associated or not) must be mentioned on the LHS

Here is a simple example of something we should reject:

  class C a b where
    type F a x
  instance C Int Bool where
    type F Int x = z

Here, `z` is mentioned on the RHS of the associated instance without being
mentioned on the LHS. The renamer will reject `z` as being out of scope without much fuss.

Things get slightly trickier when the instance header itself binds type
variables. Consider this example (adapted from #5515):

   instance C (p,q) z where
      type F (p,q) x = (x, z)

According to the rule above, this instance is improperly scoped. However, due
to the way GHC's renamer works, `z` is /technically/ in scope, as GHC will
always bring type variables from an instance header into scope over the
associated type family instances. As a result, the renamer won't simply reject
the `z` as being out of scope (like it would for the `type F Int x = z`
example) unless further action is taken. It is important to reject this sort of
thing in the renamer, because if it is allowed to make it through to the
typechecker, unexpected shenanigans can occur (see #18021 for examples).

To prevent these sorts of shenanigans, we reject programs like the one above
with an extra validity check in rnFamEqn. For each type variable bound in the
parent instance head, we check if it is mentioned on the RHS of the associated
family instance but not bound on the LHS. If any of the instance-head-bound
variables meet these criteria, we throw an error.
(See rnFamEqn.improperly_scoped for how this is implemented.)

Some additional wrinkles:

* This Note only applies to *instance* declarations.  In *class* declarations
  there is no RHS to worry about, and the class variables can all be in scope
  (#5862):

    class Category (x :: k -> k -> *) where
      type Ob x :: k -> Constraint
      id :: Ob x a => x a a
      (.) :: (Ob x a, Ob x b, Ob x c) => x b c -> x a b -> x a c

  Here 'k' is in scope in the kind signature, just like 'x'.

* Although type family equations can bind type variables with explicit foralls,
  it need not be the case that all variables that appear on the RHS must be
  bound by a forall. For instance, the following is acceptable:

    class C4 a where
      type T4 a b
    instance C4 (Maybe a) where
      type forall b. T4 (Maybe a) b = Either a b

  Even though `a` is not bound by the forall, this is still accepted because `a`
  was previously bound by the `instance C4 (Maybe a)` part. (see #16116).

* In addition to the validity check in rnFamEqn.improperly_scoped, there is an
  additional check in GHC.Tc.Validity.checkFamPatBinders that checks each family
  instance equation for type variables used on the RHS but not bound on the
  LHS. This is not made redundant by rmFamEqn.improperly_scoped, as there are
  programs that each check will reject that the other check will not catch:

  - checkValidFamPats is used on all forms of family instances, whereas
    rmFamEqn.improperly_scoped only checks associated family instances. Since
    checkFamPatBinders occurs after typechecking, it can catch programs that
    introduce dodgy scoping by way of type synonyms (see #7536), which is
    impractical to accomplish in the renamer.
  - rnFamEqn.improperly_scoped catches some programs that, if allowed to escape
    the renamer, would accidentally be accepted by the typechecker. Here is one
    such program (#18021):

      class C5 a where
        data family D a

      instance forall a. C5 Int where
        data instance D Int = MkD a

    If this is not rejected in the renamer, the typechecker would treat this
    program as though the `a` were existentially quantified, like so:

      data instance D Int = forall a. MkD a

    This is likely not what the user intended!

    Here is another such program (#9574):

      class Funct f where
        type Codomain f
      instance Funct ('KProxy :: KProxy o) where
        type Codomain 'KProxy = NatTr (Proxy :: o -> Type)

    Where:

      data Proxy (a :: k) = Proxy
      data KProxy (t :: Type) = KProxy
      data NatTr (c :: o -> Type)

    Note that the `o` in the `Codomain 'KProxy` instance should be considered
    improperly scoped. It does not meet the criteria for being explicitly
    quantified, as it is not mentioned by name on the LHS.
    However, `o` /is/ bound by the instance header, so if this
    program is not rejected by the renamer, the typechecker would treat it as
    though you had written this:

      instance Funct ('KProxy :: KProxy o) where
        type Codomain ('KProxy @o) = NatTr (Proxy :: o -> Type)

    Although this is a valid program, it's probably a stretch too far to turn
    `type Codomain 'KProxy = ...` into `type Codomain ('KProxy @o) = ...` here.
    If the user really wants the latter, it is simple enough to communicate
    their intent by mentioning `o` on the LHS by name.

* Historical note: Previously we had to add type variables from the outermost
  kind signature on the RHS to the scope of associated type family instance,
  i.e. GHC did implicit quantification over them. But now that we implement
  GHC Proposal #425 "Invisible binders in type declarations"
  we don't need to do this anymore.

Note [Type family equations and occurrences]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In most data/type family equations, the type family name used in the equation
is treated as an occurrence. For example:

  module A where
    type family F a

  module B () where
    import B (F)
    type instance F Int = Bool

We do not want to warn about `F` being unused in the module `B`, as the
instance constitutes a use site for `F`. The exception to this rule is closed
type families, whose equations constitute a definition, not occurrences. For
example:

  module C () where
    type family CF a where
      CF Char = Float

Here, we /do/ want to warn that `CF` is unused in the module `C`, as it is
defined but not used (#18470).

GHC accomplishes this in rnFamEqn when determining the set of free
variables to return at the end. If renaming a data family or open type family
equation, we add the name of the type family constructor to the set of returned
free variables to ensure that the name is marked as an occurrence. If renaming
a closed type family equation, we avoid adding the type family constructor name
to the free variables. This is quite simple, but it is not a perfect solution.
Consider this example:

  module X () where
    type family F a where
      F Int = Bool
      F Double = F Int

At present, GHC will treat any use of a type family constructor on the RHS of a
type family equation as an occurrence. Since `F` is used on the RHS of the
second equation of `F`, it is treated as an occurrence, causing `F` not to be
warned about. This is not ideal, since `F` isn't exported—it really /should/
cause a warning to be emitted. There is some discussion in #10089/#12920 about
how this limitation might be overcome, but until then, we stick to the
simplistic solution above, as it fixes the egregious bug in #18470.
-}


{-
*********************************************************
*                                                      *
\subsection{Stand-alone deriving declarations}
*                                                      *
*********************************************************
-}

rnSrcDerivDecl :: DerivDecl GhcPs -> RnM (DerivDecl GhcRn, FreeNames)
rnSrcDerivDecl (DerivDecl (inst_warn_ps, ann) ty mds overlap)
  = do { standalone_deriv_ok <- xoptM LangExt.StandaloneDeriving
       ; unless standalone_deriv_ok (addErr TcRnUnexpectedStandaloneDerivingDecl)
       ; checkInferredVars ctxt nowc_ty
       ; (mds', ty', fvs) <- rnLDerivStrategy ctxt mds $ rnHsSigWcType ctxt ty
         -- Check if there are any nested `forall`s or contexts, which are
         -- illegal in the type of an instance declaration (see
         -- Note [No nested foralls or contexts in instance types] in
         -- GHC.Hs.Type).
       ; addNoNestedForallsContextsErr ctxt
           NFC_StandaloneDerivedInstanceHead
           (getLHsInstDeclHead $ dropWildCards ty')
       ; inst_warn_rn <- mapM rnLWarningTxt inst_warn_ps
       ; let overlap' = rnOverlapMode overlap
       ; return (DerivDecl (inst_warn_rn, ann) ty' mds' overlap', fvs) }
  where
    ctxt    = DerivDeclCtx
    nowc_ty = dropWildCards ty

{-
*********************************************************
*                                                      *
\subsection{Rules}
*                                                      *
*********************************************************
-}

rnHsRuleDecls :: RuleDecls GhcPs -> RnM (RuleDecls GhcRn, FreeNames)
rnHsRuleDecls (HsRules { rds_ext = (_, src)
                       , rds_rules = rules })
  = do { (rn_rules,fvs) <- rnList rnHsRuleDecl rules
       ; return (HsRules { rds_ext = src
                         , rds_rules = rn_rules }, fvs) }

rnHsRuleDecl :: RuleDecl GhcPs -> RnM (RuleDecl GhcRn, FreeNames)
rnHsRuleDecl (HsRule { rd_ext   = (_, st)
                     , rd_name  = lrule_name@(L _ rule_name)
                     , rd_act   = act
                     , rd_bndrs = bndrs
                     , rd_lhs   = lhs
                     , rd_rhs   = rhs })
  = let rule_name_fs = mkFastStringShortText rule_name in
    bindRuleBndrs (RuleCtx rule_name_fs) bndrs $ \tm_names bndrs' ->
    do { (lhs', fv_lhs') <- rnLExpr lhs
       ; (rhs', fv_rhs') <- rnLExpr rhs
       ; checkValidRule rule_name_fs tm_names lhs' fv_lhs'
       ; return (HsRule { rd_ext   = (HsRuleRn fv_lhs' fv_rhs', st)
                        , rd_name  = lrule_name
                        , rd_act   = act
                        , rd_bndrs = bndrs'
                        , rd_lhs   = lhs'
                        , rd_rhs   = rhs' }
                , fv_lhs' `plusFN` fv_rhs') }

{-
Note [Rule LHS validity checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Check the shape of a rewrite rule LHS.  Currently we only allow
LHSs of the form @(f e1 .. en)@, where @f@ is not one of the
@forall@'d variables.

We used restrict the form of the 'ei' to prevent you writing rules
with LHSs with a complicated desugaring (and hence unlikely to match);
(e.g. a case expression is not allowed: too elaborate.)

But there are legitimate non-trivial args ei, like sections and
lambdas.  So it seems simpler not to check at all, and that is why
check_e is commented out.

Note [Parens on the LHS of a RULE]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You may think that no one would write

   {-# RULES "foo" (f True) = blah #-}

with the LHS wrapped in parens. But Template Haskell does (#24621)!
So we should accommodate them.
-}

checkValidRule :: RuleName -> [Name] -> LHsExpr GhcRn -> NameSet -> RnM ()
checkValidRule rule_name ids lhs' fv_lhs'
  = do  {       -- Check for the form of the LHS
          case (validRuleLhs ids lhs') of
                Nothing  -> return ()
                Just bad -> failWithTc (badRuleLhsErr rule_name lhs' bad)

                -- Check that LHS vars are all bound
        ; let bad_vars = [var | var <- ids, not (var `elemNameSet` fv_lhs')]
        ; mapM_ (addErr . TcRnUnusedVariableInRuleDecl rule_name) bad_vars }

validRuleLhs :: [Name] -> LHsExpr GhcRn -> Maybe (HsExpr GhcRn)
-- Nothing => OK
-- Just e  => Not ok, and e is the offending sub-expression
validRuleLhs foralls lhs
  = checkl lhs
  where
    checkl = check . unLoc

    check (OpApp _ e1 op e2)              = checkl op `mplus` checkl_e e1
                                                      `mplus` checkl_e e2
    check (HsApp _ e1 e2)                 = checkl e1 `mplus` checkl_e e2
    check (HsAppType _ e _)               = checkl e
    check (HsVar _ lv)
      | getName lv `notElem` foralls      = Nothing
    -- See Note [Parens on the LHS of a RULE]
    check (HsPar _ e)                     = checkl e
    check other                           = Just other  -- Failure

        -- Check an argument
    checkl_e _ = Nothing
    -- Was (check_e e); see Note [Rule LHS validity checking]

{-      Commented out; see Note [Rule LHS validity checking] above
    check_e (HsVar v)     = Nothing
    check_e (HsPar e)     = checkl_e e
    check_e (HsLit e)     = Nothing
    check_e (HsOverLit e) = Nothing

    check_e (OpApp e1 op _ e2)   = checkl_e e1 `mplus` checkl_e op `mplus` checkl_e e2
    check_e (HsApp e1 e2)        = checkl_e e1 `mplus` checkl_e e2
    check_e (NegApp e _)         = checkl_e e
    check_e (ExplicitList _ es)  = checkl_es es
    check_e other                = Just other   -- Fails

    checkl_es es = foldr (mplus . checkl_e) Nothing es
-}


badRuleLhsErr :: FastString -> LHsExpr GhcRn -> HsExpr GhcRn -> TcRnMessage
badRuleLhsErr name lhs bad_e
  = TcRnIllegalRuleLhs errReason name lhs bad_e
  where
    errReason = case bad_e of
      HsHole (HoleVar (L _ uv)) ->
        UnboundVariable uv $ notInScopeErr WL_Global uv
      _ -> IllegalExpression

{- **************************************************************
         *                                                      *
      Renaming type, class, instance and role declarations
*                                                               *
*****************************************************************

@rnTyDecl@ uses the `global name function' to create a new type
declaration in which local names have been replaced by their original
names, reporting any unknown names.

Renaming type variables is a pain. Because they now contain uniques,
it is necessary to pass in an association list which maps a parsed
tyvar to its @Name@ representation.
In some cases (type signatures of values),
it is even necessary to go over the type first
in order to get the set of tyvars used by it, make an assoc list,
and then go over it again to rename the tyvars!
However, we can also do some scoping checks at the same time.

Note [Dependency analysis of type and class decls]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A TyClGroup represents a strongly connected component of type/class/instance
decls, together with the role annotations and standalone kind signatures for the
type/class declarations. The renamer uses strongly connected component analysis
to build these groups. We do this for a number of reasons:

* Improve kind error messages. Consider

     data T f a = MkT f a
     data S f a = MkS f (T f a)

  This has a kind error, but the error message is better if you
  check T first, (fixing its kind) and *then* S.  If you do kind
  inference together, you might get an error reported in S, which
  is jolly confusing.  See #4875

* Increase kind polymorphism.  See GHC.Tc.TyCl
  Note [Grouping of type and class declarations]

What about instances? Based on a number of tickets (#12088, #12239, #14668,
#15561, #16410, #16448, #16693, #19611, #20875, #21172, #22257, #25238, #25834,
etc) we concluded that we cannot handle them at this stage.

It is not possible, by looking at the free variables of a declaration, to
determine which instances a declaration depends on; furthermore, it is not
possible to discover dependencies between instances, for the same reason.

Previously GHC inserted instances at the earliest positions where their FVs are
bound, but it only helped with a subset of tickets. The current approach is to
accept that the dependency analysis here is incomplete and recover in the kind
checker with a retrying mechanism. See Note [Retrying TyClGroups] in GHC.Tc.TyCl

------------------------------------
So much for why we want SCCs.  What about how and when we construct them?

First, an overview:
  (TCDEP1) Flatten TyClGroups from the parser
  (TCDEP2) Rename the type/class declarations, standalone kind signatures, role
           declarations, and instances individually
  (TCDEP3) Preprocess FVs and build a dependency graph
  (TCDEP4) Find strongly connected components (SCCs) of declarations
  (TCDEP5) Attach roles and kind signatures to the appropriate SCC
  (TCDEP6) Create one singleton "SCC" per instance and put them at the end

And now the deep dive:

(TCDEP1) We start with a `HsGroup GhcPs`, containing a `[TyClGroup GhcPs]`:
  a big pile of declarations. It is not important how the parser distributes
  declarations across those TyClGroups, as the first thing we do in `rnTyClDecls`
  is flatten them using a few helpers:

    tyClGroupTyClDecls = Data.List.concatMap group_tyclds
    tyClGroupInstDecls = Data.List.concatMap group_instds
    tyClGroupRoleDecls = Data.List.concatMap group_roles
    tyClGroupKindSigs  = Data.List.concatMap group_kisigs

  In practice, the parser just puts all declarations in a single `TyClGroup`,
  so the `concatMap` is a no-op.

(TCDEP2) Rename each declaration separately, yielding the following lists
  in `rnTyClDecls`:

    tycls_w_fvs  :: [(LTyClDecl GhcRn, FreeNames)]
    instds_w_fvs :: [(LInstDecl GhcRn, FreeNames)]
    kisigs_w_fvs :: [(LStandaloneKindSig GhcRn, FreeNames)]
    role_annots  :: [LRoleAnnotDecl GhcRn]

  The `FreeNames` are the free type/data constructors of the decl. For example:

    type family F (a :: k)        -- FVs: {}
    data X = MkX Char (Maybe X)   -- FVs: {Char, Maybe, X}
    data Y = MkY X (Maybe Y)      -- FVs: {Maybe, Y, X}
    type instance F MkX = X       -- FVs: {F, MkX, X}
    type instance F MkY = Int     -- FVs: {F, MkY, Int}

(TCDEP3) Build a graph where each node is a `TyClDecl` keyed by its name, and
  its `FreeNames` give rise to edges. Happens in `depAnalTyClDecls`. Examples:

    data A x = MkA x       -- node `A`, edges: {}
    data B x = MkB (A x)   -- node `B`, edges: {B -> A}
    data C = MkC (B C)     -- node `C`, edges: {C -> B, C -> C}

  The `FreeNames` are not used "as is" to create the edges. They first undergo a
  few transformations.

  (TCDEP3.fvs_kisig) If a standalone kind signature is present, add its free
    variables to those of the declaration. Consider:

      data A = MkA
      data B = MkB

      type P :: A -> Type           -- sig  FVs: {A, Type}
      data P x = MkP (Proxy MkB)    -- decl FVs: {Proxy, MkB}

    By adding the sig and decl FVs together, we get {A, Type, Proxy, MkB}.
    Then proceed to the next step.

  (TCDEP3.fvs_parent) Replace any mention of a (promoted) data constructor
    with its parent TyCon. Consider the FVs from the previous step:

      the name set {A, Type, Proxy, MkB}
      turns into   {A, Type, Proxy, B}

    MkB does not get its own node in the graph, so an edge to it must actually
    point to B.

  (TCDEP3.fvs_nogbl) Filter out references to type constructors outside this
    `HsGroup`. They just clutter things up:

      the name set {A, Type, Proxy, B}
      turns into   {A, B}

  Note [Prepare TyClGroup FVs] describes these transformations in more detail.
  Back to our `P` example, the final nodes and edges are as follows:

      data A = MkA    -- node `A`, edges: {}
      data B = MkB    -- node `B`, edges: {}

      type P :: A -> Type
      data P x = MkP (Proxy MkB)  -- node `P`, edges: {P -> A, P -> B}

(TCDEP4) Find strongly connected components (SCCs) of `TyClDecl`s.
  This happens immediately after building the dependency graph in
  `depAnalTyClDecls`. As the result, in `rnTyClDecls` we get

    tycl_sccs :: [SCC (LTyClDecl GhcRn, NameSet)]

  These SCCs are topologically sorted, but only according to lexical
  dependencies (i.e. dependencies that can be found by looking at the FVs).
  Non-lexical dependencies (i.e. dependencies on instances) are ignored because
  they can't be reliably found prior to type checking.

  More on that in Note [Retrying TyClGroups] in GHC.Tc.TyCl.

(TCDEP5) For each SCC, create a `TyClGroup GhcRn`. Standalone kind signatures
  and role annotations are looked up by name and included in the same
  `TyClGroup` as the corresponding type/class declarations.

  The extension field `group_ext` of `TyClGroup GhcRn` contains the dependencies
  of the SCC computed from FVs in step (TCDEP3), but /excluding/ the type
  constructors bound by the group itself. Example:

     -- TyClGroup: binds {Z}
     --            depends on {}
     data Z = MkZ      -- FVs: {}

     -- TyClGroup: binds {X, Y}
     --            depends on {Z} rather than {X, Y, Z}
     data X = MkX Y Z  -- FVs: {Y, Z}
     data Y = MkY X Z  -- FVs: {X, Z}

  Reason: `isReadyTyClGroup` in GHC.Tc.TyCl is a function that checks whether
  all of a TyClGroup's dependencies are present in the type checking env, and we
  wouldn't want it to consider a group to be "blocked" on its own declarations.

(TCDEP6) For each instance, create a singleton `TyClGroup GhcRn`, and put them
  all at the end, where their lexical dependencies are surely satisfied.
  More on that in Note [Put instances at the end].

  The `group_ext` field in an instance TyClGroup is set to the FVs of the
  instance, preprocessed much in the same way as declaration FVs in step
  (TCDEP3). See Note [Prepare TyClGroup FVs] for details.

Note [No dependencies on data instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this
   data family D a
   data instance D Int = D1
   data S = MkS (Proxy 'D1)

Here the declaration of S depends on the /data instance/ declaration
for 'D Int'.  That makes things a lot more complicated, especially
if the data instance is an associated type of an enclosing class instance.
(And the class instance might have several associated type instances
with different dependency structure!)

Ugh.  For now we simply don't allow promotion of data constructors for
data instances.  See Note [AFamDataCon: not promoting data family
constructors] in GHC.Tc.Utils.Env

Note [Put instances at the end]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is no point including type family instances or class instances in the
graph for SCC analysis because instances are not referred to by name. We cannot
discover, by looking at the free variables of a declaration, what instances it
depends on.

Instead, we create one singleton "SCC" per instance using mkInstGroups and put
them at the end. This is simple and guarantees that the FVs of all instances are
bound in the preceding TyClGroups.

The problem is that if there are any declarations that depend on instances,
they're going to fail kind checking, because instances come after the
declarations. To account for that, the kind checker goes through a multipass
ordeal described in Note [Retrying TyClGroups] in GHC.Tc.TyCl.

One might ask, "why not insert instances at the earliest positions where their
FVs are bound?" Indeed, this sounds like a plausible solution, and GHC used to
do it at one point. However, there are many tickets (and now test cases)
demonstrating its inadequacy: #12088, #12239, #14668, #15561, #16410, #16448,
#16693, #19611, #20875, #21172, #22257, #25238, #25834, etc.

As to why we put each instance in its own group, as opposed to using just one
group with all the instances, the reason is that an instance may depend on
another instance, so we better add them to the environment one by one.
-}


rnTyClDecls :: LiftedDecls -> OccEnv (OccName, Bool) -> [TyClGroup GhcPs]
            -> RnM ([TyClGroup GhcRn], FreeNames)
-- Rename the declarations and do dependency analysis on them
rnTyClDecls hidden scopes tycl_ds
  = do { -- Flatten TyClGroups and rename each declaration individually.
         -- Steps (TCDEP1) and (TCDEP2) of Note [Dependency analysis of type and class decls]
       ; tycls_w_fvs <- mapM (wrapLocFstMA (rnTyClDecl scopes)) (tyClGroupTyClDecls tycl_ds)
       ; let tc_names = mkNameSet (map (tcdName . unLoc . fst) tycls_w_fvs)
       ; traceRn "rnTyClDecls" $
           vcat [ text "tyClGroupTyClDecls:" <+> ppr tycls_w_fvs
                , text "tc_names:" <+> ppr tc_names ]
       ; kisigs_w_fvs <- rnStandaloneKindSignatures hidden tc_names (tyClGroupKindSigs tycl_ds)
       ; instds_w_fvs <- mapM (wrapLocFstMA rnSrcInstDecl) (tyClGroupInstDecls tycl_ds)
       ; role_annots  <- rnRoleAnnots tc_names (tyClGroupRoleDecls tycl_ds)

       -- Do SCC analysis on the type/class declarations.
       -- Steps (TCDEP3) and (TCDEP4) of Note [Dependency analysis of type and class decls]
       ; rdr_env <- getGlobalRdrEnv
       ; traceRn "rnTyClDecls SCC analysis" $
           vcat [ text "rdr_env:" <+> ppr rdr_env ]
       ; let tycl_sccs = depAnalTyClDecls hidden rdr_env tc_names kisig_fv_env tycls_w_fvs
             (kisig_env, kisig_fv_env) = mkKindSig_fv_env kisigs_w_fvs
             role_annot_env = mkRoleAnnotEnv role_annots

       -- Construct renamed TyClGroups.
       -- Steps (TCDEP5) and (TCDEP6) of Note [Dependency analysis of type and class decls]
       ; let tycl_groups = map (mk_group role_annot_env kisig_env) tycl_sccs
             inst_groups = mkInstGroups rdr_env tc_names instds_w_fvs
             all_groups  = tycl_groups ++ inst_groups

       ; traceRn "rnTycl dependency analysis made groups" (ppr all_groups)

       ; let all_fvs = foldr (plusFN . snd) emptyFNs tycls_w_fvs  `plusFN`
                       foldr (plusFN . snd) emptyFNs instds_w_fvs `plusFN`
                       foldr (plusFN . snd) emptyFNs kisigs_w_fvs

       ; return (all_groups, all_fvs) }
  where
    -- Construct a type/class declaration TyClGroup.
    -- Step (TCDEP5) of Note [Dependency analysis of type and class decls]
    mk_group :: RoleAnnotEnv
             -> KindSigEnv
             -> SCC (LTyClDecl GhcRn, NameSet)
             -> TyClGroup GhcRn
    mk_group role_env kisig_env scc = group
      where
        (tycl_ds, nodes_deps) = unzip (flattenSCC scc)
        node_bndrs = map (tcdName . unLoc) tycl_ds
        deps = delFNs node_bndrs (plusFNs nodes_deps)
          -- (delFNs node_bndrs) removes the self-references.
          -- See Note [Prepare TyClGroup FVs]
        group = TyClGroup { group_ext    = deps
                          , group_tyclds = tycl_ds
                          , group_kisigs = getKindSigs node_bndrs kisig_env
                          , group_roles  = getRoleAnnots node_bndrs role_env
                          , group_instds = [] }

-- | Free variables of standalone kind signatures.
newtype KindSig_FV_Env = KindSig_FV_Env (NameEnv FreeNames)

lookupKindSig_FV_Env :: KindSig_FV_Env -> Name -> FreeNames
lookupKindSig_FV_Env (KindSig_FV_Env e) name
  = fromMaybe emptyFNs (lookupNameEnv e name)

-- | Standalone kind signatures.
type KindSigEnv = NameEnv (LStandaloneKindSig GhcRn)

mkKindSig_fv_env :: [(LStandaloneKindSig GhcRn, FreeNames)] -> (KindSigEnv, KindSig_FV_Env)
mkKindSig_fv_env kisigs_w_fvs = (kisig_env, kisig_fv_env)
  where
    kisig_env = mapNameEnv fst compound_env
    kisig_fv_env = KindSig_FV_Env (mapNameEnv snd compound_env)
    compound_env :: NameEnv (LStandaloneKindSig GhcRn, FreeNames)
      = mkNameEnvWith (standaloneKindSigName . unLoc . fst) kisigs_w_fvs

getKindSigs :: [Name] -> KindSigEnv -> [LStandaloneKindSig GhcRn]
getKindSigs bndrs kisig_env = mapMaybe (lookupNameEnv kisig_env) bndrs

rnStandaloneKindSignatures
  :: LiftedDecls -> NameSet  -- names of types and classes in the current TyClGroup
  -> [LStandaloneKindSig GhcPs]
  -> RnM [(LStandaloneKindSig GhcRn, FreeNames)]
rnStandaloneKindSignatures hidden tc_names kisigs
  = do { let (no_dups, dup_kisigs) = removeDupsOn get_name kisigs
             get_name = standaloneKindSigName . unLoc
       ; mapM_ dupKindSig_Err dup_kisigs
       ; mapM (wrapLocFstMA (rnStandaloneKindSignature hidden tc_names)) no_dups
       }

rnStandaloneKindSignature
  :: LiftedDecls -> NameSet  -- names of types and classes in the current TyClGroup
  -> StandaloneKindSig GhcPs
  -> RnM (StandaloneKindSig GhcRn, FreeNames)
rnStandaloneKindSignature hidden tc_names (StandaloneKindSig _ v ki)
  = do  { standalone_ki_sig_ok <- xoptM LangExt.StandaloneKindSignatures
        ; unless (standalone_ki_sig_ok || rdrNameOcc (unLoc v) `elem` map fst hidden) $
            addErr TcRnUnexpectedStandaloneKindSig
        ; new_v <- lookupSigCtxtOccRn (TopSigCtxt tc_names) SigLikeStandaloneKindSig v
        ; (new_ki, fvs) <- rnHsSigType (StandaloneKindSigCtx v) KindLevel ki
        ; return (StandaloneKindSig noExtField new_v new_ki, fvs)
        }

-- See Note [Dependency analysis of type and class decls]
depAnalTyClDecls :: LiftedDecls -> GlobalRdrEnv
                 -> NameSet                         -- Names in the current HsGroup
                 -> KindSig_FV_Env                  -- FVs of standalone kind signatures
                 -> [(LTyClDecl GhcRn, FreeNames)]
                 -> [SCC (LTyClDecl GhcRn, NameSet)]
depAnalTyClDecls hidden rdr_env tc_names kisig_fv_env ds_w_fvs
  = stronglyConnCompFromEdgedVerticesUniq edges
  where
    lifted_owners = mkOccEnv hidden
    names_by_occ = mkOccEnv [(nameOccName name, name) | name <- nonDetEltsUniqSet tc_names]
    -- The application supplies an edge from the owner to the lifted family.
    -- Add the reverse edge when they need to share kind inference.
    owner_edge name = mkNameSet
      [owner | Just occ <- maybeToList (lookupOccEnv lifted_owners (nameOccName name))
             , owner <- maybeToList (lookupOccEnv names_by_occ occ)]
    -- Build a graph where each node is a `TyClDecl` keyed by its name, and
    -- its `FreeNames` give rise to edges.
    -- Step (TCDEP3) of Note [Dependency analysis of type and class decls]
    edges :: [ Node Name (LTyClDecl GhcRn, NameSet) ]
    edges = [ DigraphNode (d, deps) name (nonDetEltsUniqSet deps)
              -- nonDetEltsUniqSet is OK to use here because the order of the edges
              -- has no effect on the result of stronglyConnCompFromEdgedVerticesUniq.
              -- See Note [Deterministic SCC] in GHC.Data.Graph.Directed.

            | (d, fvs) <- ds_w_fvs,
              let { name = tcdName (unLoc d)
                  ; kisig_fvs = lookupKindSig_FV_Env kisig_fv_env name
                  ; node_fvs = fvs `plusFN` kisig_fvs `plusFN` owner_edge name -- (TCDEP3.fvs_kisig)
                  ; deps = toParents rdr_env node_fvs `intersectFNs` tc_names
                      -- (toParents rdr_env) maps datacons to parent tycons (TCDEP3.fvs_parent),
                      -- (`intersectFNs` tc_names) filters out imported names (TCDEP3.fvs_nogbl).
                      -- See also Note [Prepare TyClGroup FVs]
                  }
            ]

toParents :: GlobalRdrEnv -> NameSet -> NameSet
toParents rdr_env ns
  = nonDetStrictFoldUniqSet add emptyNameSet ns
  -- It's OK to use a non-deterministic fold because we immediately forget the
  -- ordering by creating a set
  where
    add n s = extendNameSet s (getParent rdr_env n)

getParent :: GlobalRdrEnv -> Name -> Name
getParent rdr_env n
  = case lookupGRE_Name rdr_env n of
      Just gre -> case greParent gre of
                    ParentIs  { par_is = p } -> p
                    _                        -> n
      Nothing -> n

{- Note [Prepare TyClGroup FVs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The renamer returns, alongside each renamed type/class declaration or instance,
the set of its free variables:

  rnTyClDecl    :: TyClDecl GhcPs -> RnM (TyClDecl GhcRn, FreeNames)
  rnSrcInstDecl :: InstDecl GhcPs -> RnM (InstDecl GhcRn, FreeNames)

For example:

  type family F (a :: k)        -- FVs: {}
  data X = MkX Char (Maybe X)   -- FVs: {Char, Maybe, X}
  data Y = MkY X (Maybe Y)      -- FVs: {Maybe, Y, X}
  type instance F MkX = X       -- FVs: {F, MkX, X}
  type instance F MkY = Int     -- FVs: {F, MkY, Int}

This is almost what we need for dependency analysis (i.e. to figure out in which
order to check the declarations), but first we apply a few transformations:

* toParents rdr_env inst_fvs `intersectFVs` tc_names   -- in mkInstGroups
* toParents rdr_env node_fvs `intersectFVs` tc_names   -- in depAnalTyClDecls
* delFVs node_bndrs (plusFVs nodes_deps)               -- in rnTyClGroups

The cited lines of code are somewhat far apart; to see the big picture, refer
to Note [Dependency analysis of type and class decls], and more specifically
steps TCDEP3, TCDEP5, and TCDEP6.

The cumulative effect of these transformations is as follows:

* Data constructor names are replaced by the parent type constructors:
    {MkX} ==> {X}
  Part of the code:
    toParents rdr_env
  Reason:
    MkX does not get its own node in the dependency graph,
    so we cannot make an edge to it in `depAnalTyClDecls`

* Names defined outside the current HsGroup are filtered out:
    {Char,Maybe} ==> {}
  Part of the code:
    `intersectFVs` tc_names
  Reason:
    makes the NameSet smaller, so there are fewer subsequent lookups
    in `graphFromEdgedVertices` (GHC.Data.Graph.Directed)
    and in `isReadyTyClGroup`   (GHC.Tc.TyCl)

* Self-references are removed:
    {A,B,C} ==> {C}, iff in the TyClGroup that defines {A,B}
  Part of the code:
    delFVs node_bndrs
  Reason:
    avoids the problem that `isReadyTyClGroup` would otherwise consider
    a recursive TyClGroup to be blocked on itself
  Note:
    needs to happen after the SCC analysis because of
    mutually-recursive data types

These "prepared" FVs are the lexical dependencies of a TyClGroup that are stored
in the XCTyClGroup extension field:

  type family F (a :: k)        -- XCTyClGroup: {}
  data X = MkX Char (Maybe X)   -- XCTyClGroup: {}
  data Y = MkY X (Maybe Y)      -- XCTyClGroup: {X}
  type instance F MkX = X       -- XCTyClGroup: {F, X}
  type instance F MkY = Int     -- XCTyClGroup: {F, Y}

Before attempting to kind-check a TyClGroup, all of its lexical dependencies
need to be satisfied, i.e. there must be a TyThing in the TypeEnv for each of
these names.

Imported names {Char,Maybe} don't need to be explicitly included in the set of
lexical dependencies because `isReadyTyClGroup` can safely assume those are
definitely already in the env.
-}


{- ******************************************************
*                                                       *
       Role annotations
*                                                       *
****************************************************** -}

-- | Renames role annotations, returning them as the values in a NameEnv
-- and checks for duplicate role annotations.
-- It is quite convenient to do both of these in the same place.
-- See also Note [Role annotations in the renamer]
rnRoleAnnots :: NameSet
             -> [LRoleAnnotDecl GhcPs]
             -> RnM [LRoleAnnotDecl GhcRn]
rnRoleAnnots tc_names role_annots
  = do {  -- Check for duplicates *before* renaming, to avoid
          -- lumping together all the unboundNames
         let (no_dups, dup_annots) = removeDupsOn get_name role_annots
             get_name = roleAnnotDeclName . unLoc
       ; mapM_ dupRoleAnnotErr dup_annots
       ; mapM (wrapLocMA rn_role_annot1) no_dups }
  where
    rn_role_annot1 (RoleAnnotDecl _ tycon roles)
      = do {  -- the name is an *occurrence*, but look it up only in the
              -- decls defined in this group (see #10263)
             tycon' <- lookupSigCtxtOccRn (RoleAnnotCtxt tc_names)
                                          SigLikeRoleAnnotation
                                          tycon
           ; return $ RoleAnnotDecl noExtField tycon' roles }

dupRoleAnnotErr :: NonEmpty (LRoleAnnotDecl GhcPs) -> RnM ()
dupRoleAnnotErr list@(L loc _ :| _)
  = addErrAt (locA loc) (TcRnDuplicateRoleAnnot list)

dupKindSig_Err :: NonEmpty (LStandaloneKindSig GhcPs) -> RnM ()
dupKindSig_Err list@(L loc _ :| _)
  = addErrAt (locA loc) (TcRnDuplicateKindSig list)

{- Note [Role annotations in the renamer]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We must ensure that a type's role annotation is put in the same group as the
proper type declaration. This is because role annotations are needed during
type-checking when creating the type's TyCon. So, rnRoleAnnots builds a
NameEnv (LRoleAnnotDecl Name) that maps a name to a role annotation for that
type, if any. Then, this map can be used to add the role annotations to the
groups after dependency analysis.

This process checks for duplicate role annotations, where we must be careful
to do the check *before* renaming to avoid calling all unbound names duplicates
of one another.

The renaming process, as usual, might identify and report errors for unbound
names. This is done by using lookupSigCtxtOccRn in rnRoleAnnots (using
lookupGlobalOccRn led to #8485).
-}


{- ******************************************************
*                                                       *
       Dependency info for instances
*                                                       *
****************************************************** -}

----------------------------------------------------------
-- | Construct a @TyClGroup@ for each @InstDecl@.
-- Step (TCDEP6) of Note [Dependency analysis of type and class decls]
mkInstGroups :: GlobalRdrEnv
             -> NameSet
             -> [(LInstDecl GhcRn, FreeNames)]
             -> [TyClGroup GhcRn]
mkInstGroups rdr_env tc_names inst_ds_fvs
  = [ mk_inst_group deps inst_decl
    | (inst_decl, inst_fvs) <- inst_ds_fvs
    , let deps = toParents rdr_env inst_fvs `intersectFNs` tc_names ]
            -- (toParents rdr_env) maps datacons to parent tycons,
            -- (`intersectFNs` tc_names) filters out imported names.
            -- See Note [Prepare TyClGroup FVs]
  where
    mk_inst_group :: NameSet -> LInstDecl GhcRn -> TyClGroup GhcRn
    mk_inst_group deps inst_d =
      TyClGroup { group_ext = deps
                , group_tyclds = []
                , group_kisigs = []
                , group_roles  = []
                , group_instds = [inst_d] }

{- ******************************************************
*                                                       *
         Renaming a type or class declaration
*                                                       *
****************************************************** -}

rnTyClDecl :: OccEnv (OccName, Bool) -> TyClDecl GhcPs
           -> RnM (TyClDecl GhcRn, FreeNames)

-- All flavours of top-level type family declarations ("type family", "newtype
-- family", and "data family")
rnTyClDecl scopes (FamDecl { tcdFam = fam })
  = do { (fam', fvs) <- rnFamDecl Nothing fam
       ; info <- case fdInfo fam' of
           ClosedTypeFamily (anns, _) equations
             | Just (root, is_sum) <- lookupOccEnv scopes (rdrNameOcc (unLoc (fdLName fam))) -> do
                 owner <- getName <$> lookupTopBndrRn WL_TyCon (mkRdrUnqual root)
                 pure (ClosedTypeFamily (anns, Just (owner, is_sum)) equations)
           ordinary -> pure ordinary
       ; return (FamDecl noExtField fam' { fdInfo = info }, fvs) }

rnTyClDecl _ (SynDecl { tcdLName = tycon, tcdTyVars = tyvars,
                      tcdFixity = fixity, tcdRhs = rhs })
  = do { tycon' <- lookupLocatedTopBndrRnN WL_TyCon tycon
       ; let kvs = extractHsTyRdrTyVarsKindVars rhs
             doc = TySynCtx tycon
       ; traceRn "rntycl-ty" (ppr tycon <+> ppr kvs)
       ; bindHsQTyVars doc Nothing kvs tyvars $ \ tyvars' free_rhs_kvs ->
    do { mapM_ warn_implicit_kvs (nubL free_rhs_kvs)
       ; (rhs', fvs) <- rnTySyn doc rhs
       ; return (SynDecl { tcdLName = tycon', tcdTyVars = tyvars'
                         , tcdFixity = fixity
                         , tcdRhs = rhs', tcdSExt = fvs }, fvs) } }
  where
    warn_implicit_kvs :: LocatedN RdrName -> RnM ()
    warn_implicit_kvs kv =
      addDiagnosticAt (getLocA kv) (TcRnImplicitRhsQuantification kv)

-- "data", "newtype" declarations
rnTyClDecl _ (DataDecl
    { tcdLName = tycon, tcdTyVars = tyvars,
      tcdFixity = fixity,
      tcdDataDefn = defn@HsDataDefn{ dd_cons = cons, dd_kindSig = kind_sig},
      tcdModifiers = mods })
  = do { tycon' <- lookupLocatedTopBndrRnN WL_TyCon tycon
       ; let kvs = extractDataDefnKindVars defn
             doc = TyDataCtx tycon
             new_or_data = dataDefnConsNewOrData cons
       ; traceRn "rntycl-data" (ppr tycon <+> ppr kvs)
       ; bindHsQTyVars doc Nothing kvs tyvars $ \ tyvars' free_rhs_kvs ->
    do { (defn', fvs) <- rnDataDefn doc defn
       ; cusk <- data_decl_has_cusk tyvars' new_or_data (null free_rhs_kvs) kind_sig
       ; let rn_info = DataDeclRn { tcdDataCusk = cusk
                                  , tcdFVs      = fvs }
       ; traceRn "rndata" (ppr tycon <+> ppr cusk <+> ppr free_rhs_kvs)
       ; (mods', mods_fvs) <- rnModifiersContextAndWarn doc mods
       ; return (DataDecl { tcdLName    = tycon'
                          , tcdTyVars   = tyvars'
                          , tcdFixity   = fixity
                          , tcdDataDefn = defn'
                          , tcdDExt     = rn_info
                          , tcdModifiers = mods' }, fvs `plusFN` mods_fvs) } }

rnTyClDecl _ (ClassDecl { tcdCtxt = context, tcdLName = lcls,
                        tcdTyVars = tyvars, tcdFixity = fixity,
                        tcdFDs = fds,
                        tcdDecls = decls,
                        tcdModifiers = modifiers})
  = do  { let HsNestedGroup { ng_meths = mbinds, ng_sigs = sigs, ng_ats = ats
                            , ng_tyfam_insts = at_defs, ng_docs = docs} = partitionBindsAndSigs decls
        ; lcls' <- lookupLocatedTopBndrRnN WL_TyCon lcls
        ; let cls' = unLoc lcls'
              kvs = []  -- No scoped kind vars except those in
                        -- kind signatures on the tyvars

        -- Tyvars scope over superclass context and method signatures
        ; ((tyvars', context', fds', ats'), stuff_fvs)
            <- bindHsQTyVars cls_doc Nothing kvs tyvars $ \ tyvars' _ -> do
                  -- Checks for distinct tyvars
             { (context', cxt_fvs) <- rnMaybeContext cls_doc context
             ; fds'  <- rnFds fds
                         -- The fundeps have no free variables
             ; (ats', fv_ats) <- rnATDecls cls' (hsAllLTyVarNames tyvars') ats
             ; let fvs = cxt_fvs     `plusFN`
                         fv_ats
             ; return ((tyvars', context', fds', ats'), fvs) }

        ; (at_defs', fv_at_defs) <- rnList (rnTyFamDefltDecl cls') at_defs

        -- No need to check for duplicate associated type decls
        -- since that is done by GHC.Rename.Names.extendGlobalRdrEnvRn

        -- Check the signatures
        -- First process the class op sigs (op_sigs), then the fixity sigs (non_op_sigs).
        ; let sig_rdr_names_w_locs =
                [op | L _ (ClassOpSig _ False ops _) <- sigs
                    , op <- ops]
        ; checkDupRdrNames sig_rdr_names_w_locs
                -- Typechecker is responsible for checking that we only
                -- give default-method bindings for things in this class.
                -- The renamer *could* check this for class decls, but can't
                -- for instance decls.

        -- The newLocals call is tiresome: given a generic class decl
        --      class C a where
        --        op :: a -> a
        --        op {| x+y |} (Inl a) = ...
        --        op {| x+y |} (Inr b) = ...
        --        op {| a*b |} (a*b)   = ...
        -- we want to name both "x" tyvars with the same unique, so that they are
        -- easy to group together in the typechecker.
        ; (mbinds', sigs', meth_fvs)
            <- rnMethodBinds True cls' (hsAllLTyVarNames tyvars') mbinds sigs
                -- No need to check for duplicate method signatures
                -- since that is done by GHC.Rename.Names.extendGlobalRdrEnvRn
                -- and the methods are already in scope

        ; (modifiers', mods_fvs) <- rnModifiersContextAndWarn cls_doc modifiers
        ; let all_fvs = meth_fvs `plusFN` stuff_fvs `plusFN` fv_at_defs `plusFN` mods_fvs
        ; docs' <- traverse rnLDocDecl docs
        ; return (ClassDecl { tcdCtxt = context', tcdLName = lcls',
                              tcdTyVars = tyvars', tcdFixity = fixity,
                              tcdFDs = fds',
                              tcdDecls = [], -- See Note [Pass-sensitive decls for ClassDecls/ClsInstDecls]
                              tcdCExt = (HsNestedGroup { ng_sigs          = sigs',
                                                         ng_meths         = mbinds',
                                                         ng_ats           = ats',
                                                         ng_tyfam_insts   = at_defs',
                                                         ng_docs          = docs',
                                                         ng_datafam_insts = [] -- Not used in a ClassDecl
                                                         },
                              all_fvs), tcdModifiers = modifiers' },
                  all_fvs ) }
  where
    cls_doc  = ClassDeclCtx lcls

-- Does the data type declaration include a CUSK?
data_decl_has_cusk :: LHsQTyVars (GhcPass p) -> NewOrData -> Bool -> Maybe (LHsKind (GhcPass p')) -> RnM Bool
data_decl_has_cusk tyvars new_or_data no_rhs_kvs kind_sig = do
  { -- See Note [Unlifted Newtypes and CUSKs], and for a broader
    -- picture, see Note [Implementation of UnliftedNewtypes].
  ; unlifted_newtypes <- xoptM LangExt.UnliftedNewtypes
  ; let non_cusk_newtype
          | NewType <- new_or_data =
              unlifted_newtypes && isNothing kind_sig
          | otherwise = False
    -- See Note [CUSKs: complete user-supplied kind signatures] in GHC.Hs.Decls
  ; return $ hsTvbAllKinded tyvars && no_rhs_kvs && not non_cusk_newtype
  }

{- Note [Unlifted Newtypes and CUSKs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When unlifted newtypes are enabled, a newtype must have a kind signature
in order to be considered have a CUSK. This is because the flow of
kind inference works differently. Consider:

  newtype Foo = FooC Int

When UnliftedNewtypes is disabled, we decide that Foo has kind
`TYPE 'LiftedRep` without looking inside the data constructor. So, we
can say that Foo has a CUSK. However, when UnliftedNewtypes is enabled,
we fill in the kind of Foo as a metavar that gets solved by unification
with the kind of the field inside FooC (that is, Int, whose kind is
`TYPE 'LiftedRep`). But since we have to look inside the data constructors
to figure out the kind signature of Foo, it does not have a CUSK.

See Note [Implementation of UnliftedNewtypes] for where this fits in to
the broader picture of UnliftedNewtypes.
-}

-- "type" and "type instance" declarations
rnTySyn :: HsDocContext -> LHsType GhcPs -> RnM (LHsType GhcRn, FreeNames)
rnTySyn doc rhs = rnLHsType doc rhs

rnDataDefn :: HsDocContext -> HsDataDefn GhcPs
           -> RnM (HsDataDefn GhcRn, FreeNames)
rnDataDefn doc (HsDataDefn { dd_cType = cType, dd_ctxt = context, dd_cons = condecls
                           , dd_kindSig = m_sig, dd_derivs = derivs })
  = do  { -- DatatypeContexts (i.e., stupid contexts) can't be combined with
          -- GADT syntax. See Note [The stupid context] in GHC.Core.DataCon.
          checkTc (h98_style || null (hsc_ctxt $ fromMaybeContext context))
                  (TcRnStupidThetaInGadt doc)

        -- Check restrictions on "type data" declarations.
        -- See Note [Type data declarations].
        ; when (isTypeDataDefnCons condecls) check_type_data

        ; (m_sig', sig_fvs) <- case m_sig of
             Just sig -> first Just <$> rnLHsKind doc sig
             Nothing  -> return (Nothing, emptyFNs)
        ; (context', fvs1) <- rnMaybeContext doc context
        ; (derivs',  fvs3) <- rn_derivs derivs

        -- For the constructor declarations, drop the LocalRdrEnv
        -- in the GADT case, where the type variables in the declaration
        -- do not scope over the constructor signatures
        -- data T a where { T1 :: forall b. b-> b }
        ; let zap_lcl_env thing
                | h98_style = thing
                | otherwise = do
                    -- Start tracking again for the constructors' own binders.
                    warn_puns <- woptM Opt_WarnPunUses
                    let rdr_env | warn_puns = trackPunScopes emptyLocalRdrEnv
                                | otherwise = emptyLocalRdrEnv
                    setLocalRdrEnv rdr_env thing
        ; (condecls', con_fvs) <- zap_lcl_env $ rnConDecls condecls
           -- No need to check for duplicate constructor decls
           -- since that is done by GHC.Rename.Names.extendGlobalRdrEnvRn

        ; let all_fvs = fvs1 `plusFN` fvs3 `plusFN`
                        con_fvs `plusFN` sig_fvs
        ; return ( HsDataDefn { dd_ext = noAnn
                              , dd_cType = fmap rn_ctype <$> cType
                              , dd_ctxt = context'
                              , dd_kindSig = m_sig'
                              , dd_cons = condecls'
                              , dd_derivs = derivs' }
                 , all_fvs )
        }
  where
    rn_ctype :: CType GhcPs -> CType GhcRn
    rn_ctype (CType x y z) = CType x (renameHeader <$> y) z
    h98_style = not $ anyLConIsGadt condecls  -- Note [Stupid theta]

    rn_derivs ds
      = do { deriv_strats_ok <- xoptM LangExt.DerivingStrategies
           ; failIfTc (lengthExceeds ds 1 && not deriv_strats_ok)
               TcRnIllegalMultipleDerivClauses
           ; (ds', fvs) <- mapFvRn (rnLHsDerivingClause doc) ds
           ; return (ds', fvs) }

    -- Given a "type data" declaration, check that the TypeData extension
    -- is enabled and check restrictions (R1), (R2), (R3) and (R5)
    -- on the declaration.  See Note [Type data declarations].
    check_type_data
      = do { unlessXOptM LangExt.TypeData $ failWith TcRnIllegalTypeData
           ; unless (null (hsc_ctxt $ fromMaybeContext context)) $
               failWith $ TcRnTypeDataForbids TypeDataForbidsDatatypeContexts
           ; mapM_ (addLocM check_type_data_condecl) condecls
           ; unless (null derivs) $
               failWith $ TcRnTypeDataForbids TypeDataForbidsDerivingClauses
           }

    -- Check restrictions (R2) and (R3) on a "type data" constructor.
    -- See Note [Type data declarations].
    check_type_data_condecl :: ConDecl GhcPs -> RnM ()
    check_type_data_condecl condecl
      = do {
           ; when (has_labelled_fields condecl) $
               failWith $ TcRnTypeDataForbids TypeDataForbidsLabelledFields
           ; when (has_field_annotations condecl) $
               failWith $ TcRnTypeDataForbids TypeDataForbidsFieldAnnotations
           }

    has_labelled_fields (ConDeclGADT { con_g_args = RecConGADT _ _ }) = True
    has_labelled_fields (ConDeclH98 { con_args = RecCon _ flds })
      = not (null (unLoc flds))
    has_labelled_fields _ = False

    has_field_annotations condecl
      = not (all unannotated (con_arg_fields condecl))
      where
        unannotated (CDF { cdf_bang = NoSrcStrict
                         , cdf_unpack = NoSrcUnpack
                         , cdf_multiplicity = HsModifiedFunArr _ [] (HsStandardArr _) })
          = True
        unannotated _ = False

    con_arg_fields (ConDeclGADT { con_g_args = PrefixConGADT _ args }) = args
    con_arg_fields (ConDeclH98 { con_args = PrefixCon _ args }) = args
    con_arg_fields (ConDeclH98 { con_args = InfixCon _ arg1 arg2 }) = [arg1, arg2]
    con_arg_fields _ = []

{-
Note [Type data declarations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
With the TypeData extension (GHC proposal #106), one can write `type data`
declarations, like

    type data Nat = Zero | Succ Nat

or equivalently in GADT style:

    type data Nat where
        Zero :: Nat
        Succ :: Nat -> Nat

This defines the constructors `Zero` and `Succ` in the TcCls namespace
(type constructors and classes) instead of the Data namespace (data
constructors).  This contrasts with the DataKinds extension, which
allows constructors defined in the Data namespace to be promoted to the
TcCls namespace at the point of use in a type.

Type data declarations have the syntax of `data` declarations (but not
`newtype` declarations), either ordinary algebraic data types or GADTs,
preceded by `type`, with the following restrictions:

(R0) 'data' decls only, not 'newtype' decls.  This is checked by
     the parser.

(R1) There are no data type contexts (even with the DatatypeContexts
     extension).

(R2) There are no labelled fields.  Perhaps these could be supported
     using type families, but they are omitted for now.

(R3) There are no strictness, unpackedness or multiplicity annotations
     (!, ~, {-# UNPACK #-}, {-# NOUNPACK #-}, %m ->, ⊸), because they
     don't make sense at the type level.

(R4) The types of the constructors contain no constraints.

(R5) There are no deriving clauses.

The data constructors of a `type data` declaration obey the following
Core invariant:

(I1) The data constructors of a `type data` declaration may be mentioned in
     /types/, but never in /terms/ or the /pattern of a case alternative/.

Wrinkles:

(W0) Wrappers.  The data constructor of a `type data` declaration has a worker
     (like every data constructor) but does /not/ have a wrapper.  Wrappers
     only make sense for value-level data constructors. Indeed, the worker Id
     is never used (invariant (I1)), so it barely makes sense to talk about
     the worker. A `type data` constructor only shows up in types, where it
     appears as a TyCon, specifically a PromotedDataCon -- no Id in sight.
     See #24620 for an example of what happens if you accidentally include
     a wrapper.

     See `wrapped_reqd` in GHC.Types.Id.Make.mkDataConRep` for the place where
     this check is implemented.

     This specifically includes `type data` declarations implemented as GADTs,
     such as this example from #22948:

        type data T a where
          A :: T Int
          B :: T a

     If `T` were an ordinary `data` declaration, then `A` would have a wrapper
     to account for the GADT-like equality in its return type. Because `T` is
     declared as a `type data` declaration, however, the wrapper is omitted.

(W1) To prevent users from conjuring up `type data` values at the term level,
     we disallow using the tagToEnum# function on a type headed by a `type
     data` type. For instance, GHC will reject this code:

       type data Letter = A | B | C

       f :: Letter
       f = tagToEnum# 0#

     See `GHC.Tc.Gen.App.checkTagToEnum`, specifically `check_enumeration`.

(W2) Although `type data` data constructors do not exist at the value level,
     it is still possible to match on a value whose type is headed by a `type data`
     type constructor, such as this example from #22964:

       type data T a where
         A :: T Int
         B :: T a

       f :: T a -> ()
       f x = case x of {}

     And yet we must guarantee invariant (I1). This has three consequences:

     (W2a) During checking the coverage of `f`'s pattern matches, we treat `T`
           as if it were an empty data type so that GHC does not warn the user
           to match against `A` or `B`. (Otherwise, you end up with the bug
           reported in #22964.)  See GHC.HsToCore.Pmc.Solver.vanillaCompleteMatchTC.

     (W2b) In `GHC.Core.Utils.refineDataAlt`, do /not/ fill in the DEFAULT case
           with the data constructor, else (I1) is violated. See GHC.Core.Utils
           Note [Refine DEFAULT case alternatives] Exception 2

     (W2c) In `GHC.Core.Opt.ConstantFold.caseRules`, we do not want to transform
              case dataToTagLarge# x of t -> blah
           into
              case x of { A -> ...; B -> .. }
           because again that conjures up the type-level-only data constructors
           `A` and `B` in a pattern, violating (I1) (#23023).
           So we check for "type data" TyCons before applying this
           transformation.  (In practice, this doesn't matter because
           we also refuse to solve DataToTag instances at types
           corresponding to type data declarations.  See rule C1 from
           Note [DataToTag overview] in GHC.Tc.Instance.Class.)

The main parts of the implementation are:

* The parser recognizes `type data` (but not `type newtype`); this ensures (R0).

* During the initial construction of the AST,
  GHC.Parser.PostProcess.checkNewOrData sets the `Bool` argument of the
  `DataTypeCons` inside a `HsDataDefn` to mark a `type data` declaration.
  It also puts the the constructor names (`Zero` and `Succ` in our
  example) in the TcCls namespace.

* GHC.Rename.Module.rnDataDefn calls `check_type_data` on these
  declarations, which checks that the TypeData extension is enabled and
  checks restrictions (R1), (R2), (R3) and (R5).  They could equally
  well be checked in the typechecker, but we err on the side of catching
  imposters early.

* GHC.Tc.TyCl.checkValidDataCon checks restriction (R4) on these declarations.

* When beginning to type check a mutually recursive group of declarations,
  the `type data` constructors (`Zero` and `Succ` in our example) are
  added to the type-checker environment as `APromotionErr TyConPE` by
  GHC.Tc.TyCl.mkPromotionErrorEnv, so they cannot be used within the
  recursive group.  This mirrors the DataKinds behaviour described
  at Note [Recursion and promoting data constructors] in GHC.Tc.TyCl.
  For example, this is rejected:

    type data T f = K (f (K Int)) -- illegal: tycon K is recursively defined

* The `type data` data type, such as `Nat` in our example, is represented
  by a `TyCon` that is an `AlgTyCon`, but its `AlgTyConRhs` has the
  `is_type_data` field set.

* The constructors of the data type, `Zero` and `Succ` in our example,
  are each represented by a `DataCon` as usual.  That `DataCon`'s
  `dcPromotedField` is a `TyCon` (for `Zero`, say) that you can use
  in a type.

* After a `type data` declaration has been type-checked, the
  type-checker environment entry (a `TyThing`) for each constructor
  (`Zero` and `Succ` in our example) is
  - just an `ATyCon` for the promoted type constructor,
  - not the bundle (`ADataCon` for the data con, `AnId` for the work id,
    wrap id) required for a normal data constructor
  See GHC.Types.TyThing.implicitTyConThings.

* GHC.Core.TyCon.isDataKindsPromotedDataCon ignores promoted constructors
  from `type data`, which do not use the distinguishing quote mark added
  to constructors promoted by DataKinds.

* GHC.Core.TyCon.isBoxedDataTyCon ignores types coming from a `type data`
  declaration (by checking the `is_type_data` field), so that these do
  not contribute executable code such as constructor wrappers.

* The `is_type_data` field is copied into a Boolean argument
  of the `IfDataTyCon` constructor of `IfaceConDecls` by
  GHC.Iface.Make.tyConToIfaceDecl.

* The Template Haskell `Dec` type has an constructor `TypeDataD` for
  `type data` declarations.  When these are converted back to Hs types
  in a splice, the constructors are placed in the TcCls namespace.

-}

rnLHsDerivingClause :: HsDocContext -> LHsDerivingClause GhcPs
                    -> RnM (LHsDerivingClause GhcRn, FreeNames)
rnLHsDerivingClause doc
                (L loc (HsDerivingClause
                              { deriv_clause_ext = noExtField
                              , deriv_clause_strategy = dcs
                              , deriv_clause_tys = dct }))
  = do { (dcs', dct', fvs)
           <- rnLDerivStrategy doc dcs $ rn_deriv_clause_tys dct
       ; pure ( L loc (HsDerivingClause { deriv_clause_ext = noExtField
                                        , deriv_clause_strategy = dcs'
                                        , deriv_clause_tys = dct' })
              , fvs ) }
  where
    rn_deriv_clause_tys :: LDerivClauseTys GhcPs
                        -> RnM (LDerivClauseTys GhcRn, FreeNames)
    rn_deriv_clause_tys (L l dct) = case dct of
      DctSingle x ty -> do
        (ty', fvs) <- rn_clause_pred ty
        pure (L l (DctSingle x ty'), fvs)
      DctMulti x tys -> do
        (tys', fvs) <- mapFvRn rn_clause_pred tys
        pure (L l (DctMulti x tys'), fvs)

    rn_clause_pred :: LHsSigType GhcPs -> RnM (LHsSigType GhcRn, FreeNames)
    rn_clause_pred pred_ty = do
      checkInferredVars doc pred_ty
      ret@(pred_ty', _) <- rnHsSigType doc TypeLevel pred_ty
      -- Check if there are any nested `forall`s, which are illegal in a
      -- `deriving` clause.
      -- See Note [No nested foralls or contexts in instance types]
      -- (Wrinkle: Derived instances) in GHC.Hs.Type.
      addNoNestedForallsContextsErr doc NFC_DerivedClassType
        (getLHsInstDeclHead pred_ty')
      pure ret

rnLDerivStrategy :: forall a.
                    HsDocContext
                 -> Maybe (LDerivStrategy GhcPs)
                 -> RnM (a, FreeNames)
                 -> RnM (Maybe (LDerivStrategy GhcRn), a, FreeNames)
rnLDerivStrategy doc mds thing_inside
  = case mds of
      Nothing -> boring_case Nothing
      Just (L loc ds) ->
        setSrcSpanA loc $ do
          (ds', thing, fvs) <- rn_deriv_strat ds
          pure (Just (L loc ds'), thing, fvs)
  where
    rn_deriv_strat :: DerivStrategy GhcPs
                   -> RnM (DerivStrategy GhcRn, a, FreeNames)
    rn_deriv_strat ds = do
      let extNeeded :: LangExt.Extension
          extNeeded
            | ViaStrategy{} <- ds
            = LangExt.DerivingVia
            | otherwise
            = LangExt.DerivingStrategies

      unlessXOptM extNeeded $
        failWith $ TcRnIllegalDerivStrategy ds

      case ds of
        StockStrategy    _ -> boring_case (StockStrategy noExtField)
        AnyclassStrategy _ -> boring_case (AnyclassStrategy noExtField)
        NewtypeStrategy  _ -> boring_case (NewtypeStrategy noExtField)
        ViaStrategy (XViaStrategyPs _ via_ty) ->
          do checkInferredVars doc via_ty
             (via_ty', fvs1) <- rnHsSigType doc TypeLevel via_ty
             let HsSig { sig_bndrs = via_outer_bndrs
                       , sig_body  = via_body } = unLoc via_ty'
                 via_tvs = hsOuterTyVarNames via_outer_bndrs
             -- Check if there are any nested `forall`s, which are illegal in a
             -- `via` type.
             -- See Note [No nested foralls or contexts in instance types]
             -- (Wrinkle: Derived instances) in GHC.Hs.Type.
             addNoNestedForallsContextsErr doc
               NFC_ViaType via_body
             (thing, fvs2) <- bindLocalNamesFV via_tvs thing_inside
             pure (ViaStrategy via_ty', thing, fvs1 `plusFN` fvs2)

    boring_case :: ds -> RnM (ds, a, FreeNames)
    boring_case ds = do
      (thing, fvs) <- thing_inside
      pure (ds, thing, fvs)

rnFamDecl :: Maybe (Name, [Name])
                        -- Just (cls, cls_tvs) => this FamilyDecl is nested
                        --             inside an *class decl* for cls
                        --             used for associated types
          -> FamilyDecl GhcPs
          -> RnM (FamilyDecl GhcRn, FreeNames)
rnFamDecl mb_cls (FamilyDecl { fdLName = tycon, fdTyVars = tyvars
                             , fdTopLevel = toplevel
                             , fdFixity = fixity
                             , fdInfo = info, fdResultSig = res_sig
                             , fdInjectivityAnn = injectivity })
  = do { tycon' <- lookupLocatedTopBndrRnN WL_TyCon tycon
       ; ((tyvars', res_sig', injectivity'), fv1) <-
            bindHsQTyVars doc mb_cls kvs tyvars $ \ tyvars' _ ->
            do { let rn_sig = rnFamResultSig doc
               ; (res_sig', fv_kind) <- wrapLocFstMA rn_sig res_sig
               ; injectivity' <- traverse (rnInjectivityAnn tyvars' res_sig')
                                          injectivity
               ; return ( (tyvars', res_sig', injectivity') , fv_kind ) }
       ; (info', fv2) <- rn_info info
       ; return (FamilyDecl { fdExt = noAnn
                            , fdLName = tycon', fdTyVars = tyvars'
                            , fdTopLevel = toplevel
                            , fdFixity = fixity
                            , fdInfo = info', fdResultSig = res_sig'
                            , fdInjectivityAnn = injectivity' }
                , fv1 `plusFN` fv2) }
  where
     doc = TyFamilyCtx tycon
     kvs = extractRdrKindSigVars res_sig

     ----------------------
     rn_info :: FamilyInfo GhcPs -> RnM (FamilyInfo GhcRn, FreeNames)
     rn_info (ClosedTypeFamily anns (Just eqns))
       = do { (eqns', fvs)
                <- rnList (rnTyFamInstEqn (NonAssocTyFamEqn ClosedTyFam)) eqns
                                          -- no class context
            ; return (ClosedTypeFamily (anns, Nothing) (Just eqns'), fvs) }
     rn_info (ClosedTypeFamily anns Nothing)
       = return (ClosedTypeFamily (anns, Nothing) Nothing, emptyFNs)
     rn_info (OpenTypeFamily x) = return (OpenTypeFamily x, emptyFNs)
     rn_info (DataFamily x)     = return (DataFamily x, emptyFNs)

rnFamResultSig :: HsDocContext
               -> FamilyResultSig GhcPs
               -> RnM (FamilyResultSig GhcRn, FreeNames)
rnFamResultSig _ (NoSig _)
   = return (NoSig noExtField, emptyFNs)
rnFamResultSig doc (KindSig _ kind)
   = do { (rndKind, ftvs) <- rnLHsKind doc kind
        ;  return (KindSig noExtField rndKind, ftvs) }
rnFamResultSig doc (TyVarSig _ tvbndr)
   = do { -- `TyVarSig` tells us that user named the result of a type family by
          -- writing `= tyvar` or `= (tyvar :: kind)`. In such case we want to
          -- be sure that the supplied result name is not identical to an
          -- already in-scope type variable from an enclosing class.
          --
          --  Example of disallowed declaration:
          --         class C a b where
          --            type F b = a | a -> b
          rdr_env <- getLocalRdrEnv

       ; case hsBndrVar (unLoc tvbndr) of
          HsBndrWildCard _ ->
            -- See Note [Wildcard binders in disallowed contexts] in GHC.Hs.Type
            addErrAt (getLocA tvbndr) $
              TcRnIllegalWildcardInType Nothing WildcardBndrInTyFamResultVar
          HsBndrVar _ (L _ resName) ->
            when (resName `elemLocalRdrEnv` rdr_env) $
            addErrAt (getLocA tvbndr) $
              TcRnShadowedTyVarNameInFamResult resName

       ; bindLHsTyVarBndr doc Nothing -- This might be a lie, but it's used for
                                      -- scoping checks that are irrelevant here
                          tvbndr $ \ tvbndr' ->
         return (TyVarSig noExtField tvbndr', maybe emptyFNs unitFN (hsLTyVarName tvbndr')) }

-- Note [Renaming injectivity annotation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- During renaming of injectivity annotation we have to make several checks to
-- make sure that it is well-formed.  At the moment injectivity annotation
-- consists of a single injectivity condition, so the terms "injectivity
-- annotation" and "injectivity condition" might be used interchangeably.  See
-- Note [Injectivity annotation] for a detailed discussion of currently allowed
-- injectivity annotations.
--
-- Checking LHS is simple because the only type variable allowed on the LHS of
-- injectivity condition is the variable naming the result in type family head.
-- Example of disallowed annotation:
--
--     type family Foo a b = r | b -> a
--
-- Verifying RHS of injectivity consists of checking that:
--
--  1. only variables defined in type family head appear on the RHS (kind
--     variables are also allowed).  Example of disallowed annotation:
--
--        type family Foo a = r | r -> b
--
--  2. for associated types the result variable does not shadow any of type
--     class variables. Example of disallowed annotation:
--
--        class Foo a b where
--           type F a = b | b -> a
--
-- Breaking any of these assumptions results in an error.

-- | Rename injectivity annotation. Note that injectivity annotation is just the
-- part after the "|".  Everything that appears before it is renamed in
-- rnFamDecl.
rnInjectivityAnn :: LHsQTyVars GhcRn           -- ^ Type variables declared in
                                               --   type family head
                 -> LFamilyResultSig GhcRn     -- ^ Result signature
                 -> LInjectivityAnn GhcPs      -- ^ Injectivity annotation
                 -> RnM (LInjectivityAnn GhcRn)
rnInjectivityAnn tvBndrs (L _ (TyVarSig _ resTv))
                 (L srcSpan (InjectivityAnn x injFrom injTo))
 = do
   { (injDecl'@(L _ (InjectivityAnn _ injFrom' injTo')), rn_errors)
          <- askErrsFound $
             bindLocalNames (maybeToList (hsLTyVarName resTv)) $
             -- The return type variable scopes over the injectivity annotation
             -- e.g.   type family F a = (r::*) | r -> a
             do { injFrom' <- fmap greName <$> rnLTyVar injFrom
                ; injTo'   <- mapM (fmap (fmap greName) . rnLTyVar) injTo
                -- Note: srcSpan is unchanged, but typechecker gets
                -- confused, l2l call makes it happy
                ; return $ L (l2l srcSpan) (InjectivityAnn x injFrom' injTo') }

   ; let tvNames  = Set.fromList $ hsAllLTyVarNames tvBndrs

   ; case hsLTyVarName resTv of { Nothing -> return ()
                                ; Just resName -> do {

   ; let -- See Note [Renaming injectivity annotation]
         lhsValid = EQ == (stableNameCmp resName (unLoc injFrom'))
         rhsValid = Set.fromList (map unLoc injTo') `Set.difference` tvNames

   -- if renaming of type variables ended with errors (eg. there were
   -- not-in-scope variables) don't check the validity of injectivity
   -- annotation. This gives better error messages.
   ; unless (rn_errors || lhsValid) $
        addErrAt (getLocA injFrom) $
          TcRnIncorrectTyVarOnLhsOfInjCond resName injFrom

   ; unless (rn_errors || Set.null rhsValid) $
      do { let errorVars = Set.toList rhsValid
         ; addErrAt (locA srcSpan) $
              TcRnUnknownTyVarsOnRhsOfInjCond errorVars } } }

   ; return injDecl' }

-- We can only hit this case when the user writes injectivity annotation without
-- naming the result:
--
--   type family F a | result -> a
--   type family F a :: * | result -> a
--
-- So we rename injectivity annotation like we normally would except that
-- this time we expect "result" to be reported not in scope by rnLTyVar.
rnInjectivityAnn _ _ (L srcSpan (InjectivityAnn x injFrom injTo)) =
   setSrcSpanA srcSpan $ do
   (injDecl', _) <- askErrsFound $ do
     injFrom' <- rnLTyVar injFrom
     injTo'   <- mapM rnLTyVar injTo
     return $ L srcSpan (InjectivityAnn x (fmap greName injFrom') (fmap (fmap greName) injTo'))
   return $ injDecl'

{-
Note [Stupid theta]
~~~~~~~~~~~~~~~~~~~
#3850 complains about a regression wrt 6.10 for
     data Show a => T a
There is no reason not to allow the stupid theta if there are no data
constructors.  It's still stupid, but does no harm, and I don't want
to cause programs to break unnecessarily (notably HList).  So if there
are no data constructors we allow h98_style = True
-}


{- *****************************************************
*                                                      *
     Support code for type/data declarations
*                                                      *
***************************************************** -}

-----------------
rnConDecls :: DataDefnCons (LConDecl GhcPs) -> RnM (DataDefnCons (LConDecl GhcRn), FreeNames)
rnConDecls = mapFvRn (wrapLocFstMA rnConDecl)

rnConDecl :: ConDecl GhcPs -> RnM (ConDecl GhcRn, FreeNames)
rnConDecl decl@(ConDeclH98 { con_name = name, con_telescope = teles
                           , con_args = args
                           , con_doc = mb_doc
                           , con_modifiers = mods })
  = do  { _        <- addLocM checkConName name
        ; new_name <- lookupLocatedTopBndrRnN WL_ConLike name

        -- We bind no implicit binders here; this is just like
        -- a nested HsForAllTy.  E.g. consider
        --         data T a = forall (b::k). MkT (...)
        -- The 'k' will already be in scope from the bindHsQTyVars
        -- for the data decl itself. So we'll get
        --         data T {k} a = ...
        -- And indeed we may later discover (a::k).  But that's the
        -- scoping we get.  So no implicit binders at the existential forall

        ; let ctxt = ConDeclCtx [new_name]
        ; bindHsGadtTelescopes ctxt teles $ \ new_teles ->
    do  { (new_args,    fvs2) <- rnConDeclH98Details (unLoc new_name) ctxt args
        ; (mods', mods_fvs) <- rnModifiersContextAndWarn ctxt mods
        ; let all_fvs  = fvs2 `plusFN` mods_fvs
        ; traceRn "rnConDecl (ConDeclH98)" (ppr name <+> vcat
             [ text "telescopes:" <+> ppr teles
             , text "renamed telescopes:" <+> ppr new_teles ])

        ; mb_doc' <- traverse rnLHsDoc mb_doc
        ; return (decl { con_ext = noExtField
                       , con_name = new_name, con_telescope = new_teles
                       , con_args = new_args
                       , con_doc = mb_doc'
                       , con_modifiers = mods' },
                  all_fvs) }}

rnConDecl (ConDeclGADT { con_names   = names
                       , con_outer_bndrs = L outer_bndrs_loc outer_bndrs
                       , con_telescope = inner_bndrs
                       , con_g_args  = args
                       , con_res_ty  = res_ty
                       , con_modifiers = mods
                       , con_doc     = mb_doc })
  = do  { mapM_ (addLocM checkConName) names
        ; new_names <- mapM (lookupLocatedTopBndrRnN WL_ConLike) names

        ; let -- We must ensure that we extract the free tkvs in left-to-right
              -- order of their appearance in the constructor type.
              -- That order governs the order the implicitly-quantified type
              -- variable, and hence the order needed for visible type application
              -- See #14808.
              free_vars =
                extractHsOuterTvBndrs outer_bndrs           $
                extractHsGadtTelescopes inner_bndrs         $
                extractConDeclGADTDetailsTyVars args        $
                extractHsTysRdrTyVars [res_ty] []

        ; envs <- getRdrEnvs
        ; implicit_bndrs <- filterSourceTableNames envs free_vars
        ; let ctxt = ConDeclCtx (toList new_names)

        ; bindHsOuterTyVarBndrs ctxt Nothing implicit_bndrs outer_bndrs $ \outer_bndrs' ->
          bindHsGadtTelescopes ctxt inner_bndrs $ \inner_bndrs' ->
    do  { (new_args, fvs2)   <- rnConDeclGADTDetails (unLoc (head new_names)) ctxt args
        ; (new_res_ty, fvs3) <-
            if hsHasForEachTy (unLoc res_ty)
            then do { enabled <- xoptM LangExt.ExperimentalDependentTypes
                    ; if enabled then rnRetainedSigType ctxt res_ty
                                 else rnLHsType ctxt res_ty }
            else case HsDecls.splitGadtInterior args res_ty of
                   Just{} -> rnLHsGadtSuffix ctxt res_ty
                   Nothing -> rnLHsType ctxt res_ty
        ; (mods', mods_fvs) <- rnModifiersContextAndWarn ctxt mods

         -- Keep terminal groups on the source side of the declaration.
         -- The prefix fields have already been renamed in their own scope.
       ; let con_result
               | Just (_, _, result) <- HsDecls.splitGadtTerminal new_args new_res_ty
               = result
               | Just (_, result) <- HsDecls.splitGadtInterior new_args new_res_ty
               = result
               | otherwise = new_res_ty
       ; addNoNestedForallsContextsErr ctxt NFC_GadtConSig con_result

        ; let all_fvs = fvs2 `plusFN` fvs3 `plusFN` mods_fvs

        ; traceRn "rnConDecl (ConDeclGADT)"
            (ppr names $$ ppr outer_bndrs' $$ ppr inner_bndrs')
        ; new_mb_doc <- traverse rnLHsDoc mb_doc
        ; return (ConDeclGADT { con_g_ext = noExtField, con_names = new_names
                              , con_outer_bndrs = L outer_bndrs_loc outer_bndrs'
                              , con_telescope = inner_bndrs'
                              , con_g_args = new_args, con_res_ty = new_res_ty
                              , con_doc = new_mb_doc, con_modifiers = mods' },
                  all_fvs) } }

rnConDeclH98Details ::
      Name
   -> HsDocContext
   -> HsConDeclH98Details GhcPs
   -> RnM (HsConDeclH98Details GhcRn, FreeNames)
rnConDeclH98Details _ doc (PrefixCon x tys)
  = do { (new_tys, fvs) <- mapFvRn (rnHsConDeclField doc) tys
       ; return (PrefixCon x new_tys, fvs) }
rnConDeclH98Details _ doc (InfixCon x ty1 ty2)
  = do { (new_ty1, fvs1) <- rnHsConDeclField doc ty1
       ; (new_ty2, fvs2) <- rnHsConDeclField doc ty2
       ; return (InfixCon x new_ty1 new_ty2, fvs1 `plusFN` fvs2) }
rnConDeclH98Details con doc (RecCon x flds)
  = do { (new_flds, fvs) <- rnRecHsConDeclRecFields con doc flds
       ; return (RecCon x new_flds, fvs) }

rnConDeclGADTDetails ::
      Name
   -> HsDocContext
   -> HsConDeclGADTDetails GhcPs
   -> RnM (HsConDeclGADTDetails GhcRn, FreeNames)
rnConDeclGADTDetails _ doc (PrefixConGADT _ tys)
  = do { (new_tys, fvs) <- mapFvRn (rnHsConDeclField doc) tys
       ; return (PrefixConGADT noExtField new_tys, fvs) }
rnConDeclGADTDetails con doc (RecConGADT _ flds)
  = do { (new_flds, fvs) <- rnRecHsConDeclRecFields con doc flds
       ; return (RecConGADT noExtField new_flds, fvs) }

rnRecHsConDeclRecFields ::
     Name
  -> HsDocContext
  -> LocatedA [LHsConDeclRecField GhcPs]
  -> RnM (LocatedA [LHsConDeclRecField GhcRn], FreeNames)
rnRecHsConDeclRecFields con doc (L l fields)
  = do  { fls <- lookupConstructorFields (noUserRdr con)
        ; (new_fields, fvs) <- rnHsConDeclRecFields doc fls fields
                -- No need to check for duplicate fields
                -- since that is done by GHC.Rename.Names.extendGlobalRdrEnvRn
        ; pure (L l new_fields, fvs) }

-------------------------------------------------

-- | Brings pattern synonym names and also pattern synonym selectors
-- from record pattern synonyms into scope.
extendPatSynEnv :: DuplicateRecordFields -> FieldSelectors -> HsValBinds GhcPs -> MiniFixityEnv
                -> ([Name] -> TcRnIf TcGblEnv TcLclEnv a) -> TcM a
extendPatSynEnv dup_fields_ok has_sel val_decls local_fix_env thing = do {
     names_with_fls <- new_ps val_decls
   ; let pat_syn_bndrs = concat [ conLikeName_Name name : map flSelector flds
                                | (name, con_info) <- names_with_fls
                                , let flds = conInfoFields con_info ]
   ; let gres =  map (mkLocalConLikeGRE NoParent) names_with_fls
              ++ mkLocalFieldGREs NoParent names_with_fls
      -- Recall Note [Parents] in GHC.Types.Name.Reader:
      --
      -- pattern synonym constructors and their record fields have no parent
      -- in the module in which they are defined.
   ; (gbl_env, lcl_env) <- extendGlobalRdrEnvRn gres local_fix_env
   ; restoreEnvs (gbl_env, lcl_env) (thing pat_syn_bndrs) }
  where

    new_ps :: HsValBinds GhcPs -> TcM [(ConLikeName, ConInfo)]
    new_ps (ValBinds _ binds) = foldrM new_ps' [] (val_binds binds)
    new_ps _ = panic "new_ps"

    new_ps' :: LHsBindLR GhcPs GhcPs
            -> [(ConLikeName, ConInfo)]
            -> TcM [(ConLikeName, ConInfo)]
    new_ps' bind names
      | (L bind_loc (PatSynBind _ (PSB { psb_id = L _ n
                                       , psb_args = RecCon _ as }))) <- bind
      = do
          bnd_name <- newTopSrcBinder (L (l2l bind_loc) n)
          let field_occs = map ((\ f -> L (noAnnSrcSpan $ getLocA (foLabel f)) f) . recordPatSynField) as
          flds <- mapM (newRecordFieldLabel dup_fields_ok has_sel [bnd_name]) field_occs
          let con_info = mkConInfo ConIsPatSyn (conDetailsVisArity (RecCon noAnn as)) flds
          return ((PatSynName bnd_name, con_info) : names)
      | L bind_loc (PatSynBind _ (PSB { psb_id = L _ n, psb_args = as })) <- bind
      = do
        bnd_name <- newTopSrcBinder (L (l2l bind_loc) n)
        let con_info = mkConInfo ConIsPatSyn (conDetailsVisArity as) []
        return ((PatSynName bnd_name, con_info) : names)
      | otherwise
      = return names

conDetailsVisArity :: HsPatSynDetails (GhcPass p) -> VisArity
conDetailsVisArity = \case
  PrefixCon _ args -> length args
  RecCon _ flds -> length flds
  InfixCon _ _ _ -> 2

{-
*********************************************************
*                                                      *
\subsection{Support code to rename types}
*                                                      *
*********************************************************
-}

rnFds :: [LHsFunDep GhcPs] -> RnM [LHsFunDep GhcRn]
rnFds fds
  = mapM (wrapLocMA rn_fds) fds
  where
    rn_fds :: FunDep GhcPs -> RnM (FunDep GhcRn)
    rn_fds (FunDep x tys1 tys2)
      = do { tys1' <- rnHsTyVars tys1
           ; tys2' <- rnHsTyVars tys2
           ; return (FunDep x tys1' tys2') }

rnHsTyVars :: [LocatedN RdrName] -> RnM [LocatedN Name]
rnHsTyVars tvs  = mapM rnHsTyVar tvs

rnHsTyVar :: LocatedN RdrName -> RnM (LocatedN Name)
rnHsTyVar (L l tyvar) = do
  tyvar' <- lookupOccRn WL_TyVar tyvar
  return (L l tyvar')

{-
*********************************************************
*                                                      *
        findSplice
*                                                      *
*********************************************************

This code marches down the declarations, looking for the first
Template Haskell splice.  As it does so it
        a) groups the declarations into a HsGroup
        b) runs any top-level quasi-quotes
-}

findSplice :: [LHsDecl GhcPs]
           -> RnM (HsGroup GhcPs, Maybe (SpliceDecl GhcPs, [LHsDecl GhcPs]))
findSplice ds = addl emptyRdrGroup ds

addl :: HsGroup GhcPs -> [LHsDecl GhcPs]
     -> RnM (HsGroup GhcPs, Maybe (SpliceDecl GhcPs, [LHsDecl GhcPs]))
-- This stuff reverses the declarations (again) but it doesn't matter
addl gp []           = return (gp, Nothing)
addl gp (L l d : ds) = add gp l d ds


add :: HsGroup GhcPs -> SrcSpanAnnA -> HsDecl GhcPs -> [LHsDecl GhcPs]
    -> RnM (HsGroup GhcPs, Maybe (SpliceDecl GhcPs, [LHsDecl GhcPs]))

-- #10047: Declaration QuasiQuoters are expanded immediately, without
--         causing a group split
add gp _ (SpliceD _ (SpliceDecl _ (L _ qq@HsQuasiQuote{}) _)) ds
  = do { (ds', _) <- rnTopSpliceDecls qq
       ; addl gp (ds' ++ ds)
       }

add gp loc (SpliceD _ splice@(SpliceDecl _ _ flag)) ds
  = do { -- We've found a top-level splice.  If it is an *implicit* one
         -- (i.e. a naked top level expression), throw an error if
         -- TemplateHaskell is not enabled.
         case flag of
           DollarSplice -> return ()
           BareSplice -> do { unlessXOptM LangExt.TemplateHaskell
                            $ setSrcSpan (locA loc)
                            $ failWith badImplicitSplice }

       ; return (gp, Just (splice, ds)) }
  where
    badImplicitSplice :: TcRnMessage
    badImplicitSplice = TcRnTHError (THSyntaxError BadImplicitSplice)

-- Class declarations: added to the TyClGroup
add gp@(HsGroup {hs_tyclds = ts}) l (TyClD _ d) ds
  = addl (gp { hs_tyclds = add_tycld (L l d) ts }) ds

-- Signatures: fixity sigs go a different place than all others
add gp@(HsGroup {hs_fixds = ts}) l (SigD _ (FixSig _ f)) ds
  = addl (gp {hs_fixds = L l f : ts}) ds

-- Standalone kind signatures: added to the TyClGroup
add gp@(HsGroup {hs_tyclds = ts}) l (KindSigD _ s) ds
  = addl (gp {hs_tyclds = add_kisig (L l s) ts}) ds

add gp@(HsGroup {hs_valds = ts}) l (SigD _ d) ds
  = addl (gp {hs_valds = add_sig (L l d) ts}) ds

-- Value declarations: use add_bind
add gp@(HsGroup {hs_valds  = ts}) l (ValD _ d) ds
  = addl (gp { hs_valds = add_bind (L l d) ts }) ds

-- Role annotations: added to the TyClGroup
add gp@(HsGroup {hs_tyclds = ts}) l (RoleAnnotD _ d) ds
  = addl (gp { hs_tyclds = add_role_annot (L l d) ts }) ds

-- NB instance declarations go into TyClGroups. We throw them into the first
-- group, just as we do for the TyClD case. The renamer will go on to group
-- and order them later.
add gp@(HsGroup {hs_tyclds = ts})  l (InstD _ d) ds
  = addl (gp { hs_tyclds = add_instd (L l d) ts }) ds

-- The rest are routine
add gp@(HsGroup {hs_derivds = ts})  l (DerivD _ d) ds
  = addl (gp { hs_derivds = L l d : ts }) ds
add gp@(HsGroup {hs_defds  = ts})  l (DefD _ d) ds
  = addl (gp { hs_defds = L l d : ts }) ds
add gp@(HsGroup {hs_fords  = ts}) l (ForD _ d) ds
  = addl (gp { hs_fords = L l d : ts }) ds
add gp@(HsGroup {hs_warnds  = ts})  l (WarningD _ d) ds
  = addl (gp { hs_warnds = L l d : ts }) ds
add gp@(HsGroup {hs_annds  = ts}) l (AnnD _ d) ds
  = addl (gp { hs_annds = L l d : ts }) ds
add gp@(HsGroup {hs_ruleds  = ts}) l (RuleD _ d) ds
  = addl (gp { hs_ruleds = L l d : ts }) ds
add gp l (DocD _ d) ds
  = addl (gp { hs_docs = (L l d) : (hs_docs gp) })  ds

add_tycld :: LTyClDecl GhcPs -> [TyClGroup GhcPs]
          -> [TyClGroup GhcPs]
add_tycld d []       = [TyClGroup { group_ext    = noExtField
                                  , group_tyclds = [d]
                                  , group_kisigs = []
                                  , group_roles  = []
                                  , group_instds = []
                                  }
                       ]
add_tycld d (ds@(TyClGroup { group_tyclds = tyclds }):dss)
  = ds { group_tyclds = d : tyclds } : dss

add_instd :: LInstDecl GhcPs -> [TyClGroup GhcPs]
          -> [TyClGroup GhcPs]
add_instd d []       = [TyClGroup { group_ext    = noExtField
                                  , group_tyclds = []
                                  , group_kisigs = []
                                  , group_roles  = []
                                  , group_instds = [d]
                                  }
                       ]
add_instd d (ds@(TyClGroup { group_instds = instds }):dss)
  = ds { group_instds = d : instds } : dss

add_role_annot :: LRoleAnnotDecl GhcPs -> [TyClGroup GhcPs]
               -> [TyClGroup GhcPs]
add_role_annot d [] = [TyClGroup { group_ext    = noExtField
                                 , group_tyclds = []
                                 , group_kisigs = []
                                 , group_roles  = [d]
                                 , group_instds = []
                                 }
                      ]
add_role_annot d (tycls@(TyClGroup { group_roles = roles }) : rest)
  = tycls { group_roles = d : roles } : rest

add_kisig :: LStandaloneKindSig GhcPs
         -> [TyClGroup GhcPs] -> [TyClGroup GhcPs]
add_kisig d [] = [TyClGroup { group_ext    = noExtField
                            , group_tyclds = []
                            , group_kisigs = [d]
                            , group_roles  = []
                            , group_instds = []
                            }
                 ]
add_kisig d (tycls@(TyClGroup { group_kisigs = kisigs }) : rest)
  = tycls { group_kisigs = d : kisigs } : rest

add_bind :: LHsBind a -> HsValBinds a -> HsValBinds a
add_bind b (ValBinds x bs) = ValBinds x (bs ++ [VbBind b])
add_bind _ (XValBindsLR {})     = panic "GHC.Rename.Module.add_bind"

add_sig :: LSig (GhcPass a) -> HsValBinds (GhcPass a) -> HsValBinds (GhcPass a)
add_sig s (ValBinds x bs) = ValBinds x (VbSig s:bs)
add_sig _ (XValBindsLR {})     = panic "GHC.Rename.Module.add_sig"
