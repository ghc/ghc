{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE DisambiguateRecordFields #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns   #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Renaming of patterns

Basically dependency analysis.

Handles @Match@, @GRHSs@, @HsExpr@, and @Qualifier@ datatypes.  In
general, all of these functions return a renamed thing, and a set of
free variables.
-}
module GHC.Rename.Pat (-- main entry points
              rnPat, rnBindPat, rnBindMatchPat, rnPatAndThen,
              rnLMatchPats, rnLMatchPat,

              NameMaker, applyNameMaker,     -- a utility for making names:
              localRecNameMaker, topRecNameMaker,  --   sometimes we want to make local names,
                                             --   sometimes we want to make top (qualified) names.
              isTopRecNameMaker,

              rnHsRecFields, HsRecFieldContext(..),
              rnHsRecUpdFields,

              -- CpsRn monad
              CpsRn, liftCps, liftCpsWithCont,

              -- Literals
              rnLit, rnOverLit,
             ) where

-- ENH: thin imports to only what is necessary for patterns

import GHC.Prelude

import {-# SOURCE #-} GHC.Rename.Expr ( rnLExpr )
import {-# SOURCE #-} GHC.Rename.Splice ( rnSplicePat )

import GHC.Hs
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.Zonk   ( hsOverLitName )
import GHC.Rename.Env
import GHC.Rename.Fixity
import GHC.Rename.Utils    ( HsDocContext(..), newLocalBndrRn, bindLocalNames
                           , warnUnusedMatches, newLocalBndrRn
                           , checkUnusedRecordWildcard
                           , checkDupNames, checkDupAndShadowedNames
                           , wrapGenSpan, genHsApps, genLHsVar, genHsIntegralLit, warnForallIdentifier )
import GHC.Rename.HsType
import GHC.Builtin.Names
import GHC.Types.Avail ( greNameMangledName )
import GHC.Types.Error
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Name.Reader
import GHC.Types.Basic
import GHC.Types.SourceText
import GHC.Utils.Misc
import GHC.Data.List.SetOps( removeDups )
import GHC.Utils.Outputable
import GHC.Utils.Panic.Plain
import GHC.Types.SrcLoc
import GHC.Types.Literal   ( inCharRange )
import GHC.Builtin.Types   ( nilDataCon )
import GHC.Core.DataCon
import GHC.Driver.Session ( getDynFlags, xopt_DuplicateRecordFields )
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad       ( when, ap, guard, forM, unless )
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.Ratio
import GHC.Types.FieldLabel (DuplicateRecordFields(..))

{-
*********************************************************
*                                                      *
        The CpsRn Monad
*                                                      *
*********************************************************

Note [CpsRn monad]
~~~~~~~~~~~~~~~~~~
The CpsRn monad uses continuation-passing style to support this
style of programming:

        do { ...
           ; ns <- bindNames rs
           ; ...blah... }

   where rs::[RdrName], ns::[Name]

The idea is that '...blah...'
  a) sees the bindings of ns
  b) returns the free variables it mentions
     so that bindNames can report unused ones

In particular,
    mapM rnPatAndThen [p1, p2, p3]
has a *left-to-right* scoping: it makes the binders in
p1 scope over p2,p3.
-}

newtype CpsRn b = CpsRn { unCpsRn :: forall r. (b -> RnM (r, FreeVars))
                                            -> RnM (r, FreeVars) }
        deriving (Functor)
        -- See Note [CpsRn monad]

instance Applicative CpsRn where
    pure x = CpsRn (\k -> k x)
    (<*>) = ap

instance Monad CpsRn where
  (CpsRn m) >>= mk = CpsRn (\k -> m (\v -> unCpsRn (mk v) k))

runCps :: CpsRn a -> RnM (a, FreeVars)
runCps (CpsRn m) = m (\r -> return (r, emptyFVs))

liftCps :: RnM a -> CpsRn a
liftCps rn_thing = CpsRn (\k -> rn_thing >>= k)

liftCpsFV :: RnM (a, FreeVars) -> CpsRn a
liftCpsFV rn_thing = CpsRn (\k -> do { (v,fvs1) <- rn_thing
                                     ; (r,fvs2) <- k v
                                     ; return (r, fvs1 `plusFV` fvs2) })

liftCpsWithCont :: (forall r. (b -> RnM (r, FreeVars)) -> RnM (r, FreeVars)) -> CpsRn b
liftCpsWithCont = CpsRn

wrapSrcSpanCps :: (a -> CpsRn b) -> LocatedA a -> CpsRn (LocatedA b)
-- Set the location, and also wrap it around the value returned
wrapSrcSpanCps fn (L loc a)
  = CpsRn (\k -> setSrcSpanA loc $
                 unCpsRn (fn a) $ \v ->
                 k (L loc v))

lookupConCps :: LocatedN RdrName -> CpsRn (LocatedN Name)
lookupConCps con_rdr
  = CpsRn (\k -> do { con_name <- lookupLocatedOccRnConstr con_rdr
                    ; (r, fvs) <- k con_name
                    ; return (r, addOneFV fvs (unLoc con_name)) })
    -- We add the constructor name to the free vars
    -- See Note [Patterns are uses]

{-
Note [Patterns are uses]
~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  module Foo( f, g ) where
  data T = T1 | T2

  f T1 = True
  f T2 = False

  g _ = T1

Arguably we should report T2 as unused, even though it appears in a
pattern, because it never occurs in a constructed position.
See #7336.
However, implementing this in the face of pattern synonyms would be
less straightforward, since given two pattern synonyms

  pattern P1 <- P2
  pattern P2 <- ()

we need to observe the dependency between P1 and P2 so that type
checking can be done in the correct order (just like for value
bindings). Dependencies between bindings is analyzed in the renamer,
where we don't know yet whether P2 is a constructor or a pattern
synonym. So for now, we do report conid occurrences in patterns as
uses.

*********************************************************
*                                                      *
        Name makers
*                                                      *
*********************************************************

Externally abstract type of name makers,
which is how you go from a RdrName to a Name
-}

data NameMaker
  = LamMk       -- Lambdas
      Bool      -- True <=> report unused bindings
                --   (even if True, the warning only comes out
                --    if -Wunused-matches is on)

  | LetMk       -- Let bindings, incl top level
                -- Do *not* check for unused bindings
      TopLevelFlag
      MiniFixityEnv

topRecNameMaker :: MiniFixityEnv -> NameMaker
topRecNameMaker fix_env = LetMk TopLevel fix_env

isTopRecNameMaker :: NameMaker -> Bool
isTopRecNameMaker (LetMk TopLevel _) = True
isTopRecNameMaker _ = False

localRecNameMaker :: MiniFixityEnv -> NameMaker
localRecNameMaker fix_env = LetMk NotTopLevel fix_env

matchNameMaker :: HsMatchContext a -> NameMaker
matchNameMaker ctxt = LamMk report_unused
  where
    -- Do not report unused names in interactive contexts
    -- i.e. when you type 'x <- e' at the GHCi prompt
    report_unused = case ctxt of
                      StmtCtxt (HsDoStmt GhciStmtCtxt) -> False
                      -- also, don't warn in pattern quotes, as there
                      -- is no RHS where the variables can be used!
                      ThPatQuote            -> False
                      _                     -> True

newPatLName :: NameMaker -> LocatedN RdrName -> CpsRn (LocatedN Name)
newPatLName name_maker rdr_name@(L loc _)
  = do { name <- newPatName name_maker rdr_name
       ; return (L loc name) }

newPatName :: NameMaker -> LocatedN RdrName -> CpsRn Name
newPatName (LamMk report_unused) rdr_name
  = CpsRn (\ thing_inside ->
        do { warnForallIdentifier rdr_name
           ; name <- newLocalBndrRn rdr_name
           ; (res, fvs) <- bindLocalNames [name] (thing_inside name)
           ; when report_unused $ warnUnusedMatches [name] fvs
           ; return (res, name `delFV` fvs) })

newPatName (LetMk is_top fix_env) rdr_name
  = CpsRn (\ thing_inside ->
        do { warnForallIdentifier rdr_name
           ; name <- case is_top of
                       NotTopLevel -> newLocalBndrRn rdr_name
                       TopLevel    -> newTopSrcBinder rdr_name
           ; bindLocalNames [name] $
                 -- Do *not* use bindLocalNameFV here;
                 --   see Note [View pattern usage]
                 -- For the TopLevel case
                 --   see Note [bindLocalNames for an External name]
             addLocalFixities fix_env [name] $
             thing_inside name })

{- Note [bindLocalNames for an External name]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In the TopLevel case, the use of bindLocalNames here is somewhat
suspicious because it binds a top-level External name in the
LocalRdrEnv.  c.f. Note [LocalRdrEnv] in GHC.Types.Name.Reader.

However, this only happens when renaming the LHS (only) of a top-level
pattern binding.  Even though this only the LHS, we need to bring the
binder into scope in the pattern itself in case the binder is used in
subsequent view patterns.  A bit bizarre, something like
  (x, Just y <- f x) = e

Anyway, bindLocalNames does work, and the binding only exists for the
duration of the pattern; then the top-level name is added to the
global env before going on to the RHSes (see GHC.Rename.Module).

Note [View pattern usage]
~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  let (r, (r -> x)) = x in ...
Here the pattern binds 'r', and then uses it *only* in the view pattern.
We want to "see" this use, and in let-bindings we collect all uses and
report unused variables at the binding level. So we must use bindLocalNames
here, *not* bindLocalNameFV.  #3943.


Note [Don't report shadowing for pattern synonyms]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There is one special context where a pattern doesn't introduce any new binders -
pattern synonym declarations. Therefore we don't check to see if pattern
variables shadow existing identifiers as they are never bound to anything
and have no scope.

Without this check, there would be quite a cryptic warning that the `x`
in the RHS of the pattern synonym declaration shadowed the top level `x`.

```
x :: ()
x = ()

pattern P x = Just x
```

See #12615 for some more examples.

Note [Handling overloaded and rebindable patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Overloaded paterns and rebindable patterns are desugared in the renamer
using the HsPatExpansion mechanism detailed in:
Note [Rebindable syntax and HsExpansion]
The approach is similar to that of expressions, which is further detailed
in Note [Handling overloaded and rebindable constructs] in GHC.Rename.Expr.

Here are the patterns that are currently desugared in this way:

* ListPat (list patterns [p1,p2,p3])
  When (and only when) OverloadedLists is on, desugar to a view pattern:
    [p1, p2, p3]
  ==>
    toList -> [p1, p2, p3]
              ^^^^^^^^^^^^ built-in (non-overloaded) list pattern
  NB: the type checker and desugarer still see ListPat,
      but to them it always means the built-in list pattern.
  See Note [Desugaring overloaded list patterns] below for more details.

We expect to add to this list as we deal with more patterns via the expansion
mechanism.

Note [Desugaring overloaded list patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If OverloadedLists is enabled, we desugar a list pattern to a view pattern:

  [p1, p2, p3]
==>
  toList -> [p1, p2, p3]

This happens directly in the renamer, using the HsPatExpansion mechanism
detailed in Note [Rebindable syntax and HsExpansion].

Note that we emit a special view pattern: we additionally keep track of an
inverse to the pattern.
See Note [Invertible view patterns] in GHC.Tc.TyCl.PatSyn for details.

== Wrinkle ==

This is all fine, except in one very specific case:
  - when RebindableSyntax is off,
  - and the type being matched on is already a list type.

In this case, it is undesirable to desugar an overloaded list pattern into
a view pattern. To illustrate, consider the following program:

> {-# LANGUAGE OverloadedLists #-}
>
> f []    = True
> f (_:_) = False

Without any special logic, the pattern `[]` is desugared to `(toList -> [])`,
whereas `(_:_)` remains a constructor pattern. This implies that the argument
of `f` is necessarily a list (even though `OverloadedLists` is enabled).
After desugaring the overloaded list pattern `[]`, and type-checking, we obtain:

> f :: [a] -> Bool
> f (toList -> []) = True
> f (_:_)          = False

The pattern match checker then warns that the pattern `[]` is not covered,
as it isn't able to look through view patterns.
We can see that this is silly: as we are matching on a list, `toList` doesn't
actually do anything. So we ignore it, and desugar the pattern to an explicit
list pattern, instead of a view pattern.

Note however that this is not necessarily sound, because it is possible to have
a list `l` such that `toList l` is not the same as `l`.
This can happen with an overlapping instance, such as the following:

instance {-# OVERLAPPING #-} IsList [Int] where
  type Item [Int] = Int
  toList = reverse
  fromList = reverse

We make the assumption that no such instance exists, in order to avoid worsening
pattern-match warnings (see #14547).

*********************************************************
*                                                      *
        External entry points
*                                                      *
*********************************************************

There are various entry points to renaming patterns, depending on
 (1) whether the names created should be top-level names or local names
 (2) whether the scope of the names is entirely given in a continuation
     (e.g., in a case or lambda, but not in a let or at the top-level,
      because of the way mutually recursive bindings are handled)
 (3) whether the a type signature in the pattern can bind
        lexically-scoped type variables (for unpacking existential
        type vars in data constructors)
 (4) whether we do duplicate and unused variable checking
 (5) whether there are fixity declarations associated with the names
     bound by the patterns that need to be brought into scope with them.

 Rather than burdening the clients of this module with all of these choices,
 we export the three points in this design space that we actually need:
-}

-- ----------- Entry point 1: rnPats -------------------
-- Binds local names; the scope of the bindings is entirely in the thing_inside
--   * allows type sigs to bind type vars
--   * local namemaker
--   * unused and duplicate checking
--   * no fixities
rnLMatchPats :: HsMatchContext GhcRn -- for error messages
             -> [LMatchPat GhcPs]
             -> ([LMatchPat GhcRn] -> RnM (a, FreeVars))
             -> RnM (a, FreeVars)
rnLMatchPats ctxt pats thing_inside
  = do { envs_before <- getRdrEnvs

         -- (1) rename the patterns, bringing into scope all of the term variables
         -- (2) then do the thing inside.
       ; unCpsRn (rnLMatchPatsAndThen (matchNameMaker ctxt) pats) $ \ pats' -> do
       { -- Check for duplicated and shadowed names
         -- Must do this *after* renaming the patterns
         -- See Note [Collect binders only after renaming] in GHC.Hs.Utils
         -- Because we don't bind the vars all at once, we can't
         --    check incrementally for duplicates;
         -- Nor can we check incrementally for shadowing, else we'll
         --    complain *twice* about duplicates e.g. f (x,x) = ...
         --
         -- See note [Don't report shadowing for pattern synonyms]
       ; let bndrs = collectLMatchPatsBinders CollNoDictBinders pats'
       ; addErrCtxt doc_pat $
         if isPatSynCtxt ctxt
            then checkDupNames bndrs
            else checkDupAndShadowedNames envs_before bndrs
       ; thing_inside pats'
       } }
  where
    doc_pat = text "In" <+> pprMatchContext ctxt

rnLMatchPatAndThen :: NameMaker -> LMatchPat GhcPs -> CpsRn (LMatchPat GhcRn)
rnLMatchPatAndThen nm pat = wrapSrcSpanCps (rnMatchPatAndThen nm) pat

rnLMatchPatsAndThen :: NameMaker -> [LMatchPat GhcPs] -> CpsRn ([LMatchPat GhcRn])
rnLMatchPatsAndThen nm = mapM (rnLMatchPatAndThen nm)

rnMatchPatAndThen :: NameMaker -> MatchPat GhcPs -> CpsRn (MatchPat GhcRn)
rnMatchPatAndThen nm (VisPat _ lpat)
  = do { renamed_pat <- rnLPatAndThen nm lpat
       ; return (VisPat NoExtField renamed_pat)
       }
rnMatchPatAndThen nm (InvisTyVarPat _ (L l rdr))
  = do { loc <- liftCps getSrcSpanM
       ; name <- newPatName nm (L (noAnnSrcSpan loc) rdr)
       ; return (InvisTyVarPat NoExtField (L l name))
       }
rnMatchPatAndThen _ (InvisWildTyPat _) = return (InvisWildTyPat NoExtField)

rnLMatchPat :: HsMatchContext GhcRn -- for error messages
            -> LMatchPat GhcPs
            -> (LMatchPat GhcRn -> RnM (a, FreeVars))
            -> RnM (a, FreeVars)
rnLMatchPat ctxt pat thing_inside =
  rnLMatchPats ctxt [pat] (\[pat'] -> thing_inside pat')

rnPat :: HsMatchContext GhcRn -- for error messages
      -> LPat GhcPs
      -> (LPat GhcRn -> RnM (a, FreeVars))
      -> RnM (a, FreeVars)     -- Variables bound by pattern do not
                               -- appear in the result FreeVars
rnPat ctxt pat thing_inside
  = rnLMatchPat ctxt (mkVisMatchPat pat) (\(L _ (VisPat _ pat')) -> thing_inside pat')

applyNameMaker :: NameMaker -> LocatedN RdrName -> RnM (LocatedN Name)
applyNameMaker mk rdr = do { (n, _fvs) <- runCps (newPatLName mk rdr)
                           ; return n }

-- ----------- Entry point 2: rnBindPat -------------------
-- Binds local names; in a recursive scope that involves other bound vars
--      e.g let { (x, Just y) = e1; ... } in ...
--   * does NOT allows type sig to bind type vars
--   * local namemaker
--   * no unused and duplicate checking
--   * fixities might be coming in
rnBindPat :: NameMaker
          -> LPat GhcPs
          -> RnM (LPat GhcRn, FreeVars)
   -- Returned FreeVars are the free variables of the pattern,
   -- of course excluding variables bound by this pattern
rnBindPat name_maker pat = runCps (rnLPatAndThen name_maker pat)

rnBindMatchPat :: NameMaker
               -> LMatchPat GhcPs
               -> RnM (LMatchPat GhcRn, FreeVars)
rnBindMatchPat name_maker pat = runCps (rnLMatchPatAndThen name_maker pat)

{-
*********************************************************
*                                                      *
        The main event
*                                                      *
*********************************************************
-}

-- ----------- Entry point 3: rnLPatAndThen -------------------
-- General version: parametrized by how you make new names

rnLPatsAndThen :: NameMaker -> [LPat GhcPs] -> CpsRn [LPat GhcRn]
rnLPatsAndThen mk = mapM (rnLPatAndThen mk)
  -- Despite the map, the monad ensures that each pattern binds
  -- variables that may be mentioned in subsequent patterns in the list

--------------------
-- The workhorse
rnLPatAndThen :: NameMaker -> LPat GhcPs -> CpsRn (LPat GhcRn)
rnLPatAndThen nm lpat = wrapSrcSpanCps (rnPatAndThen nm) lpat

rnPatAndThen :: NameMaker -> Pat GhcPs -> CpsRn (Pat GhcRn)
rnPatAndThen _  (WildPat _)   = return (WildPat noExtField)
rnPatAndThen mk (ParPat x lpar pat rpar) =
  do { pat' <- rnLPatAndThen mk pat
     ; return (ParPat x lpar pat' rpar) }
rnPatAndThen mk (LazyPat _ pat) = do { pat' <- rnLPatAndThen mk pat
                                     ; return (LazyPat noExtField pat') }
rnPatAndThen mk (BangPat _ pat) = do { pat' <- rnLPatAndThen mk pat
                                     ; return (BangPat noExtField pat') }
rnPatAndThen mk (VarPat x (L l rdr))
    = do { loc <- liftCps getSrcSpanM
         ; name <- newPatName mk (L (noAnnSrcSpan loc) rdr)
         ; return (VarPat x (L l name)) }
     -- we need to bind pattern variables for view pattern expressions
     -- (e.g. in the pattern (x, x -> y) x needs to be bound in the rhs of the tuple)

rnPatAndThen mk (SigPat _ pat sig)
  -- When renaming a pattern type signature (e.g. f (a :: T) = ...), it is
  -- important to rename its type signature _before_ renaming the rest of the
  -- pattern, so that type variables are first bound by the _outermost_ pattern
  -- type signature they occur in. This keeps the type checker happy when
  -- pattern type signatures happen to be nested (#7827)
  --
  -- f ((Just (x :: a) :: Maybe a)
  -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~^       `a' is first bound here
  -- ~~~~~~~~~~~~~~~^                   the same `a' then used here
  = do { sig' <- rnHsPatSigTypeAndThen sig
       ; pat' <- rnLPatAndThen mk pat
       ; return (SigPat noExtField pat' sig' ) }
  where
    rnHsPatSigTypeAndThen :: HsPatSigType GhcPs -> CpsRn (HsPatSigType GhcRn)
    rnHsPatSigTypeAndThen sig = liftCpsWithCont (rnHsPatSigType AlwaysBind PatCtx sig)

rnPatAndThen mk (LitPat x lit)
  | HsString src s <- lit
  = do { ovlStr <- liftCps (xoptM LangExt.OverloadedStrings)
       ; if ovlStr
         then rnPatAndThen mk
                           (mkNPat (noLocA (mkHsIsString src s))
                                      Nothing noAnn)
         else normal_lit }
  | otherwise = normal_lit
  where
    normal_lit = do { liftCps (rnLit lit); return (LitPat x (convertLit lit)) }

rnPatAndThen _ (NPat x (L l lit) mb_neg _eq)
  = do { (lit', mb_neg') <- liftCpsFV $ rnOverLit lit
       ; mb_neg' -- See Note [Negative zero]
           <- let negative = do { (neg, fvs) <- lookupSyntax negateName
                                ; return (Just neg, fvs) }
                  positive = return (Nothing, emptyFVs)
              in liftCpsFV $ case (mb_neg , mb_neg') of
                                  (Nothing, Just _ ) -> negative
                                  (Just _ , Nothing) -> negative
                                  (Nothing, Nothing) -> positive
                                  (Just _ , Just _ ) -> positive
       ; eq' <- liftCpsFV $ lookupSyntax eqName
       ; return (NPat x (L l lit') mb_neg' eq') }

rnPatAndThen mk (NPlusKPat _ rdr (L l lit) _ _ _ )
  = do { new_name <- newPatName mk (l2n rdr)
       ; (lit', _) <- liftCpsFV $ rnOverLit lit -- See Note [Negative zero]
                                                -- We skip negateName as
                                                -- negative zero doesn't make
                                                -- sense in n + k patterns
       ; minus <- liftCpsFV $ lookupSyntax minusName
       ; ge    <- liftCpsFV $ lookupSyntax geName
       ; return (NPlusKPat noExtField (L (noAnnSrcSpan $ nameSrcSpan new_name) new_name)
                                      (L l lit') lit' ge minus) }
                -- The Report says that n+k patterns must be in Integral

rnPatAndThen mk (AsPat _ rdr pat)
  = do { new_name <- newPatLName mk rdr
       ; pat' <- rnLPatAndThen mk pat
       ; return (AsPat noExtField new_name pat') }

rnPatAndThen mk p@(ViewPat _ expr pat)
  = do { liftCps $ do { vp_flag <- xoptM LangExt.ViewPatterns
                      ; checkErr vp_flag (TcRnIllegalViewPattern p) }
         -- Because of the way we're arranging the recursive calls,
         -- this will be in the right context
       ; expr' <- liftCpsFV $ rnLExpr expr
       ; pat' <- rnLPatAndThen mk pat
       -- Note: at this point the PreTcType in ty can only be a placeHolder
       -- ; return (ViewPat expr' pat' ty) }

       -- Note: we can't cook up an inverse for an arbitrary view pattern,
       -- so we pass 'Nothing'.
       ; return (ViewPat Nothing expr' pat') }

rnPatAndThen mk (ConPat _ con args)
   -- rnConPatAndThen takes care of reconstructing the pattern
   -- The pattern for the empty list needs to be replaced by an empty explicit list pattern when overloaded lists is turned on.
  = case unLoc con == nameRdrName (dataConName nilDataCon) of
      True    -> do { ol_flag <- liftCps $ xoptM LangExt.OverloadedLists
                    ; if ol_flag then rnPatAndThen mk (ListPat noAnn [])
                                 else rnConPatAndThen mk con args}
      False   -> rnConPatAndThen mk con args

rnPatAndThen mk (ListPat _ pats)
  = do { opt_OverloadedLists  <- liftCps $ xoptM LangExt.OverloadedLists
       ; pats' <- rnLPatsAndThen mk pats
       ; if not opt_OverloadedLists
         then return (ListPat noExtField pats')
         else
    -- If OverloadedLists is enabled, desugar to a view pattern.
    -- See Note [Desugaring overloaded list patterns]
    do { (to_list_name,_)     <- liftCps $ lookupSyntaxName toListName
       -- Use 'fromList' as proof of invertibility of the view pattern.
       -- See Note [Invertible view patterns] in GHC.Tc.TyCl.PatSyn
       ; (from_list_n_name,_) <- liftCps $ lookupSyntaxName fromListNName
       ; let
           lit_n   = mkIntegralLit (length pats)
           hs_lit  = genHsIntegralLit lit_n
           inverse = genHsApps from_list_n_name [hs_lit]
           rn_list_pat  = ListPat noExtField pats'
           exp_expr     = genLHsVar to_list_name
           exp_list_pat = ViewPat (Just inverse) exp_expr (wrapGenSpan rn_list_pat)
       ; return $ mkExpandedPat rn_list_pat exp_list_pat }}

rnPatAndThen mk (TuplePat _ pats boxed)
  = do { pats' <- rnLPatsAndThen mk pats
       ; return (TuplePat noExtField pats' boxed) }

rnPatAndThen mk (SumPat _ pat alt arity)
  = do { pat <- rnLPatAndThen mk pat
       ; return (SumPat noExtField pat alt arity)
       }

-- If a splice has been run already, just rename the result.
rnPatAndThen mk (SplicePat x (HsSpliced x2 mfs (HsSplicedPat pat)))
  = SplicePat x . HsSpliced x2 mfs . HsSplicedPat <$> rnPatAndThen mk pat

rnPatAndThen mk (SplicePat _ splice)
  = do { eith <- liftCpsFV $ rnSplicePat splice
       ; case eith of   -- See Note [rnSplicePat] in GHC.Rename.Splice
           Left  not_yet_renamed -> rnPatAndThen mk not_yet_renamed
           Right already_renamed -> return already_renamed }

--------------------
rnConPatAndThen :: NameMaker
                -> LocatedN RdrName    -- the constructor
                -> HsConPatDetails GhcPs
                -> CpsRn (Pat GhcRn)

rnConPatAndThen mk con (PrefixCon tyargs pats)
  = do  { con' <- lookupConCps con
        ; liftCps check_lang_exts
        ; tyargs' <- forM tyargs $ \t ->
            liftCpsWithCont $ rnHsPatSigTypeBindingVars HsTypeCtx t
        ; pats' <- rnLPatsAndThen mk pats
        ; return $ ConPat
            { pat_con_ext = noExtField
            , pat_con = con'
            , pat_args = PrefixCon tyargs' pats'
            }
        }
  where
    check_lang_exts :: RnM ()
    check_lang_exts = do
      scoped_tyvars <- xoptM LangExt.ScopedTypeVariables
      type_app      <- xoptM LangExt.TypeApplications
      unless (scoped_tyvars && type_app) $
        case listToMaybe tyargs of
          Nothing    -> pure ()
          Just tyarg -> addErr $ TcRnUnknownMessage $ mkPlainError noHints $
            hang (text "Illegal visible type application in a pattern:"
                    <+> quotes (char '@' <> ppr tyarg))
               2 (text "Both ScopedTypeVariables and TypeApplications are"
                    <+> text "required to use this feature")

rnConPatAndThen mk con (InfixCon pat1 pat2)
  = do  { con' <- lookupConCps con
        ; pat1' <- rnLPatAndThen mk pat1
        ; pat2' <- rnLPatAndThen mk pat2
        ; fixity <- liftCps $ lookupFixityRn (unLoc con')
        ; liftCps $ mkConOpPatRn con' fixity pat1' pat2' }

rnConPatAndThen mk con (RecCon rpats)
  = do  { con' <- lookupConCps con
        ; rpats' <- rnHsRecPatsAndThen mk con' rpats
        ; return $ ConPat
            { pat_con_ext = noExtField
            , pat_con = con'
            , pat_args = RecCon rpats'
            }
        }

checkUnusedRecordWildcardCps :: SrcSpan -> Maybe [Name] -> CpsRn ()
checkUnusedRecordWildcardCps loc dotdot_names =
  CpsRn (\thing -> do
                    (r, fvs) <- thing ()
                    checkUnusedRecordWildcard loc fvs dotdot_names
                    return (r, fvs) )
--------------------
rnHsRecPatsAndThen :: NameMaker
                   -> LocatedN Name      -- Constructor
                   -> HsRecFields GhcPs (LPat GhcPs)
                   -> CpsRn (HsRecFields GhcRn (LPat GhcRn))
rnHsRecPatsAndThen mk (L _ con)
     hs_rec_fields@(HsRecFields { rec_dotdot = dd })
  = do { flds <- liftCpsFV $ rnHsRecFields (HsRecFieldPat con) mkVarPat
                                            hs_rec_fields
       ; flds' <- mapM rn_field (flds `zip` [1..])
       ; check_unused_wildcard (implicit_binders flds' <$> dd)
       ; return (HsRecFields { rec_flds = flds', rec_dotdot = dd }) }
  where
    mkVarPat l n = VarPat noExtField (L (noAnnSrcSpan l) n)
    rn_field (L l fld, n') =
      do { arg' <- rnLPatAndThen (nested_mk dd mk n') (hfbRHS fld)
         ; return (L l (fld { hfbRHS = arg' })) }

    loc = maybe noSrcSpan getLoc dd

    -- Get the arguments of the implicit binders
    implicit_binders fs (unLoc -> n) = collectPatsBinders CollNoDictBinders implicit_pats
      where
        implicit_pats = map (hfbRHS . unLoc) (drop n fs)

    -- Don't warn for let P{..} = ... in ...
    check_unused_wildcard = case mk of
                              LetMk{} -> const (return ())
                              LamMk{} -> checkUnusedRecordWildcardCps loc

        -- Suppress unused-match reporting for fields introduced by ".."
    nested_mk Nothing  mk                    _  = mk
    nested_mk (Just _) mk@(LetMk {})         _  = mk
    nested_mk (Just (unLoc -> n)) (LamMk report_unused) n'
      = LamMk (report_unused && (n' <= n))


{- *********************************************************************
*                                                                      *
              Generating code for HsPatExpanded
      See Note [Handling overloaded and rebindable constructs]
*                                                                      *
********************************************************************* -}

-- | Build a 'HsPatExpansion' out of an extension constructor,
--   and the two components of the expansion: original and
--   desugared patterns
mkExpandedPat
  :: Pat GhcRn -- ^ source pattern
  -> Pat GhcRn -- ^ expanded pattern
  -> Pat GhcRn -- ^ suitably wrapped 'HsPatExpansion'
mkExpandedPat a b = XPat (HsPatExpanded a b)

{-
************************************************************************
*                                                                      *
        Record fields
*                                                                      *
************************************************************************
-}

data HsRecFieldContext
  = HsRecFieldCon Name
  | HsRecFieldPat Name
  | HsRecFieldUpd

rnHsRecFields
    :: forall arg.
       HsRecFieldContext
    -> (SrcSpan -> RdrName -> arg)
         -- When punning, use this to build a new field
    -> HsRecFields GhcPs (LocatedA arg)
    -> RnM ([LHsRecField GhcRn (LocatedA arg)], FreeVars)

-- This surprisingly complicated pass
--   a) looks up the field name (possibly using disambiguation)
--   b) fills in puns and dot-dot stuff
-- When we've finished, we've renamed the LHS, but not the RHS,
-- of each x=e binding
--
-- This is used for record construction and pattern-matching, but not updates.

rnHsRecFields ctxt mk_arg (HsRecFields { rec_flds = flds, rec_dotdot = dotdot })
  = do { pun_ok      <- xoptM LangExt.NamedFieldPuns
       ; disambig_ok <- xoptM LangExt.DisambiguateRecordFields
       ; let parent = guard disambig_ok >> mb_con
       ; flds1  <- mapM (rn_fld pun_ok parent) flds
       ; mapM_ (addErr . dupFieldErr ctxt) dup_flds
       ; dotdot_flds <- rn_dotdot dotdot mb_con flds1
       ; let all_flds | null dotdot_flds = flds1
                      | otherwise        = flds1 ++ dotdot_flds
       ; return (all_flds, mkFVs (getFieldIds all_flds)) }
  where
    mb_con = case ctxt of
                HsRecFieldCon con  -> Just con
                HsRecFieldPat con  -> Just con
                _ {- update -}     -> Nothing

    rn_fld :: Bool -> Maybe Name -> LHsRecField GhcPs (LocatedA arg)
           -> RnM (LHsRecField GhcRn (LocatedA arg))
    rn_fld pun_ok parent (L l
                           (HsFieldBind
                              { hfbLHS =
                                  (L loc (FieldOcc _ (L ll lbl)))
                              , hfbRHS = arg
                              , hfbPun      = pun }))
      = do { sel <- setSrcSpanA loc $ lookupRecFieldOcc parent lbl
           ; arg' <- if pun
                     then do { checkErr pun_ok (TcRnIllegalFieldPunning (L (locA loc) lbl))
                               -- Discard any module qualifier (#11662)
                             ; let arg_rdr = mkRdrUnqual (rdrNameOcc lbl)
                             ; return (L (l2l loc) (mk_arg (locA loc) arg_rdr)) }
                     else return arg
           ; return (L l (HsFieldBind
                             { hfbAnn = noAnn
                             , hfbLHS = (L loc (FieldOcc sel (L ll lbl)))
                             , hfbRHS = arg'
                             , hfbPun      = pun })) }


    rn_dotdot :: Maybe (Located Int)      -- See Note [DotDot fields] in GHC.Hs.Pat
              -> Maybe Name -- The constructor (Nothing for an
                                --    out of scope constructor)
              -> [LHsRecField GhcRn (LocatedA arg)] -- Explicit fields
              -> RnM ([LHsRecField GhcRn (LocatedA arg)])   -- Field Labels we need to fill in
    rn_dotdot (Just (L loc n)) (Just con) flds -- ".." on record construction / pat match
      | not (isUnboundName con) -- This test is because if the constructor
                                -- isn't in scope the constructor lookup will add
                                -- an error but still return an unbound name. We
                                -- don't want that to screw up the dot-dot fill-in stuff.
      = assert (flds `lengthIs` n) $
        do { dd_flag <- xoptM LangExt.RecordWildCards
           ; checkErr dd_flag (needFlagDotDot ctxt)
           ; (rdr_env, lcl_env) <- getRdrEnvs
           ; con_fields <- lookupConstructorFields con
           ; when (null con_fields) (addErr (TcRnIllegalWildcardsInConstructor con))
           ; let present_flds = mkOccSet $ map rdrNameOcc (getFieldLbls flds)

                   -- For constructor uses (but not patterns)
                   -- the arg should be in scope locally;
                   -- i.e. not top level or imported
                   -- Eg.  data R = R { x,y :: Int }
                   --      f x = R { .. }   -- Should expand to R {x=x}, not R{x=x,y=y}
                 arg_in_scope lbl = mkRdrUnqual lbl `elemLocalRdrEnv` lcl_env

                 (dot_dot_fields, dot_dot_gres)
                        = unzip [ (fl, gre)
                                | fl <- con_fields
                                , let lbl = mkVarOccFS (flLabel fl)
                                , not (lbl `elemOccSet` present_flds)
                                , Just gre <- [lookupGRE_FieldLabel rdr_env fl]
                                              -- Check selector is in scope
                                , case ctxt of
                                    HsRecFieldCon {} -> arg_in_scope lbl
                                    _other           -> True ]

           ; addUsedGREs dot_dot_gres
           ; let locn = noAnnSrcSpan loc
           ; return [ L (noAnnSrcSpan loc) (HsFieldBind
                        { hfbAnn = noAnn
                        , hfbLHS
                           = L (noAnnSrcSpan loc) (FieldOcc sel (L (noAnnSrcSpan loc) arg_rdr))
                        , hfbRHS = L locn (mk_arg loc arg_rdr)
                        , hfbPun      = False })
                    | fl <- dot_dot_fields
                    , let sel     = flSelector fl
                    , let arg_rdr = mkVarUnqual (flLabel fl) ] }

    rn_dotdot _dotdot _mb_con _flds
      = return []
      -- _dotdot = Nothing => No ".." at all
      -- _mb_con = Nothing => Record update
      -- _mb_con = Just unbound => Out of scope data constructor

    dup_flds :: [NE.NonEmpty RdrName]
        -- Each list represents a RdrName that occurred more than once
        -- (the list contains all occurrences)
        -- Each list in dup_fields is non-empty
    (_, dup_flds) = removeDups compare (getFieldLbls flds)


-- NB: Consider this:
--      module Foo where { data R = R { fld :: Int } }
--      module Odd where { import Foo; fld x = x { fld = 3 } }
-- Arguably this should work, because the reference to 'fld' is
-- unambiguous because there is only one field id 'fld' in scope.
-- But currently it's rejected.

rnHsRecUpdFields
    :: [LHsRecUpdField GhcPs]
    -> RnM ([LHsRecUpdField GhcRn], FreeVars)
rnHsRecUpdFields flds
  = do { pun_ok        <- xoptM LangExt.NamedFieldPuns
       ; dup_fields_ok <- xopt_DuplicateRecordFields <$> getDynFlags
       ; (flds1, fvss) <- mapAndUnzipM (rn_fld pun_ok dup_fields_ok) flds
       ; mapM_ (addErr . dupFieldErr HsRecFieldUpd) dup_flds

       -- Check for an empty record update  e {}
       -- NB: don't complain about e { .. }, because rn_dotdot has done that already
       ; when (null flds) $ addErr TcRnEmptyRecordUpdate

       ; return (flds1, plusFVs fvss) }
  where
    rn_fld :: Bool -> DuplicateRecordFields -> LHsRecUpdField GhcPs
           -> RnM (LHsRecUpdField GhcRn, FreeVars)
    rn_fld pun_ok dup_fields_ok (L l (HsFieldBind { hfbLHS = L loc f
                                                  , hfbRHS = arg
                                                  , hfbPun      = pun }))
      = do { let lbl = rdrNameAmbiguousFieldOcc f
           ; mb_sel <- setSrcSpanA loc $
                      -- Defer renaming of overloaded fields to the typechecker
                      -- See Note [Disambiguating record fields] in GHC.Tc.Gen.Head
                      lookupRecFieldOcc_update dup_fields_ok lbl
           ; arg' <- if pun
                     then do { checkErr pun_ok (TcRnIllegalFieldPunning (L (locA loc) lbl))
                               -- Discard any module qualifier (#11662)
                             ; let arg_rdr = mkRdrUnqual (rdrNameOcc lbl)
                             ; return (L (l2l loc) (HsVar noExtField
                                              (L (l2l loc) arg_rdr))) }
                     else return arg
           ; (arg'', fvs) <- rnLExpr arg'

           ; let (lbl', fvs') = case mb_sel of
                   UnambiguousGre gname -> let sel_name = greNameMangledName gname
                                           in (Unambiguous sel_name (L (l2l loc) lbl), fvs `addOneFV` sel_name)
                   AmbiguousFields       -> (Ambiguous   noExtField (L (l2l loc) lbl), fvs)

           ; return (L l (HsFieldBind { hfbAnn = noAnn
                                      , hfbLHS = L loc lbl'
                                      , hfbRHS = arg''
                                      , hfbPun = pun }), fvs') }

    dup_flds :: [NE.NonEmpty RdrName]
        -- Each list represents a RdrName that occurred more than once
        -- (the list contains all occurrences)
        -- Each list in dup_fields is non-empty
    (_, dup_flds) = removeDups compare (getFieldUpdLbls flds)



getFieldIds :: [LHsRecField GhcRn arg] -> [Name]
getFieldIds flds = map (hsRecFieldSel . unLoc) flds

getFieldLbls :: forall p arg . UnXRec p => [LHsRecField p arg] -> [RdrName]
getFieldLbls flds
  = map (unXRec @p . foLabel . unXRec @p . hfbLHS . unXRec @p) flds

getFieldUpdLbls :: [LHsRecUpdField GhcPs] -> [RdrName]
getFieldUpdLbls flds = map (rdrNameAmbiguousFieldOcc . unLoc . hfbLHS . unLoc) flds

needFlagDotDot :: HsRecFieldContext -> TcRnMessage
needFlagDotDot = TcRnIllegalWildcardsInRecord . toRecordFieldPart

dupFieldErr :: HsRecFieldContext -> NE.NonEmpty RdrName -> TcRnMessage
dupFieldErr ctxt = TcRnDuplicateFieldName (toRecordFieldPart ctxt)

toRecordFieldPart :: HsRecFieldContext -> RecordFieldPart
toRecordFieldPart (HsRecFieldCon n)  = RecordFieldConstructor n
toRecordFieldPart (HsRecFieldPat n)  = RecordFieldPattern     n
toRecordFieldPart (HsRecFieldUpd {}) = RecordFieldUpdate

{-
************************************************************************
*                                                                      *
\subsubsection{Literals}
*                                                                      *
************************************************************************

When literals occur we have to make sure
that the types and classes they involve
are made available.
-}

rnLit :: HsLit p -> RnM ()
rnLit (HsChar _ c) = checkErr (inCharRange c) (TcRnCharLiteralOutOfRange c)
rnLit _ = return ()

-- | Turn a Fractional-looking literal which happens to be an integer into an
-- Integer-looking literal.
-- We only convert numbers where the exponent is between 0 and 100 to avoid
-- converting huge numbers and incurring long compilation times. See #15646.
generalizeOverLitVal :: OverLitVal -> OverLitVal
generalizeOverLitVal (HsFractional fl@(FL {fl_text=src,fl_neg=neg,fl_exp=e}))
    | e >= -100 && e <= 100
    , let val = rationalFromFractionalLit fl
    , denominator val == 1 = HsIntegral (IL {il_text=src,il_neg=neg,il_value=numerator val})
generalizeOverLitVal lit = lit

isNegativeZeroOverLit :: HsOverLit t -> Bool
isNegativeZeroOverLit lit
 = case ol_val lit of
        HsIntegral i    -> 0 == il_value i && il_neg i
        -- For HsFractional, the value of fl is n * (b ^^ e) so it is sufficient
        -- to check if n = 0. b is equal to either 2 or 10. We don't call
        -- rationalFromFractionalLit here as it is expensive when e is big.
        HsFractional fl -> 0 == fl_signi fl && fl_neg fl
        _               -> False

{-
Note [Negative zero]
~~~~~~~~~~~~~~~~~~~~~~~~~
There were problems with negative zero in conjunction with Negative Literals
extension. Numeric literal value is contained in Integer and Rational types
inside IntegralLit and FractionalLit. These types cannot represent negative
zero value. So we had to add explicit field 'neg' which would hold information
about literal sign. Here in rnOverLit we use it to detect negative zeroes and
in this case return not only literal itself but also negateName so that users
can apply it explicitly. In this case it stays negative zero.  #13211
-}

rnOverLit :: HsOverLit t ->
             RnM ((HsOverLit GhcRn, Maybe (HsExpr GhcRn)), FreeVars)
rnOverLit origLit
  = do  { opt_NumDecimals <- xoptM LangExt.NumDecimals
        ; let { lit@(OverLit {ol_val=val})
            | opt_NumDecimals = origLit {ol_val = generalizeOverLitVal (ol_val origLit)}
            | otherwise       = origLit
          }
        ; let std_name = hsOverLitName val
        ; (from_thing_name, fvs1) <- lookupSyntaxName std_name
        ; let rebindable = from_thing_name /= std_name
              lit' = lit { ol_ext = OverLitRn { ol_rebindable = rebindable
                                              , ol_from_fun = noLocA from_thing_name } }
        ; if isNegativeZeroOverLit lit'
          then do { (negate_name, fvs2) <- lookupSyntaxExpr negateName
                  ; return ((lit' { ol_val = negateOverLitVal val }, Just negate_name)
                                  , fvs1 `plusFV` fvs2) }
          else return ((lit', Nothing), fvs1) }
