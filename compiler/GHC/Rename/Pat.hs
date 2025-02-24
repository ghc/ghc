{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE DisambiguateRecordFields   #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

{-
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Renaming of patterns

Basically dependency analysis.

Handles @Match@, @GRHSs@, @HsExpr@, and @Qualifier@ datatypes.  In
general, all of these functions return a renamed thing, and a set of
free variables.
-}
module GHC.Rename.Pat (-- main entry points
              rnPat, rnPats, rnBindPat,

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
import {-# SOURCE #-} GHC.Rename.Splice ( rnSplicePat, rnSpliceTyPat )

import GHC.Hs
import GHC.Tc.Errors.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Utils.TcMType ( hsOverLitName )
import GHC.Rename.Doc (rnLHsDoc)
import GHC.Rename.Env
import GHC.Rename.Fixity
import GHC.Rename.Utils    ( newLocalBndrRn, bindLocalNames
                           , warnUnusedMatches, newLocalBndrRn
                           , checkUnusedRecordWildcard
                           , checkDupNames, checkDupAndShadowedNames
                           , wrapGenSpan, genHsApps, genLHsVar, genHsIntegralLit, delLocalNames, typeAppErr )
import GHC.Rename.HsType
import GHC.Builtin.Names

import GHC.Types.Hint
import GHC.Types.Fixity (LexicalFixity(..))
import GHC.Types.Name
import GHC.Types.Name.Set
import GHC.Types.Name.Reader
import GHC.Types.Unique.Set
import GHC.Types.Basic
import GHC.Types.SourceText

import GHC.Data.FastString ( uniqCompareFS )
import GHC.Data.List.SetOps( removeDups )

import GHC.Utils.Misc
import GHC.Utils.Panic.Plain
import GHC.Types.SrcLoc
import GHC.Types.Literal   ( inCharRange )
import GHC.Types.GREInfo   ( ConInfo(..), conInfoFields, ConFieldInfo (..) )
import GHC.Builtin.Types   ( nilDataCon )
import GHC.Core.DataCon
import GHC.Core.TyCon      ( isKindName )
import qualified GHC.LanguageExtensions as LangExt

import Control.Monad       ( when, ap, guard, unless )
import Data.Foldable
import Data.Function       ( on )
import Data.Functor.Identity ( Identity (..) )
import qualified Data.List.NonEmpty as NE
import Data.Ratio
import Control.Monad.Trans.Writer.CPS
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Functor ((<&>))
import Data.Coerce

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

matchNameMaker :: HsMatchContext fn -> NameMaker
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
        do { name <- newLocalBndrRn rdr_name
           ; (res, fvs) <- bindLocalNames [name] (thing_inside name)
           ; when report_unused $ warnUnusedMatches [name] fvs
           ; return (res, name `delFV` fvs) })

newPatName (LetMk is_top fix_env) rdr_name
  = CpsRn (\ thing_inside ->
        do { name <- case is_top of
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
Overloaded patterns and rebindable patterns are desugared in the renamer
using the HsPatExpansion mechanism detailed in:
Note [Rebindable syntax and XXExprGhcRn]
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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
If OverloadedLists is enabled, we desugar a list pattern to a view pattern:

  [p1, p2, p3]
==>
  toList -> [p1, p2, p3]

This happens directly in the renamer, using the HsPatExpansion mechanism
detailed in Note [Rebindable syntax and XXExprGhcRn].

Note that we emit a special view pattern: we additionally keep track of an
inverse to the pattern.
See Note [Invertible view patterns] in GHC.Tc.TyCl.PatSyn for details.

== Wrinkle ==

This is all fine, except in one very specific case:
When the type being matched on is already a list type, so that the
pattern looks like
     toList @[ty] dict -> pat
then we know for certain that `toList` is an identity function, so we can
behave exactly as if the pattern was just `pat`.  This is important when
we have `OverloadedLists`.  For example (#14547, #25257)

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
(NB: Because of -XRebindableSyntax we have to check that the `toList` we see is
actually resolved to `GHC.Exts.toList`.)

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

-- rn_pats_general is the generalisation of two functions:
--    rnPats, rnPat
-- Those are the only call sites, so we inline it for improved performance.
-- Kind of like a macro.
{-# INLINE rn_pats_general #-}
rn_pats_general :: Traversable f => HsMatchContextRn
  -> f (LPat GhcPs)
  -> (f (LPat GhcRn) -> RnM (r, FreeVars))
  -> RnM (r, FreeVars)
rn_pats_general ctxt pats thing_inside = do
  envs_before <- getRdrEnvs

  -- (1) rename the patterns, bringing into scope all of the term variables
  -- (2) then do the thing inside.
  unCpsRn (rn_pats_fun (matchNameMaker ctxt) pats) $ \ pats' -> do
    -- Check for duplicated and shadowed names
    -- Must do this *after* renaming the patterns
    -- See Note [Collect binders only after renaming] in GHC.Hs.Utils
    -- Because we don't bind the vars all at once, we can't
    --    check incrementally for duplicates;
    -- Nor can we check incrementally for shadowing, else we'll
    --    complain *twice* about duplicates e.g. f (x,x) = ...
    --
    -- See Note [Don't report shadowing for pattern synonyms]
    let bndrs = collectPatsBinders CollVarTyVarBinders (toList pats')
    addErrCtxt (MatchCtxt ctxt) $
      if isPatSynCtxt ctxt
         then checkDupNames bndrs
         else checkDupAndShadowedNames envs_before bndrs
    thing_inside pats'
  where

    -- See Note [Invisible binders in functions] in GHC.Hs.Pat
    --
    -- BTW, Or-patterns would be awesome here
    rn_pats_fun = case ctxt of
      FunRhs{} -> mapM . rnLArgPatAndThen
      LamAlt LamSingle -> mapM . rnLArgPatAndThen
      LamAlt LamCases -> mapM . rnLArgPatAndThen
      _ -> mapM . rnLPatAndThen

rnPats :: HsMatchContextRn   -- For error messages and choosing if @-patterns are allowed
       -> [LPat GhcPs]
       -> ([LPat GhcRn] -> RnM (a, FreeVars))
       -> RnM (a, FreeVars)
rnPats = rn_pats_general

rnPat :: forall a. HsMatchContextRn      -- For error messages and choosing if @-patterns are allowed
      -> LPat GhcPs
      -> (LPat GhcRn -> RnM (a, FreeVars))
      -> RnM (a, FreeVars)     -- Variables bound by pattern do not
                               -- appear in the result FreeVars
rnPat
       = coerce (rn_pats_general @Identity @a)

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

{-
*********************************************************
*                                                      *
        The main event
*                                                      *
*********************************************************
-}


rnLArgPatAndThen :: NameMaker -> LocatedA (Pat GhcPs) -> CpsRn (LocatedA (Pat GhcRn))
rnLArgPatAndThen mk = wrapSrcSpanCps rnArgPatAndThen where

  rnArgPatAndThen (InvisPat (_, spec) tp) = do
    tp' <- rnHsTyPat HsTypePatCtx tp
    liftCps $ unlessXOptM LangExt.TypeAbstractions $
      addErr (TcRnIllegalInvisibleTypePattern tp' InvisPatWithoutFlag)
    pure (InvisPat spec tp')
  rnArgPatAndThen p = rnPatAndThen mk p

-- ----------- Entry point 3: rnLPatAndThen -------------------
-- General version: parameterized by how you make new names

rnLPatsAndThen :: Traversable f => NameMaker -> f (LPat GhcPs) -> CpsRn (f (LPat GhcRn))
rnLPatsAndThen mk = traverse (rnLPatAndThen mk)
  -- Despite the map, the monad ensures that each pattern binds
  -- variables that may be mentioned in subsequent patterns in the list
{-# SPECIALISE rnLPatsAndThen :: NameMaker -> [LPat GhcPs] -> CpsRn [LPat GhcRn] #-}
{-# SPECIALISE rnLPatsAndThen :: NameMaker -> NE.NonEmpty (LPat GhcPs) -> CpsRn (NE.NonEmpty (LPat GhcRn)) #-}

--------------------
-- The workhorse
rnLPatAndThen :: NameMaker -> LPat GhcPs -> CpsRn (LPat GhcRn)
rnLPatAndThen nm lpat = wrapSrcSpanCps (rnPatAndThen nm) lpat

rnPatAndThen :: NameMaker -> Pat GhcPs -> CpsRn (Pat GhcRn)
rnPatAndThen _  (WildPat _)   = return (WildPat noExtField)
rnPatAndThen mk (ParPat _ pat) =
  do { pat' <- rnLPatAndThen mk pat
     ; return (ParPat noExtField pat') }
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
  = do { new_name <- newPatName mk (la2la rdr)
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

rnPatAndThen mk (OrPat _ pats)
  = do { loc <- liftCps getSrcSpanM
       ; pats' <- rnLPatsAndThen mk pats
       ; let bndrs = collectPatsBinders CollVarTyVarBinders (NE.toList pats')
       ; liftCps $ setSrcSpan loc $ checkErr (null bndrs) $
           TcRnOrPatBindsVariables (NE.fromList (ordNubOn getOccName bndrs))
       ; return (OrPat noExtField pats') }

rnPatAndThen mk (SumPat _ pat alt arity)
  = do { pat <- rnLPatAndThen mk pat
       ; return (SumPat noExtField pat alt arity)
       }

rnPatAndThen mk (SplicePat _ splice)
  = do { eith <- liftCpsFV $ rnSplicePat splice
       ; case eith of   -- See Note [rnSplicePat] in GHC.Rename.Splice
           (rn_splice, HsUntypedSpliceTop mfs pat) -> -- Splice was top-level and thus run, creating Pat GhcPs
               gParPat . (fmap (flip SplicePat rn_splice . HsUntypedSpliceTop mfs)) <$> rnLPatAndThen mk pat
           (rn_splice, HsUntypedSpliceNested splice_name) -> return (SplicePat (HsUntypedSpliceNested splice_name) rn_splice) -- Splice was nested and thus already renamed
       }

rnPatAndThen _ (EmbTyPat _ tp)
  = do { tp' <- rnHsTyPat HsTypePatCtx tp
       ; return (EmbTyPat noExtField tp') }
rnPatAndThen _ (InvisPat (_, spec) tp)
  = do { -- Invisible patterns are handled in `rnLArgPatAndThen`
         -- so unconditionally emit error here
       ; tp' <- rnHsTyPat HsTypePatCtx tp
       ; liftCps $ addErr (TcRnIllegalInvisibleTypePattern tp' InvisPatMisplaced)
       ; return (InvisPat spec tp')
       }

--------------------
rnConPatAndThen :: NameMaker
                -> LocatedN RdrName    -- the constructor
                -> HsConPatDetails GhcPs
                -> CpsRn (Pat GhcRn)

rnConPatAndThen mk con (PrefixCon tyargs pats)
  = do  { con' <- lookupConCps con
        ; tyargs' <- mapM rnConPatTyArg tyargs
        ; pats' <- rnLPatsAndThen mk pats
        ; return $ ConPat
            { pat_con_ext = noExtField
            , pat_con = con'
            , pat_args = PrefixCon tyargs' pats'
            }
        }
  where
    rnConPatTyArg (HsConPatTyArg _ tp) = do
      tp' <- rnHsTyPat HsTypePatCtx tp
      liftCps $ unlessXOptM LangExt.TypeAbstractions $
        addErr (TcRnIllegalInvisibleTypePattern tp' InvisPatWithoutFlag)
      return (HsConPatTyArg noExtField tp')

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
checkUnusedRecordWildcardCps :: SrcSpan
                             -> Maybe [ImplicitFieldBinders]
                             -> CpsRn ()
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
       ; check_unused_wildcard (lHsRecFieldsImplicits flds' <$> unLoc <$> dd)
       ; return (HsRecFields { rec_ext = noExtField, rec_flds = flds', rec_dotdot = dd }) }
  where
    mkVarPat l n = VarPat noExtField (L (noAnnSrcSpan l) n)
    rn_field (L l fld, n') =
      do { arg' <- rnLPatAndThen (nested_mk dd mk (RecFieldsDotDot n')) (hfbRHS fld)
         ; return (L l (fld { hfbRHS = arg' })) }

    loc = maybe noSrcSpan getLocA dd

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
                HsRecFieldUpd      -> Nothing

    rn_fld :: Bool -> Maybe Name -> LHsRecField GhcPs (LocatedA arg)
           -> RnM (LHsRecField GhcRn (LocatedA arg))
    rn_fld pun_ok parent (L l
                           (HsFieldBind
                              { hfbLHS = L loc (FieldOcc _ (L ll lbl))
                              , hfbRHS = arg
                              , hfbPun = pun }))
      = do { sel <- setSrcSpanA loc $ lookupRecFieldOcc parent lbl
           ; let arg_rdr = mkRdrUnqual $ recFieldToVarOcc $ occName sel
                 -- Discard any module qualifier (#11662)
           ; arg' <- if pun
                     then do { checkErr pun_ok $
                                TcRnIllegalFieldPunning (L (locA loc) arg_rdr)
                             ; return $ L (l2l loc) $
                                 mk_arg (locA loc) arg_rdr }
                     else return arg
           ; return $ L l $
               HsFieldBind
                 { hfbAnn = noAnn
                 , hfbLHS = L loc (FieldOcc arg_rdr (L ll sel))
                 , hfbRHS = arg'
                 , hfbPun = pun } }

    rn_dotdot :: Maybe (LocatedE RecFieldsDotDot)     -- See Note [DotDot fields] in GHC.Hs.Pat
              -> Maybe Name -- The constructor (Nothing for an
                                --    out of scope constructor)
              -> [LHsRecField GhcRn (LocatedA arg)] -- Explicit fields
              -> RnM [LHsRecField GhcRn (LocatedA arg)]   -- Field Labels we need to fill in
    rn_dotdot (Just (L loc_e (RecFieldsDotDot n))) (Just con) flds -- ".." on record construction / pat match
      | not (isUnboundName con) -- This test is because if the constructor
                                -- isn't in scope the constructor lookup will add
                                -- an error but still return an unbound name. We
                                -- don't want that to screw up the dot-dot fill-in stuff.
      = assert (flds `lengthIs` n) $
        do { dd_flag <- xoptM LangExt.RecordWildCards
           ; checkErr dd_flag (needFlagDotDot ctxt)
           ; (rdr_env, lcl_env) <- getRdrEnvs
           ; conInfo <- lookupConstructorInfo con
           ; when (conFieldInfo conInfo == ConHasPositionalArgs) (addErr (TcRnIllegalWildcardsInConstructor con))
           ; let present_flds = mkOccSet $ map rdrNameOcc (getFieldRdrs flds)

                   -- For constructor uses (but not patterns)
                   -- the arg should be in scope locally;
                   -- i.e. not top level or imported
                   -- Eg.  data R = R { x,y :: Int }
                   --      f x = R { .. }   -- Should expand to R {x=x}, not R{x=x,y=y}
                 arg_in_scope lbl = mkRdrUnqual lbl `elemLocalRdrEnv` lcl_env

                 (dot_dot_fields, dot_dot_gres) =
                   unzip [ (fl, gre)
                         | fl <- conInfoFields conInfo
                         , let lbl = recFieldToVarOcc $ occName $ flSelector fl
                         , not (lbl `elemOccSet` present_flds)
                         , Just gre <- [lookupGRE_FieldLabel rdr_env fl]
                                       -- Check selector is in scope
                         , case ctxt of
                             HsRecFieldCon {} -> arg_in_scope lbl
                             _other           -> True ]

           ; addUsedGREs NoDeprecationWarnings dot_dot_gres
           ; let loc = locA loc_e
           ; let locn = noAnnSrcSpan loc
           ; return [ L (noAnnSrcSpan loc) (HsFieldBind
                        { hfbAnn = noAnn
                        , hfbLHS
                           = L (noAnnSrcSpan loc) (FieldOcc arg_rdr (L (noAnnSrcSpan loc) sel))
                        , hfbRHS = L locn (mk_arg loc arg_rdr)
                        , hfbPun = False })
                    | fl <- dot_dot_fields
                    , let sel     = flSelector fl
                          arg_rdr = mkRdrUnqual
                                  $ recFieldToVarOcc
                                  $ nameOccName sel ] }

    rn_dotdot _dotdot _mb_con _flds
      = return []
      -- _dotdot = Nothing => No ".." at all
      -- _mb_con = Nothing => Record update
      -- _mb_con = Just unbound => Out of scope data constructor

    dup_flds :: [NE.NonEmpty RdrName]
        -- Each list represents a RdrName that occurred more than once
        -- (the list contains all occurrences)
        -- Each list in dup_fields is non-empty
    (_, dup_flds) = removeDups (uniqCompareFS `on` (occNameFS . rdrNameOcc)) (getFieldLbls flds)
      -- See the same duplicate handling logic in rnHsRecUpdFields below for further context.

-- | Rename a regular (non-overloaded) record field update,
-- disambiguating the fields if necessary.
rnHsRecUpdFields
    :: [LHsRecUpdField GhcPs GhcPs]
    -> RnM (XLHsRecUpdLabels GhcRn, [LHsRecUpdField GhcRn GhcRn], FreeVars)
rnHsRecUpdFields flds
  = do { pun_ok <- xoptM LangExt.NamedFieldPuns

       -- Check for an empty record update:  e {}
       -- NB: don't complain about e { .. }, because rn_dotdot has done that already
       ; case flds of
          { [] -> failWithTc TcRnEmptyRecordUpdate
          ; fld:other_flds ->
    do { let dup_lbls :: [NE.NonEmpty RdrName]
             (_, dup_lbls) = removeDups (uniqCompareFS `on` (occNameFS . rdrNameOcc))
                              (fmap (unLoc . getFieldUpdLbl) flds)
               -- NB: we compare using the underlying field label FastString,
               -- in order to catch duplicates involving qualified names,
               -- as in the record update `r { fld = x, Mod.fld = y }`.
               -- See #21959.
               -- Note that this test doesn't correctly handle exact Names, but those
               -- aren't handled properly by the rest of the compiler anyway. See #22122.
       ; mapM_ (addErr . dupFieldErr HsRecFieldUpd) dup_lbls

         -- See Note [Disambiguating record updates]
       ; possible_parents <- lookupRecUpdFields (fld NE.:| other_flds)
       ; let  mb_unambig_lbls :: Maybe [FieldLabel]
              fvs :: FreeVars
              (mb_unambig_lbls, fvs) =
               case possible_parents of
                  RnRecUpdParent { rnRecUpdLabels = gres } NE.:| []
                    | let lbls = map fieldGRELabel $ NE.toList gres
                    -> ( Just lbls, mkFVs $ map flSelector lbls)
                  _ -> ( Nothing
                       , plusFVs $ map (plusFVs . map pat_syn_free_vars . NE.toList . rnRecUpdLabels)
                                 $ NE.toList possible_parents
                         -- See Note [Using PatSyn FreeVars]
                       )

        -- Rename each field.
        ; (upd_flds, fvs') <- rn_flds pun_ok mb_unambig_lbls flds
        ; let all_fvs = fvs `plusFV` fvs'
        ; return (possible_parents, upd_flds, all_fvs) } } }

    where

      -- For an ambiguous record update involving pattern synonym record fields,
      -- we must add all the possibly-relevant field selector names to ensure that
      -- we typecheck the record update **after** we typecheck the pattern synonym
      -- definition. See Note [Using PatSyn FreeVars].
      pat_syn_free_vars :: FieldGlobalRdrElt -> FreeVars
      pat_syn_free_vars (GRE { gre_info = info })
        | IAmRecField fld_info <- info
        , RecFieldInfo { recFieldLabel = fl, recFieldCons = cons } <- fld_info
        , uniqSetAny is_PS cons
        = unitFV (flSelector fl)
      pat_syn_free_vars _
        = emptyFVs

      is_PS :: ConLikeName -> Bool
      is_PS (PatSynName  {}) = True
      is_PS (DataConName {}) = False

      rn_flds :: Bool -> Maybe [FieldLabel]
              -> [LHsRecUpdField GhcPs GhcPs]
              -> RnM ([LHsRecUpdField GhcRn GhcRn], FreeVars)
      rn_flds _ _ [] = return ([], emptyFVs)
      rn_flds pun_ok mb_unambig_lbls
              ((L l (HsFieldBind { hfbLHS = L loc (FieldOcc _ f)
                                 , hfbRHS = arg
                                 , hfbPun = pun })):flds)
        = do { let lbl = unLoc f
             ; (arg' :: LHsExpr GhcPs) <- if pun
                       then do { setSrcSpanA loc $
                                 checkErr pun_ok (TcRnIllegalFieldPunning (L (locA loc) lbl))
                                 -- Discard any module qualifier (#11662)
                               ; let arg_rdr = mkRdrUnqual (rdrNameOcc lbl)
                               ; return (L (l2l loc) (HsVar noExtField (L (l2l loc) arg_rdr))) }
                       else return arg
             ; (arg'', fvs) <- rnLExpr arg'
             ; let lbl' :: FieldOcc GhcRn
                   lbl' = case mb_unambig_lbls of
                            { Just (fl:_) ->
                                let sel_name = flSelector fl
                                in FieldOcc lbl (L (l2l loc) sel_name)
                                -- We have one last chance to be disambiguated during type checking.
                                -- At least, until type-directed disambiguation stops being supported.
                                -- see note [Ambiguous FieldOcc in record updates] for more info.
                            ; _ -> FieldOcc lbl (L (l2l loc) (mkUnboundName $ rdrNameOcc lbl)) }
                   fld' :: LHsRecUpdField GhcRn GhcRn
                   fld' = L l (HsFieldBind { hfbAnn = noAnn
                                           , hfbLHS = L (l2l loc) lbl'
                                           , hfbRHS = arg''
                                           , hfbPun = pun })
             ; (flds', fvs') <- rn_flds pun_ok (tail <$> mb_unambig_lbls) flds
             ; return (fld' : flds', fvs `plusFV` fvs') }

getFieldIds :: [LHsRecField GhcRn arg] -> [Name]
getFieldIds flds = map (hsRecFieldSel . unLoc) flds

getFieldRdrs :: [LHsRecField GhcRn arg] -> [RdrName]
getFieldRdrs flds = map (foExt . unXRec @GhcRn . hfbLHS . unLoc) flds

getFieldLbls :: [LHsRecField (GhcPass p) arg] -> [IdGhcP p]
getFieldLbls flds
  = map (unLoc . foLabel . unLoc . hfbLHS . unLoc) flds

needFlagDotDot :: HsRecFieldContext -> TcRnMessage
needFlagDotDot = TcRnIllegalWildcardsInRecord . toRecordFieldPart

dupFieldErr :: HsRecFieldContext -> NE.NonEmpty RdrName -> TcRnMessage
dupFieldErr ctxt = TcRnDuplicateFieldName (toRecordFieldPart ctxt)

toRecordFieldPart :: HsRecFieldContext -> RecordFieldPart
toRecordFieldPart (HsRecFieldCon n)  = RecordFieldConstructor n
toRecordFieldPart (HsRecFieldPat n)  = RecordFieldPattern     n
toRecordFieldPart (HsRecFieldUpd {}) = RecordFieldUpdate

{- Note [Disambiguating record updates]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the -XDuplicateRecordFields extension is used, to rename and typecheck
a non-overloaded record update, we might need to disambiguate the field labels.

Consider the following definitions:

   {-# LANGUAGE DuplicateRecordFields #-}

    data R = MkR1 { fld1 :: Int, fld2 :: Char }
           | MKR2 { fld1 :: Int, fld2 :: Char, fld3 :: Bool }
    data S = MkS1 { fld1 :: Int } | MkS2 { fld2 :: Char }

In a record update, the `lookupRecUpdFields` function tries to determine
the parent datatype by computing the parents (TyCon/PatSyn) which have
at least one constructor (DataCon/PatSyn) with all of the fields.

For example, in the (non-overloaded) record update

    r { fld1 = 3, fld2 = 'x' }

only the TyCon R contains at least one DataCon which has both of the fields
being updated: in this case, MkR1 and MkR2 have both of the updated fields.
The TyCon S also has both fields fld1 and fld2, but no single constructor
has both of those fields, so S is not a valid parent for this record update.

Note that this check is namespace-aware, so that a record update such as

    import qualified M ( R (fld1, fld2) )
    f r = r { M.fld1 = 3 }

is unambiguous, as only R contains the field fld1 in the M namespace.
(See however #22122 for issues relating to the usage of exact Names in
record fields.)

See also Note [Type-directed record disambiguation] in GHC.Tc.Gen.Expr.

Note [Using PatSyn FreeVars]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we are disambiguating a non-overloaded record update, as per
Note [Disambiguating record updates], and have determined that this
record update might involve pattern synonym record fields, it is important
to declare usage of all these pattern synonyms record fields in the returned
FreeVars of rnHsRecUpdFields. This ensures that the typechecker sees
that the typechecking of the record update depends on the typechecking
of the pattern synonym, and typechecks the pattern synonyms first.
Not doing so caused #21898.

Note that this can be removed once GHC proposal #366 is implemented,
as we will be able to fully disambiguate the record update in the renamer,
and can immediately declare the correct used FreeVars instead of having
to over-estimate in case of ambiguity.

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

isNegativeZeroOverLit :: (XXOverLit t ~ DataConCantHappen) => HsOverLit t -> Bool
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

rnOverLit :: (XXOverLit t ~ DataConCantHappen) => HsOverLit t ->
             RnM ((HsOverLit GhcRn, Maybe (HsExpr GhcRn)), FreeVars)
rnOverLit origLit
  = do  { opt_NumDecimals <- xoptM LangExt.NumDecimals
        ; let { lit@(OverLit {ol_val=val})
            | opt_NumDecimals = origLit {ol_val = generalizeOverLitVal (ol_val origLit)}
            | otherwise       = origLit
          }
        ; let std_name = hsOverLitName val
        ; (from_thing_name, fvs1) <- lookupSyntaxName std_name
        ; loc <- getSrcSpanM -- See Note [Source locations for implicit function calls] in GHC.Iface.Ext.Ast
        ; let rebindable = from_thing_name /= std_name
              lit' = lit { ol_ext = OverLitRn { ol_rebindable = rebindable
                                              , ol_from_fun = L (noAnnSrcSpan loc) from_thing_name } }
        ; if isNegativeZeroOverLit lit'
          then do { (negate_expr, fvs2) <- lookupSyntaxExpr negateName
                  ; return ((lit' { ol_val = negateOverLitVal val }, Just negate_expr)
                           , fvs1 `plusFV` fvs2) }
          else return ((lit', Nothing), fvs1) }


rnHsTyPat :: HsDocContext
          -> HsTyPat GhcPs
          -> CpsRn (HsTyPat GhcRn)
rnHsTyPat ctxt sigType = case sigType of
  (HsTP { hstp_body = hs_ty }) -> do
    (hs_ty', tpb) <- runTPRnM (rn_lty_pat hs_ty) ctxt
    pure HsTP
          { hstp_body = hs_ty'
          , hstp_ext = buildHsTyPatRn tpb
          }

-- | Type pattern renaming monad
-- For the OccSet in the ReaderT, see Note [Locally bound names in type patterns]
-- For the HsTyPatRnBuilderRn in the WriterT, see Note [Implicit and explicit type variable binders]
-- For the CpsRn base monad, see Note [CpsRn monad]
-- For why we need CpsRn in TPRnM see Note [Left-to-right scoping of type patterns]
newtype TPRnM a =
  MkTPRnM (ReaderT (HsDocContext, OccSet) (WriterT HsTyPatRnBuilder CpsRn) a)
  deriving newtype (Functor, Applicative, Monad)

runTPRnM :: TPRnM a -> HsDocContext -> CpsRn (a, HsTyPatRnBuilder)
runTPRnM (MkTPRnM thing_inside) doc_ctxt = runWriterT $ runReaderT thing_inside (doc_ctxt, emptyOccSet)

askLocals :: TPRnM OccSet
askLocals = MkTPRnM (asks snd)

askDocContext :: TPRnM HsDocContext
askDocContext = MkTPRnM (asks fst)

tellTPB :: HsTyPatRnBuilder -> TPRnM ()
tellTPB = MkTPRnM . lift . tell

liftRnFV :: RnM (a, FreeVars) -> TPRnM a
liftRnFV = liftTPRnCps . liftCpsFV

liftRn :: RnM a -> TPRnM a
liftRn = liftTPRnCps . liftCps

liftRnWithCont :: (forall r. (b -> RnM (r, FreeVars)) -> RnM (r, FreeVars)) -> TPRnM b
liftRnWithCont cont = liftTPRnCps (liftCpsWithCont cont)

liftTPRnCps :: CpsRn a -> TPRnM a
liftTPRnCps = MkTPRnM . lift . lift

liftTPRnRaw ::
  ( forall r .
    HsDocContext ->
    OccSet ->
    ((a, HsTyPatRnBuilder) -> RnM (r, FreeVars)) ->
    RnM (r, FreeVars)
  ) -> TPRnM a
liftTPRnRaw cont = MkTPRnM $ ReaderT $ \(doc_ctxt, locals) -> writerT $ liftCpsWithCont (cont doc_ctxt locals)

unTPRnRaw ::
  TPRnM a ->
  HsDocContext ->
  OccSet ->
  ((a, HsTyPatRnBuilder) -> RnM (r, FreeVars)) ->
  RnM (r, FreeVars)
unTPRnRaw (MkTPRnM m) doc_ctxt locals = unCpsRn $ runWriterT $ runReaderT m (doc_ctxt, locals)

wrapSrcSpanTPRnM :: (a -> TPRnM b) -> LocatedAn ann a -> TPRnM (LocatedAn ann b)
wrapSrcSpanTPRnM fn (L loc a) = do
  a' <- fn a
  pure (L loc a')

lookupTypeOccTPRnM :: RdrName -> TPRnM Name
lookupTypeOccTPRnM rdr_name = liftRnFV $ do
  name <- lookupTypeOccRn rdr_name
  pure (name, unitFV name)

rn_lty_pat :: LHsType GhcPs -> TPRnM (LHsType GhcRn)
rn_lty_pat (L l hs_ty) = do
  hs_ty' <- rn_ty_pat hs_ty
  pure (L l hs_ty')

rn_ty_pat_var :: LocatedN RdrName -> TPRnM (LocatedN Name)
rn_ty_pat_var lrdr@(L l rdr) = do
  locals <- askLocals
  if isRdrTyVar rdr
    && not (elemOccSet (occName rdr) locals) -- See Note [Locally bound names in type patterns]

    then do -- binder
      name <- liftTPRnCps $ newPatName (LamMk True) lrdr
      tellTPB (tpBuilderExplicitTV name)
      pure (L l name)

    else do -- usage
      name <- lookupTypeOccTPRnM rdr
      pure (L l name)

-- | Rename type patterns
--
-- For the difference between `rn_ty_pat` and `rnHsTyKi` see Note [CpsRn monad]
-- and Note [Implicit and explicit type variable binders]
rn_ty_pat :: HsType GhcPs -> TPRnM (HsType GhcRn)
rn_ty_pat tv@(HsTyVar an prom lrdr) = do
  lname@(L _ name) <- rn_ty_pat_var lrdr
  when (isDataConName name && not (isKindName name)) $
    -- Any use of a promoted data constructor name (that is not specifically
    -- exempted by isKindName) is illegal without the use of DataKinds.
    -- See Note [Checking for DataKinds] in GHC.Tc.Validity.
    check_data_kinds tv
  pure (HsTyVar an prom lname)

rn_ty_pat (HsForAllTy an tele body) = liftTPRnRaw $ \ctxt locals thing_inside ->
  bindHsForAllTelescope ctxt tele $ \tele' -> do
    let
      tele_names = hsForAllTelescopeNames tele'
      locals' = locals `extendOccSetList` map occName tele_names

    unTPRnRaw (rn_lty_pat body) ctxt locals' $ \(body', tpb) ->
      delLocalNames tele_names $ -- locally bound names do not scope over the continuation
        thing_inside ((HsForAllTy an tele' body'), tpb)

rn_ty_pat (HsQualTy an lctx body) = do
  lctx' <- wrapSrcSpanTPRnM (mapM rn_lty_pat) lctx
  body' <- rn_lty_pat body
  pure (HsQualTy an lctx' body')

rn_ty_pat (HsAppTy _ fun_ty arg_ty) = do
  fun_ty' <- rn_lty_pat fun_ty
  arg_ty' <- rn_lty_pat arg_ty
  pure (HsAppTy noExtField fun_ty' arg_ty')

rn_ty_pat (HsAppKindTy _ ty ki) = do
  kind_app <- liftRn $ xoptM LangExt.TypeApplications
  unless kind_app (liftRn $ addErr (typeAppErr KindLevel ki))
  ty' <- rn_lty_pat ty
  ki' <- rn_lty_pat ki
  pure (HsAppKindTy noExtField ty' ki')

rn_ty_pat (HsFunTy an mult lhs rhs) = do
  lhs' <- rn_lty_pat lhs
  mult' <- rn_ty_pat_arrow mult
  rhs' <- rn_lty_pat rhs
  pure (HsFunTy an mult' lhs' rhs')

rn_ty_pat (HsListTy an ty) = do
  ty' <- rn_lty_pat ty
  pure (HsListTy an ty')

rn_ty_pat (HsTupleTy an con tys) = do
  tys' <- mapM rn_lty_pat tys
  pure (HsTupleTy an con tys')

rn_ty_pat (HsSumTy an tys) = do
  tys' <- mapM rn_lty_pat tys
  pure (HsSumTy an tys')

rn_ty_pat (HsOpTy _ prom ty1 l_op ty2) = do
  ty1' <- rn_lty_pat ty1
  l_op' <- rn_ty_pat_var l_op
  ty2' <- rn_lty_pat ty2
  fix  <- liftRn $ lookupTyFixityRn l_op'
  let op_name = unLoc l_op'
  when (isDataConName op_name && not (isPromoted prom)) $
    liftRn $ addDiagnostic (TcRnUntickedPromotedThing $ UntickedConstructor Infix op_name)
  liftRn $ mkHsOpTyRn prom l_op' fix ty1' ty2'

rn_ty_pat (HsParTy an ty) = do
  ty' <- rn_lty_pat ty
  pure (HsParTy an ty')

rn_ty_pat (HsIParamTy an n ty) = do
  ty' <- rn_lty_pat ty
  pure (HsIParamTy an n ty')

rn_ty_pat (HsStarTy an unicode) =
  pure (HsStarTy an unicode)

rn_ty_pat (HsDocTy an ty haddock_doc) = do
  ty' <- rn_lty_pat ty
  haddock_doc' <- liftRn $ rnLHsDoc haddock_doc
  pure (HsDocTy an ty' haddock_doc')

rn_ty_pat ty@(HsExplicitListTy _ prom tys) = do
  check_data_kinds ty

  unless (isPromoted prom) $
    liftRn $ addDiagnostic (TcRnUntickedPromotedThing $ UntickedExplicitList)

  tys' <- mapM rn_lty_pat tys
  pure (HsExplicitListTy noExtField prom tys')

rn_ty_pat ty@(HsExplicitTupleTy _ prom tys) = do
  check_data_kinds ty
  tys' <- mapM rn_lty_pat tys
  pure (HsExplicitTupleTy noExtField prom tys')

rn_ty_pat tyLit@(HsTyLit src t) = do
  check_data_kinds tyLit
  t' <- liftRn $ rnHsTyLit t
  pure (HsTyLit src t')

rn_ty_pat (HsWildCardTy _) =
  pure (HsWildCardTy noExtField)

rn_ty_pat (HsKindSig an ty ki) = do
  ctxt <- askDocContext
  kind_sigs_ok <- liftRn $ xoptM LangExt.KindSignatures
  unless kind_sigs_ok (liftRn $ badKindSigErr ctxt ki)
  ~(HsPS hsps ki') <- liftRnWithCont $
                      rnHsPatSigKind AlwaysBind ctxt (HsPS noAnn ki)
  ty' <- rn_lty_pat ty
  tellTPB (tpBuilderPatSig hsps)
  pure (HsKindSig an ty' ki')

rn_ty_pat (HsSpliceTy _ splice) = do
  res <- liftRnFV $ rnSpliceTyPat splice
  case res of
    (rn_splice, HsUntypedSpliceTop mfs pat) -> do -- Splice was top-level and thus run, creating LHsType GhcPs
        pat' <- rn_lty_pat pat
        pure (HsSpliceTy (HsUntypedSpliceTop mfs (mb_paren pat')) rn_splice)
    (rn_splice, HsUntypedSpliceNested splice_name) ->
        pure (HsSpliceTy (HsUntypedSpliceNested splice_name) rn_splice) -- Splice was nested and thus already renamed
  where
    mb_paren :: LHsType GhcRn -> LHsType GhcRn
    mb_paren lhs_ty@(L loc hs_ty)
      | hsTypeNeedsParens maxPrec hs_ty = L loc (HsParTy noAnn lhs_ty)
      | otherwise                       = lhs_ty

rn_ty_pat (HsBangTy an bang_src lty) = do
  ctxt <- askDocContext
  lty'@(L _ ty') <- rn_lty_pat lty
  liftRn $ addErr $
    TcRnWithHsDocContext ctxt $
    TcRnUnexpectedAnnotation ty' bang_src
  pure (HsBangTy an bang_src lty')

rn_ty_pat ty@HsRecTy{} = do
  ctxt <- askDocContext
  liftRn $ addErr $
    TcRnWithHsDocContext ctxt $
    TcRnIllegalRecordSyntax (Left ty)
  pure (HsWildCardTy noExtField) -- trick to avoid `failWithTc`

rn_ty_pat ty@(XHsType{}) = do
  ctxt <- askDocContext
  liftRnFV $ rnHsType ctxt ty

rn_ty_pat_arrow :: HsArrow GhcPs -> TPRnM (HsArrow GhcRn)
rn_ty_pat_arrow (HsUnrestrictedArrow _) = pure (HsUnrestrictedArrow noExtField)
rn_ty_pat_arrow (HsLinearArrow _) = pure (HsLinearArrow noExtField)
rn_ty_pat_arrow (HsExplicitMult _ p)
  = rn_lty_pat p <&> (\mult -> HsExplicitMult noExtField mult)

check_data_kinds :: HsType GhcPs -> TPRnM ()
check_data_kinds thing = liftRn $ do
  data_kinds <- xoptM LangExt.DataKinds
  unless data_kinds $
    addErr $ TcRnDataKindsError TypeLevel $ Left thing

{- Note [Locally bound names in type patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Type patterns can bind local names using forall. Compare the following examples:
  f (Proxy @(Either a b)) = ...
  g (Proxy @(forall a . Either a b)) = ...

In `f` both `a` and `b` are bound by the pattern and scope over the RHS of f.
In `g` only `b` is bound by the pattern, whereas `a` is locally bound in the pattern
and does not scope over the RHS of `g`.

We track locally bound names in the `OccSet` in `TPRnM` monad, and use it to
decide whether occurrences of type variables are usages or bindings.

The check is done in `rn_ty_pat_var`

Note [Implicit and explicit type variable binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Type patterns are renamed differently from ordinary types.
  * Types are renamed by `rnHsType` where all type variable occurrences are considered usages
  * Type patterns are renamed by `rnHsTyPat` where some type variable occurrences are usages
    and other are bindings

Here is an example:
  {-# LANGUAGE ScopedTypeVariables #-}
  f :: forall b. Proxy _ -> ...
  f (Proxy @(x :: (a, b))) = ...

In the (x :: (a,b)) type pattern
  * `x` is a type variable explicitly bound by type pattern
  * `a` is a type variable implicitly bound in a pattern signature
  * `b` is a usage of type variable bound by the outer forall

This classification is clear to us in `rnHsTyPat`, but it is also useful in later passes, such
as `collectPatBinders` and `tcHsTyPat`, so we store it in the extension field of `HsTyPat`, namely
`HsTyPatRn`.

To collect lists of those variables efficiently we use `HsTyPatRnBuilder` which is exactly like
`HsTyPatRn`, but uses Bags.

Note [Left-to-right scoping of type patterns]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In term-level patterns, we use continuation passing to implement left-to-right
scoping, see Note [CpsRn monad]. Left-to-right scoping manifests itself when
e.g. view patterns are involved:

  f (x, g x -> Just y) = ...

Here the first occurrence of `x` is a binder, and the second occurrence is a
use of `x` in a view pattern. This example does not work if we swap the
components of the tuple:

  f (g x -> Just y, x) = ...
  --  ^^^
  -- Variable not in scope: x

In type patterns there are no view patterns, but there is a different feature
that is served well by left-to-right scoping: kind annotations. Compare:

  f (Proxy @(T k (a :: k))) = ...
  g (Proxy @(T (a :: k) k)) = ...

In `f`, the first occurrence of `k` is an explicit binder,
  and the second occurrence is a usage. Simple.
In `g`, the first occurrence of `k` is an implicit binder,
  and then the second occurrence is an explicit binder that shadows it.

So we get two different results after renaming:

  f (Proxy @(T k1 (a :: k1))) = ...
  g (Proxy @(T (a :: k1) k2)) = ...

This makes GHC accept the first example but rejects the second example with an
error about duplicate binders.

One could argue that we don't want order-sensitivity here. Historically, we
used a different principle when renaming types: collect all free variables,
bind them on the outside, and then rename all occurrences as usages.
This approach does not scale to multiple patterns. Consider:

  f' (MkP @k @(a :: k)) = ...
  g' (MkP @(a :: k) @k) = ...

Here a difference in behavior is inevitable, as we rename type patterns
one at a time. Could we perhaps concatenate the free variables from all
type patterns in a ConPat? But then we still get the same problem one level up,
when we have multiple patterns in a function LHS

  f'' (Proxy @k) (Proxy @(a :: k)) = ...
  g'' (Proxy @(a :: k)) (Proxy @k) = ...

And if we tried to avoid order sensitivity at this level, then we'd still be left
with lambdas:

  f''' (Proxy @k)        = \(Proxy @(a :: k)) -> ...
  g''' (Proxy @(a :: k)) = \(Proxy @k)        -> ...


So we have at least three options where we could do free variable extraction:
HsConPatTyArg, ConPat, or a Match (used to represent a function LHS). And none
of those would be general enough. Rather than make an arbitrary choice, we
embrace left-to-right scoping in types and implement it with CPS, just like
it's done for view patterns in terms.
-}
