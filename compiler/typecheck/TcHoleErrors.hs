{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
module TcHoleErrors ( findValidHoleFits, tcFilterHoleFits
                    , tcCheckHoleFit, tcSubsumes
                    , withoutUnification
                    , fromPureHFPlugin
                    -- Re-exports for convenience
                    , hfIsLcl
                    , pprHoleFit, debugHoleFitDispConfig

                    -- Re-exported from TcHoleFitTypes
                    , TypedHole (..), HoleFit (..), HoleFitCandidate (..)
                    , CandPlugin, FitPlugin
                    , HoleFitPlugin (..), HoleFitPluginR (..)
                    ) where

import GhcPrelude

import TcRnTypes
import TcRnMonad
import Constraint
import TcOrigin
import TcMType
import TcEvidence
import TcType
import GHC.Core.Type
import GHC.Core.DataCon
import GHC.Types.Name
import GHC.Types.Name.Reader ( pprNameProvenance , GlobalRdrElt (..), globalRdrEnvElts )
import PrelNames ( gHC_ERR )
import GHC.Types.Id
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import Bag
import GHC.Core.ConLike ( ConLike(..) )
import Util
import TcEnv (tcLookup)
import Outputable
import GHC.Driver.Session
import Maybes
import FV ( fvVarList, fvVarSet, unionFV, mkFVs, FV )

import Control.Arrow ( (&&&) )

import Control.Monad    ( filterM, replicateM, foldM )
import Data.List        ( partition, sort, sortOn, nubBy )
import Data.Graph       ( graphFromEdges, topSort )


import TcSimplify    ( simpl_top, runTcSDeriveds )
import TcUnify       ( tcSubType_NC )

import GHC.HsToCore.Docs ( extractDocs )
import qualified Data.Map as Map
import GHC.Hs.Doc      ( unpackHDS, DeclDocMap(..) )
import GHC.Driver.Types        ( ModIface_(..) )
import GHC.Iface.Load  ( loadInterfaceForNameMaybe )

import PrelInfo (knownKeyNames)

import TcHoleFitTypes


{-
Note [Valid hole fits include ...]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
`findValidHoleFits` returns the "Valid hole fits include ..." message.
For example, look at the following definitions in a file called test.hs:

   import Data.List (inits)

   f :: [String]
   f = _ "hello, world"

The hole in `f` would generate the message:

  • Found hole: _ :: [Char] -> [String]
  • In the expression: _
    In the expression: _ "hello, world"
    In an equation for ‘f’: f = _ "hello, world"
  • Relevant bindings include f :: [String] (bound at test.hs:6:1)
    Valid hole fits include
      lines :: String -> [String]
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘base-4.11.0.0:Data.OldList’))
      words :: String -> [String]
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘base-4.11.0.0:Data.OldList’))
      inits :: forall a. [a] -> [[a]]
        with inits @Char
        (imported from ‘Data.List’ at mpt.hs:4:19-23
          (and originally defined in ‘base-4.11.0.0:Data.OldList’))
      repeat :: forall a. a -> [a]
        with repeat @String
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘GHC.List’))
      fail :: forall (m :: * -> *). Monad m => forall a. String -> m a
        with fail @[] @String
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘GHC.Base’))
      return :: forall (m :: * -> *). Monad m => forall a. a -> m a
        with return @[] @String
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘GHC.Base’))
      pure :: forall (f :: * -> *). Applicative f => forall a. a -> f a
        with pure @[] @String
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘GHC.Base’))
      read :: forall a. Read a => String -> a
        with read @[String]
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘Text.Read’))
      mempty :: forall a. Monoid a => a
        with mempty @([Char] -> [String])
        (imported from ‘Prelude’ at mpt.hs:3:8-9
          (and originally defined in ‘GHC.Base’))

Valid hole fits are found by checking top level identifiers and local bindings
in scope for whether their type can be instantiated to the the type of the hole.
Additionally, we also need to check whether all relevant constraints are solved
by choosing an identifier of that type as well, see Note [Relevant Constraints]

Since checking for subsumption results in the side-effect of type variables
being unified by the simplifier, we need to take care to restore them after
to being flexible type variables after we've checked for subsumption.
This is to avoid affecting the hole and later checks by prematurely having
unified one of the free unification variables.

When outputting, we sort the hole fits by the size of the types we'd need to
apply by type application to the type of the fit to to make it fit. This is done
in order to display "more relevant" suggestions first. Another option is to
sort by building a subsumption graph of fits, i.e. a graph of which fits subsume
what other fits, and then outputting those fits which are are subsumed by other
fits (i.e. those more specific than other fits) first. This results in the ones
"closest" to the type of the hole to be displayed first.

To help users understand how the suggested fit works, we also display the values
that the quantified type variables would take if that fit is used, like
`mempty @([Char] -> [String])` and `pure @[] @String` in the example above.
If -XTypeApplications is enabled, this can even be copied verbatim as a
replacement for the hole.


Note [Nested implications]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

For the simplifier to be able to use any givens present in the enclosing
implications to solve relevant constraints, we nest the wanted subsumption
constraints and relevant constraints within the enclosing implications.

As an example, let's look at the following code:

  f :: Show a => a -> String
  f x = show _

The hole will result in the hole constraint:

  [WD] __a1ph {0}:: a0_a1pd[tau:2] (CHoleCan: ExprHole(_))

Here the nested implications are just one level deep, namely:

  [Implic {
      TcLevel = 2
      Skolems = a_a1pa[sk:2]
      No-eqs = True
      Status = Unsolved
      Given = $dShow_a1pc :: Show a_a1pa[sk:2]
      Wanted =
        WC {wc_simple =
              [WD] __a1ph {0}:: a_a1pd[tau:2] (CHoleCan: ExprHole(_))
              [WD] $dShow_a1pe {0}:: Show a_a1pd[tau:2] (CDictCan(psc))}
      Binds = EvBindsVar<a1pi>
      Needed inner = []
      Needed outer = []
      the type signature for:
        f :: forall a. Show a => a -> String }]

As we can see, the givens say that the information about the skolem
`a_a1pa[sk:2]` fulfills the Show constraint.

The simples are:

  [[WD] __a1ph {0}:: a0_a1pd[tau:2] (CHoleCan: ExprHole(_)),
    [WD] $dShow_a1pe {0}:: Show a0_a1pd[tau:2] (CNonCanonical)]

I.e. the hole `a0_a1pd[tau:2]` and the constraint that the type of the hole must
fulfill `Show a0_a1pd[tau:2])`.

So when we run the check, we need to make sure that the

  [WD] $dShow_a1pe {0}:: Show a0_a1pd[tau:2] (CNonCanonical)

Constraint gets solved. When we now check for whether `x :: a0_a1pd[tau:2]` fits
the hole in `tcCheckHoleFit`, the call to `tcSubType` will end up writing the
meta type variable `a0_a1pd[tau:2] := a_a1pa[sk:2]`. By wrapping the wanted
constraints needed by tcSubType_NC and the relevant constraints (see
Note [Relevant Constraints] for more details) in the nested implications, we
can pass the information in the givens along to the simplifier. For our example,
we end up needing to check whether the following constraints are soluble.

  WC {wc_impl =
        Implic {
          TcLevel = 2
          Skolems = a_a1pa[sk:2]
          No-eqs = True
          Status = Unsolved
          Given = $dShow_a1pc :: Show a_a1pa[sk:2]
          Wanted =
            WC {wc_simple =
                  [WD] $dShow_a1pe {0}:: Show a0_a1pd[tau:2] (CNonCanonical)}
          Binds = EvBindsVar<a1pl>
          Needed inner = []
          Needed outer = []
          the type signature for:
            f :: forall a. Show a => a -> String }}

But since `a0_a1pd[tau:2] := a_a1pa[sk:2]` and we have from the nested
implications that Show a_a1pa[sk:2] is a given, this is trivial, and we end up
with a final WC of WC {}, confirming x :: a0_a1pd[tau:2] as a match.

To avoid side-effects on the nested implications, we create a new EvBindsVar so
that any changes to the ev binds during a check remains localised to that check.


Note [Valid refinement hole fits include ...]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When the `-frefinement-level-hole-fits=N` flag is given, we additionally look
for "valid refinement hole fits"", i.e. valid hole fits with up to N
additional holes in them.

With `-frefinement-level-hole-fits=0` (the default), GHC will find all
identifiers 'f' (top-level or nested) that will fit in the hole.

With `-frefinement-level-hole-fits=1`, GHC will additionally find all
applications 'f _' that will fit in the hole, where 'f' is an in-scope
identifier, applied to single argument.  It will also report the type of the
needed argument (a new hole).

And similarly as the number of arguments increases

As an example, let's look at the following code:

  f :: [Integer] -> Integer
  f = _

with `-frefinement-level-hole-fits=1`, we'd get:

  Valid refinement hole fits include

    foldl1 (_ :: Integer -> Integer -> Integer)
      with foldl1 @[] @Integer
      where foldl1 :: forall (t :: * -> *).
                      Foldable t =>
                      forall a. (a -> a -> a) -> t a -> a
    foldr1 (_ :: Integer -> Integer -> Integer)
      with foldr1 @[] @Integer
      where foldr1 :: forall (t :: * -> *).
                      Foldable t =>
                      forall a. (a -> a -> a) -> t a -> a
    const (_ :: Integer)
      with const @Integer @[Integer]
      where const :: forall a b. a -> b -> a
    ($) (_ :: [Integer] -> Integer)
      with ($) @'GHC.Types.LiftedRep @[Integer] @Integer
      where ($) :: forall a b. (a -> b) -> a -> b
    fail (_ :: String)
      with fail @((->) [Integer]) @Integer
      where fail :: forall (m :: * -> *).
                    Monad m =>
                    forall a. String -> m a
    return (_ :: Integer)
      with return @((->) [Integer]) @Integer
      where return :: forall (m :: * -> *). Monad m => forall a. a -> m a
    (Some refinement hole fits suppressed;
      use -fmax-refinement-hole-fits=N or -fno-max-refinement-hole-fits)

Which are hole fits with holes in them. This allows e.g. beginners to
discover the fold functions and similar, but also allows for advanced users
to figure out the valid functions in the Free monad, e.g.

  instance Functor f => Monad (Free f) where
      Pure a >>= f = f a
      Free f >>= g = Free (fmap _a f)

Will output (with -frefinment-level-hole-fits=1):
    Found hole: _a :: Free f a -> Free f b
          Where: ‘a’, ‘b’ are rigid type variables bound by
                  the type signature for:
                    (>>=) :: forall a b. Free f a -> (a -> Free f b) -> Free f b
                  at fms.hs:25:12-14
                ‘f’ is a rigid type variable bound by
    ...
    Relevant bindings include
      g :: a -> Free f b (bound at fms.hs:27:16)
      f :: f (Free f a) (bound at fms.hs:27:10)
      (>>=) :: Free f a -> (a -> Free f b) -> Free f b
        (bound at fms.hs:25:12)
    ...
    Valid refinement hole fits include
      ...
      (=<<) (_ :: a -> Free f b)
        with (=<<) @(Free f) @a @b
        where (=<<) :: forall (m :: * -> *) a b.
                      Monad m =>
                      (a -> m b) -> m a -> m b
        (imported from ‘Prelude’ at fms.hs:5:18-22
        (and originally defined in ‘GHC.Base’))
      ...

Where `(=<<) _` is precisely the function we want (we ultimately want `>>= g`).

We find these refinement suggestions by considering hole fits that don't
fit the type of the hole, but ones that would fit if given an additional
argument. We do this by creating a new type variable with `newOpenFlexiTyVar`
(e.g. `t_a1/m[tau:1]`), and then considering hole fits of the type
`t_a1/m[tau:1] -> v` where `v` is the type of the hole.

Since the simplifier is free to unify this new type variable with any type, we
can discover any identifiers that would fit if given another identifier of a
suitable type. This is then generalized so that we can consider any number of
additional arguments by setting the `-frefinement-level-hole-fits` flag to any
number, and then considering hole fits like e.g. `foldl _ _` with two additional
arguments.

To make sure that the refinement hole fits are useful, we check that the types
of the additional holes have a concrete value and not just an invented type
variable. This eliminates suggestions such as `head (_ :: [t0 -> a]) (_ :: t0)`,
and limits the number of less than useful refinement hole fits.

Additionally, to further aid the user in their implementation, we show the
types of the holes the binding would have to be applied to in order to work.
In the free monad example above, this is demonstrated with
`(=<<) (_ :: a -> Free f b)`, which tells the user that the `(=<<)` needs to
be applied to an expression of type `a -> Free f b` in order to match.
If -XScopedTypeVariables is enabled, this hole fit can even be copied verbatim.


Note [Relevant Constraints]
~~~~~~~~~~~~~~~~~~~

As highlighted by #14273, we need to check any relevant constraints as well
as checking for subsumption. Relevant constraints are the simple constraints
whose free unification variables are mentioned in the type of the hole.

In the simplest case, these are all non-hole constraints in the simples, such
as is the case in

  f :: String
  f = show _

Where the simples will be :

  [[WD] __a1kz {0}:: a0_a1kv[tau:1] (CHoleCan: ExprHole(_)),
    [WD] $dShow_a1kw {0}:: Show a0_a1kv[tau:1] (CNonCanonical)]

However, when there are multiple holes, we need to be more careful. As an
example, Let's take a look at the following code:

  f :: Show a => a -> String
  f x = show (_b (show _a))

Here there are two holes, `_a` and `_b`, and the simple constraints passed to
findValidHoleFits are:

  [[WD] _a_a1pi {0}:: String
                        -> a0_a1pd[tau:2] (CHoleCan: ExprHole(_b)),
    [WD] _b_a1ps {0}:: a1_a1po[tau:2] (CHoleCan: ExprHole(_a)),
    [WD] $dShow_a1pe {0}:: Show a0_a1pd[tau:2] (CNonCanonical),
    [WD] $dShow_a1pp {0}:: Show a1_a1po[tau:2] (CNonCanonical)]


Here we have the two hole constraints for `_a` and `_b`, but also additional
constraints that these holes must fulfill. When we are looking for a match for
the hole `_a`, we filter the simple constraints to the "Relevant constraints",
by throwing out all hole constraints and any constraints which do not mention
a variable mentioned in the type of the hole. For hole `_a`, we will then
only require that the `$dShow_a1pp` constraint is solved, since that is
the only non-hole constraint that mentions any free type variables mentioned in
the hole constraint for `_a`, namely `a_a1pd[tau:2]` , and similarly for the
hole `_b` we only require that the `$dShow_a1pe` constraint is solved.

Note [Leaking errors]
~~~~~~~~~~~~~~~~~~~

When considering candidates, GHC believes that we're checking for validity in
actual source. However, As evidenced by #15321, #15007 and #15202, this can
cause bewildering error messages. The solution here is simple: if a candidate
would cause the type checker to error, it is not a valid hole fit, and thus it
is discarded.

-}


data HoleFitDispConfig = HFDC { showWrap :: Bool
                              , showWrapVars :: Bool
                              , showType :: Bool
                              , showProv :: Bool
                              , showMatches :: Bool }

debugHoleFitDispConfig :: HoleFitDispConfig
debugHoleFitDispConfig = HFDC True True True False False


-- We read the various -no-show-*-of-hole-fits flags
-- and set the display config accordingly.
getHoleFitDispConfig :: TcM HoleFitDispConfig
getHoleFitDispConfig
  = do { sWrap <- goptM Opt_ShowTypeAppOfHoleFits
       ; sWrapVars <- goptM Opt_ShowTypeAppVarsOfHoleFits
       ; sType <- goptM Opt_ShowTypeOfHoleFits
       ; sProv <- goptM Opt_ShowProvOfHoleFits
       ; sMatc <- goptM Opt_ShowMatchesOfHoleFits
       ; return HFDC{ showWrap = sWrap, showWrapVars = sWrapVars
                    , showProv = sProv, showType = sType
                    , showMatches = sMatc } }

-- Which sorting algorithm to use
data SortingAlg = NoSorting      -- Do not sort the fits at all
                | BySize         -- Sort them by the size of the match
                | BySubsumption  -- Sort by full subsumption
                deriving (Eq, Ord)

getSortingAlg :: TcM SortingAlg
getSortingAlg =
    do { shouldSort <- goptM Opt_SortValidHoleFits
       ; subsumSort <- goptM Opt_SortBySubsumHoleFits
       ; sizeSort <- goptM Opt_SortBySizeHoleFits
       -- We default to sizeSort unless it has been explicitly turned off
       -- or subsumption sorting has been turned on.
       ; return $ if not shouldSort
                    then NoSorting
                    else if subsumSort
                         then BySubsumption
                         else if sizeSort
                              then BySize
                              else NoSorting }

-- If enabled, we go through the fits and add any associated documentation,
-- by looking it up in the module or the environment (for local fits)
addDocs :: [HoleFit] -> TcM [HoleFit]
addDocs fits =
  do { showDocs <- goptM Opt_ShowDocsOfHoleFits
     ; if showDocs
       then do { (_, DeclDocMap lclDocs, _) <- extractDocs <$> getGblEnv
               ; mapM (upd lclDocs) fits }
       else return fits }
  where
   msg = text "TcHoleErrors addDocs"
   lookupInIface name (ModIface { mi_decl_docs = DeclDocMap dmap })
     = Map.lookup name dmap
   upd lclDocs fit@(HoleFit {hfCand = cand}) =
        do { let name = getName cand
           ; doc <- if hfIsLcl fit
                    then pure (Map.lookup name lclDocs)
                    else do { mbIface <- loadInterfaceForNameMaybe msg name
                            ; return $ mbIface >>= lookupInIface name }
           ; return $ fit {hfDoc = doc} }
   upd _ fit = return fit

-- For pretty printing hole fits, we display the name and type of the fit,
-- with added '_' to represent any extra arguments in case of a non-zero
-- refinement level.
pprHoleFit :: HoleFitDispConfig -> HoleFit -> SDoc
pprHoleFit _ (RawHoleFit sd) = sd
pprHoleFit (HFDC sWrp sWrpVars sTy sProv sMs) (HoleFit {..}) =
 hang display 2 provenance
 where name =  getName hfCand
       tyApp = sep $ zipWithEqual "pprHoleFit" pprArg vars hfWrap
         where pprArg b arg = case binderArgFlag b of
                                Specified -> text "@" <> pprParendType arg
                                -- Do not print type application for inferred
                                -- variables (#16456)
                                Inferred  -> empty
                                Required  -> pprPanic "pprHoleFit: bad Required"
                                                         (ppr b <+> ppr arg)
       tyAppVars = sep $ punctuate comma $
           zipWithEqual "pprHoleFit" (\v t -> ppr (binderVar v) <+>
                                               text "~" <+> pprParendType t)
           vars hfWrap

       vars = unwrapTypeVars hfType
         where
           -- Attempts to get all the quantified type variables in a type,
           -- e.g.
           -- return :: forall (m :: * -> *) Monad m => (forall a . a -> m a)
           -- into [m, a]
           unwrapTypeVars :: Type -> [TyCoVarBinder]
           unwrapTypeVars t = vars ++ case splitFunTy_maybe unforalled of
                               Just (_, unfunned) -> unwrapTypeVars unfunned
                               _ -> []
             where (vars, unforalled) = splitForAllVarBndrs t
       holeVs = sep $ map (parens . (text "_" <+> dcolon <+>) . ppr) hfMatches
       holeDisp = if sMs then holeVs
                  else sep $ replicate (length hfMatches) $ text "_"
       occDisp = pprPrefixOcc name
       tyDisp = ppWhen sTy $ dcolon <+> ppr hfType
       has = not . null
       wrapDisp = ppWhen (has hfWrap && (sWrp || sWrpVars))
                   $ text "with" <+> if sWrp || not sTy
                                     then occDisp <+> tyApp
                                     else tyAppVars
       docs = case hfDoc of
                Just d -> text "{-^" <>
                          (vcat . map text . lines . unpackHDS) d
                          <> text "-}"
                _ -> empty
       funcInfo = ppWhen (has hfMatches && sTy) $
                    text "where" <+> occDisp <+> tyDisp
       subDisp = occDisp <+> if has hfMatches then holeDisp else tyDisp
       display =  subDisp $$ nest 2 (funcInfo $+$ docs $+$ wrapDisp)
       provenance = ppWhen sProv $ parens $
             case hfCand of
                 GreHFCand gre -> pprNameProvenance gre
                 _ -> text "bound at" <+> ppr (getSrcLoc name)

getLocalBindings :: TidyEnv -> Ct -> TcM [Id]
getLocalBindings tidy_orig ct
 = do { (env1, _) <- zonkTidyOrigin tidy_orig (ctLocOrigin loc)
      ; go env1 [] (removeBindingShadowing $ tcl_bndrs lcl_env) }
  where
    loc     = ctEvLoc (ctEvidence ct)
    lcl_env = ctLocEnv loc

    go :: TidyEnv -> [Id] -> [TcBinder] -> TcM [Id]
    go _ sofar [] = return (reverse sofar)
    go env sofar (tc_bndr : tc_bndrs) =
        case tc_bndr of
          TcIdBndr id _ -> keep_it id
          _ -> discard_it
     where
        discard_it = go env sofar tc_bndrs
        keep_it id = go env (id:sofar) tc_bndrs



-- See Note [Valid hole fits include ...]
findValidHoleFits :: TidyEnv        -- ^ The tidy_env for zonking
                  -> [Implication]  -- ^ Enclosing implications for givens
                  -> [Ct]
                  -- ^ The  unsolved simple constraints in the implication for
                  -- the hole.
                  -> Ct -- ^ The hole constraint itself
                  -> TcM (TidyEnv, SDoc)
findValidHoleFits tidy_env implics simples ct | isExprHoleCt ct =
  do { rdr_env <- getGlobalRdrEnv
     ; lclBinds <- getLocalBindings tidy_env ct
     ; maxVSubs <- maxValidHoleFits <$> getDynFlags
     ; hfdc <- getHoleFitDispConfig
     ; sortingAlg <- getSortingAlg
     ; dflags <- getDynFlags
     ; hfPlugs <- tcg_hf_plugins <$> getGblEnv
     ; let findVLimit = if sortingAlg > NoSorting then Nothing else maxVSubs
           refLevel = refLevelHoleFits dflags
           hole = TyH (listToBag relevantCts) implics (Just ct)
           (candidatePlugins, fitPlugins) =
             unzip $ map (\p-> ((candPlugin p) hole, (fitPlugin p) hole)) hfPlugs
     ; traceTc "findingValidHoleFitsFor { " $ ppr hole
     ; traceTc "hole_lvl is:" $ ppr hole_lvl
     ; traceTc "simples are: " $ ppr simples
     ; traceTc "locals are: " $ ppr lclBinds
     ; let (lcl, gbl) = partition gre_lcl (globalRdrEnvElts rdr_env)
           -- We remove binding shadowings here, but only for the local level.
           -- this is so we e.g. suggest the global fmap from the Functor class
           -- even though there is a local definition as well, such as in the
           -- Free monad example.
           locals = removeBindingShadowing $
                      map IdHFCand lclBinds ++ map GreHFCand lcl
           globals = map GreHFCand gbl
           syntax = map NameHFCand builtIns
           to_check = locals ++ syntax ++ globals
     ; cands <- foldM (flip ($)) to_check candidatePlugins
     ; traceTc "numPlugins are:" $ ppr (length candidatePlugins)
     ; (searchDiscards, subs) <-
        tcFilterHoleFits findVLimit hole (hole_ty, []) cands
     ; (tidy_env, tidy_subs) <- zonkSubs tidy_env subs
     ; tidy_sorted_subs <- sortFits sortingAlg tidy_subs
     ; plugin_handled_subs <- foldM (flip ($)) tidy_sorted_subs fitPlugins
     ; let (pVDisc, limited_subs) = possiblyDiscard maxVSubs plugin_handled_subs
           vDiscards = pVDisc || searchDiscards
     ; subs_with_docs <- addDocs limited_subs
     ; let vMsg = ppUnless (null subs_with_docs) $
                    hang (text "Valid hole fits include") 2 $
                      vcat (map (pprHoleFit hfdc) subs_with_docs)
                        $$ ppWhen vDiscards subsDiscardMsg
     -- Refinement hole fits. See Note [Valid refinement hole fits include ...]
     ; (tidy_env, refMsg) <- if refLevel >= Just 0 then
         do { maxRSubs <- maxRefHoleFits <$> getDynFlags
            -- We can use from just, since we know that Nothing >= _ is False.
            ; let refLvls = [1..(fromJust refLevel)]
            -- We make a new refinement type for each level of refinement, where
            -- the level of refinement indicates number of additional arguments
            -- to allow.
            ; ref_tys <- mapM mkRefTy refLvls
            ; traceTc "ref_tys are" $ ppr ref_tys
            ; let findRLimit = if sortingAlg > NoSorting then Nothing
                                                         else maxRSubs
            ; refDs <- mapM (flip (tcFilterHoleFits findRLimit hole)
                              cands) ref_tys
            ; (tidy_env, tidy_rsubs) <- zonkSubs tidy_env $ concatMap snd refDs
            ; tidy_sorted_rsubs <- sortFits sortingAlg tidy_rsubs
            -- For refinement substitutions we want matches
            -- like id (_ :: t), head (_ :: [t]), asTypeOf (_ :: t),
            -- and others in that vein to appear last, since these are
            -- unlikely to be the most relevant fits.
            ; (tidy_env, tidy_hole_ty) <- zonkTidyTcType tidy_env hole_ty
            ; let hasExactApp = any (tcEqType tidy_hole_ty) . hfWrap
                  (exact, not_exact) = partition hasExactApp tidy_sorted_rsubs
            ; plugin_handled_rsubs <- foldM (flip ($))
                                        (not_exact ++ exact) fitPlugins
            ; let (pRDisc, exact_last_rfits) =
                    possiblyDiscard maxRSubs $ plugin_handled_rsubs
                  rDiscards = pRDisc || any fst refDs
            ; rsubs_with_docs <- addDocs exact_last_rfits
            ; return (tidy_env,
                ppUnless (null rsubs_with_docs) $
                  hang (text "Valid refinement hole fits include") 2 $
                  vcat (map (pprHoleFit hfdc) rsubs_with_docs)
                    $$ ppWhen rDiscards refSubsDiscardMsg) }
       else return (tidy_env, empty)
     ; traceTc "findingValidHoleFitsFor }" empty
     ; return (tidy_env, vMsg $$ refMsg) }
  where
    -- We extract the type, the tcLevel and the types free variables
    -- from from the constraint.
    hole_ty :: TcPredType
    hole_ty = ctPred ct
    hole_fvs :: FV
    hole_fvs = tyCoFVsOfType hole_ty
    hole_lvl = ctLocLevel $ ctEvLoc $ ctEvidence ct

    -- BuiltInSyntax names like (:) and []
    builtIns :: [Name]
    builtIns = filter isBuiltInSyntax knownKeyNames

    -- We make a refinement type by adding a new type variable in front
    -- of the type of t h hole, going from e.g. [Integer] -> Integer
    -- to t_a1/m[tau:1] -> [Integer] -> Integer. This allows the simplifier
    -- to unify the new type variable with any type, allowing us
    -- to suggest a "refinement hole fit", like `(foldl1 _)` instead
    -- of only concrete hole fits like `sum`.
    mkRefTy :: Int -> TcM (TcType, [TcTyVar])
    mkRefTy refLvl = (wrapWithVars &&& id) <$> newTyVars
      where newTyVars = replicateM refLvl $ setLvl <$>
                            (newOpenTypeKind >>= newFlexiTyVar)
            setLvl = flip setMetaTyVarTcLevel hole_lvl
            wrapWithVars vars = mkVisFunTys (map mkTyVarTy vars) hole_ty

    sortFits :: SortingAlg    -- How we should sort the hole fits
             -> [HoleFit]     -- The subs to sort
             -> TcM [HoleFit]
    sortFits NoSorting subs = return subs
    sortFits BySize subs
        = (++) <$> sortBySize (sort lclFits)
               <*> sortBySize (sort gblFits)
        where (lclFits, gblFits) = span hfIsLcl subs

    -- To sort by subsumption, we invoke the sortByGraph function, which
    -- builds the subsumption graph for the fits and then sorts them using a
    -- graph sort.  Since we want locals to come first anyway, we can sort
    -- them separately. The substitutions are already checked in local then
    -- global order, so we can get away with using span here.
    -- We use (<*>) to expose the parallelism, in case it becomes useful later.
    sortFits BySubsumption subs
        = (++) <$> sortByGraph (sort lclFits)
               <*> sortByGraph (sort gblFits)
        where (lclFits, gblFits) = span hfIsLcl subs

    -- See Note [Relevant Constraints]
    relevantCts :: [Ct]
    relevantCts = if isEmptyVarSet (fvVarSet hole_fvs) then []
                  else filter isRelevant simples
      where ctFreeVarSet :: Ct -> VarSet
            ctFreeVarSet = fvVarSet . tyCoFVsOfType . ctPred
            hole_fv_set = fvVarSet hole_fvs
            anyFVMentioned :: Ct -> Bool
            anyFVMentioned ct = ctFreeVarSet ct `intersectsVarSet` hole_fv_set
            -- We filter out those constraints that have no variables (since
            -- they won't be solved by finding a type for the type variable
            -- representing the hole) and also other holes, since we're not
            -- trying to find hole fits for many holes at once.
            isRelevant ct = not (isEmptyVarSet (ctFreeVarSet ct))
                            && anyFVMentioned ct
                            && not (isHoleCt ct)

    -- We zonk the hole fits so that the output aligns with the rest
    -- of the typed hole error message output.
    zonkSubs :: TidyEnv -> [HoleFit] -> TcM (TidyEnv, [HoleFit])
    zonkSubs = zonkSubs' []
      where zonkSubs' zs env [] = return (env, reverse zs)
            zonkSubs' zs env (hf:hfs) = do { (env', z) <- zonkSub env hf
                                           ; zonkSubs' (z:zs) env' hfs }

            zonkSub :: TidyEnv -> HoleFit -> TcM (TidyEnv, HoleFit)
            zonkSub env hf@RawHoleFit{} = return (env, hf)
            zonkSub env hf@HoleFit{hfType = ty, hfMatches = m, hfWrap = wrp}
              = do { (env, ty') <- zonkTidyTcType env ty
                   ; (env, m') <- zonkTidyTcTypes env m
                   ; (env, wrp') <- zonkTidyTcTypes env wrp
                   ; let zFit = hf {hfType = ty', hfMatches = m', hfWrap = wrp'}
                   ; return (env, zFit ) }

    -- Based on the flags, we might possibly discard some or all the
    -- fits we've found.
    possiblyDiscard :: Maybe Int -> [HoleFit] -> (Bool, [HoleFit])
    possiblyDiscard (Just max) fits = (fits `lengthExceeds` max, take max fits)
    possiblyDiscard Nothing fits = (False, fits)

    -- Sort by size uses as a measure for relevance the sizes of the
    -- different types needed to instantiate the fit to the type of the hole.
    -- This is much quicker than sorting by subsumption, and gives reasonable
    -- results in most cases.
    sortBySize :: [HoleFit] -> TcM [HoleFit]
    sortBySize = return . sortOn sizeOfFit
      where sizeOfFit :: HoleFit -> TypeSize
            sizeOfFit = sizeTypes . nubBy tcEqType .  hfWrap

    -- Based on a suggestion by phadej on #ghc, we can sort the found fits
    -- by constructing a subsumption graph, and then do a topological sort of
    -- the graph. This makes the most specific types appear first, which are
    -- probably those most relevant. This takes a lot of work (but results in
    -- much more useful output), and can be disabled by
    -- '-fno-sort-valid-hole-fits'.
    sortByGraph :: [HoleFit] -> TcM [HoleFit]
    sortByGraph fits = go [] fits
      where tcSubsumesWCloning :: TcType -> TcType -> TcM Bool
            tcSubsumesWCloning ht ty = withoutUnification fvs (tcSubsumes ht ty)
              where fvs = tyCoFVsOfTypes [ht,ty]
            go :: [(HoleFit, [HoleFit])] -> [HoleFit] -> TcM [HoleFit]
            go sofar [] = do { traceTc "subsumptionGraph was" $ ppr sofar
                             ; return $ uncurry (++)
                                         $ partition hfIsLcl topSorted }
              where toV (hf, adjs) = (hf, hfId hf, map hfId adjs)
                    (graph, fromV, _) = graphFromEdges $ map toV sofar
                    topSorted = map ((\(h,_,_) -> h) . fromV) $ topSort graph
            go sofar (hf:hfs) =
              do { adjs <-
                     filterM (tcSubsumesWCloning (hfType hf) . hfType) fits
                 ; go ((hf, adjs):sofar) hfs }

-- We don't (as of yet) handle holes in types, only in expressions.
findValidHoleFits env _ _ _ = return (env, empty)


-- | tcFilterHoleFits filters the candidates by whether, given the implications
-- and the relevant constraints, they can be made to match the type by
-- running the type checker. Stops after finding limit matches.
tcFilterHoleFits :: Maybe Int
               -- ^ How many we should output, if limited
               -> TypedHole -- ^ The hole to filter against
               -> (TcType, [TcTyVar])
               -- ^ The type to check for fits and a list of refinement
               -- variables (free type variables in the type) for emulating
               -- additional holes.
               -> [HoleFitCandidate]
               -- ^ The candidates to check whether fit.
               -> TcM (Bool, [HoleFit])
               -- ^ We return whether or not we stopped due to hitting the limit
               -- and the fits we found.
tcFilterHoleFits (Just 0) _ _ _ = return (False, []) -- Stop right away on 0
tcFilterHoleFits limit (TyH {..}) ht@(hole_ty, _) candidates =
  do { traceTc "checkingFitsFor {" $ ppr hole_ty
     ; (discards, subs) <- go [] emptyVarSet limit ht candidates
     ; traceTc "checkingFitsFor }" empty
     ; return (discards, subs) }
  where
    hole_fvs :: FV
    hole_fvs = tyCoFVsOfType hole_ty
    -- Kickoff the checking of the elements.
    -- We iterate over the elements, checking each one in turn for whether
    -- it fits, and adding it to the results if it does.
    go :: [HoleFit]           -- What we've found so far.
       -> VarSet              -- Ids we've already checked
       -> Maybe Int           -- How many we're allowed to find, if limited
       -> (TcType, [TcTyVar]) -- The type, and its refinement variables.
       -> [HoleFitCandidate]  -- The elements we've yet to check.
       -> TcM (Bool, [HoleFit])
    go subs _ _ _ [] = return (False, reverse subs)
    go subs _ (Just 0) _ _ = return (True, reverse subs)
    go subs seen maxleft ty (el:elts) =
        -- See Note [Leaking errors]
        tryTcDiscardingErrs discard_it $
        do { traceTc "lookingUp" $ ppr el
           ; maybeThing <- lookup el
           ; case maybeThing of
               Just id | not_trivial id ->
                       do { fits <- fitsHole ty (idType id)
                          ; case fits of
                              Just (wrp, matches) -> keep_it id wrp matches
                              _ -> discard_it }
               _ -> discard_it }
        where
          -- We want to filter out undefined and the likes from GHC.Err
          not_trivial id = nameModule_maybe (idName id) /= Just gHC_ERR

          lookup :: HoleFitCandidate -> TcM (Maybe Id)
          lookup (IdHFCand id) = return (Just id)
          lookup hfc = do { thing <- tcLookup name
                          ; return $ case thing of
                                       ATcId {tct_id = id} -> Just id
                                       AGlobal (AnId id)   -> Just id
                                       AGlobal (AConLike (RealDataCon con)) ->
                                           Just (dataConWrapId con)
                                       _ -> Nothing }
            where name = case hfc of
                           IdHFCand id -> idName id
                           GreHFCand gre -> gre_name gre
                           NameHFCand name -> name
          discard_it = go subs seen maxleft ty elts
          keep_it eid wrp ms = go (fit:subs) (extendVarSet seen eid)
                                 ((\n -> n - 1) <$> maxleft) ty elts
            where
              fit = HoleFit { hfId = eid, hfCand = el, hfType = (idType eid)
                            , hfRefLvl = length (snd ty)
                            , hfWrap = wrp, hfMatches = ms
                            , hfDoc = Nothing }




    unfoldWrapper :: HsWrapper -> [Type]
    unfoldWrapper = reverse . unfWrp'
      where unfWrp' (WpTyApp ty) = [ty]
            unfWrp' (WpCompose w1 w2) = unfWrp' w1 ++ unfWrp' w2
            unfWrp' _ = []


    -- The real work happens here, where we invoke the type checker using
    -- tcCheckHoleFit to see whether the given type fits the hole.
    fitsHole :: (TcType, [TcTyVar]) -- The type of the hole wrapped with the
                                    -- refinement variables created to simulate
                                    -- additional holes (if any), and the list
                                    -- of those variables (possibly empty).
                                    -- As an example: If the actual type of the
                                    -- hole (as specified by the hole
                                    -- constraint CHoleExpr passed to
                                    -- findValidHoleFits) is t and we want to
                                    -- simulate N additional holes, h_ty will
                                    -- be  r_1 -> ... -> r_N -> t, and
                                    -- ref_vars will be [r_1, ... , r_N].
                                    -- In the base case with no additional
                                    -- holes, h_ty will just be t and ref_vars
                                    -- will be [].
             -> TcType -- The type we're checking to whether it can be
                       -- instantiated to the type h_ty.
             -> TcM (Maybe ([TcType], [TcType])) -- If it is not a match, we
                                                 -- return Nothing. Otherwise,
                                                 -- we Just return the list of
                                                 -- types that quantified type
                                                 -- variables in ty would take
                                                 -- if used in place of h_ty,
                                                 -- and the list types of any
                                                 -- additional holes simulated
                                                 -- with the refinement
                                                 -- variables in ref_vars.
    fitsHole (h_ty, ref_vars) ty =
    -- We wrap this with the withoutUnification to avoid having side-effects
    -- beyond the check, but we rely on the side-effects when looking for
    -- refinement hole fits, so we can't wrap the side-effects deeper than this.
      withoutUnification fvs $
      do { traceTc "checkingFitOf {" $ ppr ty
         ; (fits, wrp) <- tcCheckHoleFit hole h_ty ty
         ; traceTc "Did it fit?" $ ppr fits
         ; traceTc "wrap is: " $ ppr wrp
         ; traceTc "checkingFitOf }" empty
         ; z_wrp_tys <- zonkTcTypes (unfoldWrapper wrp)
         -- We'd like to avoid refinement suggestions like `id _ _` or
         -- `head _ _`, and only suggest refinements where our all phantom
         -- variables got unified during the checking. This can be disabled
         -- with the `-fabstract-refinement-hole-fits` flag.
         -- Here we do the additional handling when there are refinement
         -- variables, i.e. zonk them to read their final value to check for
         -- abstract refinements, and to report what the type of the simulated
         -- holes must be for this to be a match.
         ; if fits
           then if null ref_vars
                then return (Just (z_wrp_tys, []))
                else do { let -- To be concrete matches, matches have to
                              -- be more than just an invented type variable.
                              fvSet = fvVarSet fvs
                              notAbstract :: TcType -> Bool
                              notAbstract t = case getTyVar_maybe t of
                                                Just tv -> tv `elemVarSet` fvSet
                                                _ -> True
                              allConcrete = all notAbstract z_wrp_tys
                        ; z_vars  <- zonkTcTyVars ref_vars
                        ; let z_mtvs = mapMaybe tcGetTyVar_maybe z_vars
                        ; allFilled <- not <$> anyM isFlexiTyVar z_mtvs
                        ; allowAbstract <- goptM Opt_AbstractRefHoleFits
                        ; if allowAbstract || (allFilled && allConcrete )
                          then return $ Just (z_wrp_tys, z_vars)
                          else return Nothing }
           else return Nothing }
     where fvs = mkFVs ref_vars `unionFV` hole_fvs `unionFV` tyCoFVsOfType ty
           hole = TyH tyHRelevantCts tyHImplics Nothing


subsDiscardMsg :: SDoc
subsDiscardMsg =
    text "(Some hole fits suppressed;" <+>
    text "use -fmax-valid-hole-fits=N" <+>
    text "or -fno-max-valid-hole-fits)"

refSubsDiscardMsg :: SDoc
refSubsDiscardMsg =
    text "(Some refinement hole fits suppressed;" <+>
    text "use -fmax-refinement-hole-fits=N" <+>
    text "or -fno-max-refinement-hole-fits)"


-- | Checks whether a MetaTyVar is flexible or not.
isFlexiTyVar :: TcTyVar -> TcM Bool
isFlexiTyVar tv | isMetaTyVar tv = isFlexi <$> readMetaTyVar tv
isFlexiTyVar _ = return False

-- | Takes a list of free variables and restores any Flexi type variables in
-- free_vars after the action is run.
withoutUnification :: FV -> TcM a -> TcM a
withoutUnification free_vars action =
  do { flexis <- filterM isFlexiTyVar fuvs
     ; result <- action
          -- Reset any mutated free variables
     ; mapM_ restore flexis
     ; return result }
  where restore = flip writeTcRef Flexi . metaTyVarRef
        fuvs = fvVarList free_vars

-- | Reports whether first type (ty_a) subsumes the second type (ty_b),
-- discarding any errors. Subsumption here means that the ty_b can fit into the
-- ty_a, i.e. `tcSubsumes a b == True` if b is a subtype of a.
tcSubsumes :: TcSigmaType -> TcSigmaType -> TcM Bool
tcSubsumes ty_a ty_b = fst <$> tcCheckHoleFit dummyHole ty_a ty_b
  where dummyHole = TyH emptyBag [] Nothing

-- | A tcSubsumes which takes into account relevant constraints, to fix trac
-- #14273. This makes sure that when checking whether a type fits the hole,
-- the type has to be subsumed by type of the hole as well as fulfill all
-- constraints on the type of the hole.
-- Note: The simplifier may perform unification, so make sure to restore any
-- free type variables to avoid side-effects.
tcCheckHoleFit :: TypedHole   -- ^ The hole to check against
               -> TcSigmaType
               -- ^ The type to check against (possibly modified, e.g. refined)
               -> TcSigmaType -- ^ The type to check whether fits.
               -> TcM (Bool, HsWrapper)
               -- ^ Whether it was a match, and the wrapper from hole_ty to ty.
tcCheckHoleFit _ hole_ty ty | hole_ty `eqType` ty
    = return (True, idHsWrapper)
tcCheckHoleFit (TyH {..}) hole_ty ty = discardErrs $
  do { -- We wrap the subtype constraint in the implications to pass along the
       -- givens, and so we must ensure that any nested implications and skolems
       -- end up with the correct level. The implications are ordered so that
       -- the innermost (the one with the highest level) is first, so it
       -- suffices to get the level of the first one (or the current level, if
       -- there are no implications involved).
       innermost_lvl <- case tyHImplics of
                          [] -> getTcLevel
                          -- imp is the innermost implication
                          (imp:_) -> return (ic_tclvl imp)
     ; (wrp, wanted) <- setTcLevel innermost_lvl $ captureConstraints $
                          tcSubType_NC ExprSigCtxt ty hole_ty
     ; traceTc "Checking hole fit {" empty
     ; traceTc "wanteds are: " $ ppr wanted
     ; if isEmptyWC wanted && isEmptyBag tyHRelevantCts
       then traceTc "}" empty >> return (True, wrp)
       else do { fresh_binds <- newTcEvBinds
                -- The relevant constraints may contain HoleDests, so we must
                -- take care to clone them as well (to avoid #15370).
               ; cloned_relevants <- mapBagM cloneWanted tyHRelevantCts
                 -- We wrap the WC in the nested implications, see
                 -- Note [Nested Implications]
               ; let outermost_first = reverse tyHImplics
                     setWC = setWCAndBinds fresh_binds
                    -- We add the cloned relevants to the wanteds generated by
                    -- the call to tcSubType_NC, see Note [Relevant Constraints]
                    -- There's no need to clone the wanteds, because they are
                    -- freshly generated by `tcSubtype_NC`.
                     w_rel_cts = addSimples wanted cloned_relevants
                     w_givens = foldr setWC w_rel_cts outermost_first
               ; traceTc "w_givens are: " $ ppr w_givens
               ; rem <- runTcSDeriveds $ simpl_top w_givens
               -- We don't want any insoluble or simple constraints left, but
               -- solved implications are ok (and necessary for e.g. undefined)
               ; traceTc "rems was:" $ ppr rem
               ; traceTc "}" empty
               ; return (isSolvedWC rem, wrp) } }
     where
       setWCAndBinds :: EvBindsVar         -- Fresh ev binds var.
                     -> Implication        -- The implication to put WC in.
                     -> WantedConstraints  -- The WC constraints to put implic.
                     -> WantedConstraints  -- The new constraints.
       setWCAndBinds binds imp wc
         = WC { wc_simple = emptyBag
              , wc_impl = unitBag $ imp { ic_wanted = wc , ic_binds = binds } }

-- | Maps a plugin that needs no state to one with an empty one.
fromPureHFPlugin :: HoleFitPlugin -> HoleFitPluginR
fromPureHFPlugin plug =
  HoleFitPluginR { hfPluginInit = newTcRef ()
                 , hfPluginRun = const plug
                 , hfPluginStop = const $ return () }
