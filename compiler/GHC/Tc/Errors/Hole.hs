{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ExistentialQuantification #-}

module GHC.Tc.Errors.Hole
   ( findValidHoleFits
   , tcCheckHoleFit
   , withoutUnification
   , tcSubsumes
   , isFlexiTyVar
   , tcFilterHoleFits
   , getLocalBindings
   , pprHoleFit
   , addHoleFitDocs
   , getHoleFitSortingAlg
   , getHoleFitDispConfig
   , HoleFitDispConfig (..)
   , HoleFitSortingAlg (..)
   , relevantCtEvidence
   , zonkSubs

   , sortHoleFitsByGraph
   , sortHoleFitsBySize


   -- Re-exported from GHC.Tc.Errors.Hole.Plugin
   , HoleFitPlugin (..), HoleFitPluginR (..)
   )
where

import GHC.Prelude

import GHC.Tc.Errors.Types ( HoleFitDispConfig(..), FitsMbSuppressed(..)
                           , ValidHoleFits(..), noValidHoleFits )
import GHC.Tc.Types
import GHC.Tc.Utils.Monad
import GHC.Tc.Types.Constraint
import GHC.Tc.Types.Origin
import GHC.Tc.Utils.TcMType
import GHC.Tc.Types.Evidence
import GHC.Tc.Types.CtLoc
import GHC.Tc.Utils.TcType
import GHC.Tc.Zonk.TcType
import GHC.Core.Type
import GHC.Core.TyCon( TyCon, isGenerativeTyCon )
import GHC.Core.TyCo.Rep( Type(..) )
import GHC.Core.DataCon
import GHC.Core.Predicate( Pred(..), classifyPredType, eqRelRole )
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Builtin.Names ( gHC_INTERNAL_ERR, gHC_INTERNAL_UNSAFE_COERCE )
import GHC.Types.Id
import GHC.Types.Var.Set
import GHC.Types.Var.Env
import GHC.Types.TyThing
import GHC.Data.Bag
import GHC.Core.ConLike ( ConLike(..) )
import GHC.Utils.Misc
import GHC.Utils.Panic
import GHC.Tc.Utils.Env (tcLookup)
import GHC.Utils.Outputable
import GHC.Driver.DynFlags
import GHC.Data.Maybe
import GHC.Utils.FV ( fvVarList, fvVarSet, unionFV, mkFVs, FV )

import Control.Arrow ( (&&&) )

import Control.Monad    ( filterM, replicateM, foldM )
import Data.List        ( partition, sort, sortOn, nubBy )
import Data.Graph       ( graphFromEdges, topSort )


import GHC.Tc.Solver    ( simplifyTopWanteds )
import GHC.Tc.Solver.Monad ( runTcSEarlyAbort )
import GHC.Tc.Utils.Unify ( tcSubTypeSigma )

import GHC.HsToCore.Docs ( extractDocs )
import GHC.Hs.Doc
import GHC.Unit.Module.ModIface ( mi_docs )
import GHC.Iface.Load  ( loadInterfaceForName )

import GHC.Builtin.Utils (knownKeyNames)

import GHC.Tc.Errors.Hole.FitTypes
import GHC.Tc.Errors.Hole.Plugin
import qualified Data.Set as Set
import GHC.Types.SrcLoc
import GHC.Data.FastString (NonDetFastString(..))
import GHC.Types.Unique.Map


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
in scope for whether their type can be instantiated to the type of the hole.
Additionally, we also need to check whether all relevant constraints are solved
by choosing an identifier of that type as well, see Note [Relevant constraints]

Since checking for subsumption results in the side-effect of type variables
being unified by the simplifier, we need to take care to restore them after
to being flexible type variables after we've checked for subsumption.
This is to avoid affecting the hole and later checks by prematurely having
unified one of the free unification variables.

When outputting, we sort the hole fits by the size of the types we'd need to
apply by type application to the type of the fit to make it fit. This is done
in order to display "more relevant" suggestions first. Another option is to
sort by building a subsumption graph of fits, i.e. a graph of which fits subsume
what other fits, and then outputting those fits which are subsumed by other
fits (i.e. those more specific than other fits) first. This results in the ones
"closest" to the type of the hole to be displayed first.

To help users understand how the suggested fit works, we also display the values
that the quantified type variables would take if that fit is used, like
`mempty @([Char] -> [String])` and `pure @[] @String` in the example above.
If -XTypeApplications is enabled, this can even be copied verbatim as a
replacement for the hole.

Note [Checking hole fits]
~~~~~~~~~~~~~~~~~~~~~~~~~
If we have a hole of type hole_ty, we want to know whether a variable
of type ty is a valid fit for the whole. This is a subsumption check:
we wish to know whether ty <: hole_ty. But, of course, the check
must take into account any givens and relevant constraints.
(See also Note [Relevant constraints]).

For the simplifier to be able to use any givens present in the enclosing
implications to solve relevant constraints, we nest the wanted subsumption
constraints and relevant constraints within the enclosing implications.

As an example, let's look at the following code:

  f :: Show a => a -> String
  f x = show _

Suppose the hole is assigned type a0_a1pd[tau:2].
Here the nested implications are just one level deep, namely:

  [Implic {
      TcLevel = 2
      Skolems = a_a1pa[sk:2]
      No-eqs = True
      Status = Unsolved
      Given = $dShow_a1pc :: Show a_a1pa[sk:2]
      Wanted =
        WC {wc_simple =
              [W] $dShow_a1pe {0}:: Show a0_a1pd[tau:2] (CDictCan(psc))}
      Binds = EvBindsVar<a1pi>
      Needed inner = []
      Needed outer = []
      the type signature for:
        f :: forall a. Show a => a -> String }]

As we can see, the givens say that the skolem
`a_a1pa[sk:2]` fulfills the Show constraint, and that we must prove
the [W] Show a0_a1pd[tau:2] constraint -- that is, whatever fills the
hole must have a Show instance.

When we now check whether `x :: a_a1pa[sk:2]` fits the hole in
`tcCheckHoleFit`, the call to `tcSubType` will end up unifying the meta type
variable `a0_a1pd[tau:2] := a_a1pa[sk:2]`. By wrapping the wanted constraints
needed by tcSubType_NC and the relevant constraints (see Note [Relevant
constraints] for more details) in the nested implications, we can pass the
information in the givens along to the simplifier. For our example, we end up
needing to check whether the following constraints are soluble.

  WC {wc_impl =
        Implic {
          TcLevel = 2
          Skolems = a_a1pa[sk:2]
          No-eqs = True
          Status = Unsolved
          Given = $dShow_a1pc :: Show a_a1pa[sk:2]
          Wanted =
            WC {wc_simple =
                  [W] $dShow_a1pe {0}:: Show a0_a1pd[tau:2] (CNonCanonical)}
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
In addition, we call withoutUnification to reset any unified metavariables; this
call is actually done outside tcCheckHoleFit so that the results can be formatted
for the user before resetting variables.

Note [Valid refinement hole fits include ...]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

Note [Relevant constraints]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
As highlighted by #14273, we need to check any relevant constraints as well
as checking for subsumption. Relevant constraints are the simple constraints
whose free unification variables are mentioned in the type of the hole.

In the simplest case, these are all non-hole constraints in the simples, such
as is the case in

  f :: String
  f = show _

Here, the hole is given type a0_a1kv[tau:1]. Then, the emitted constraint is:

  [W] $dShow_a1kw {0}:: Show a0_a1kv[tau:1] (CNonCanonical)

However, when there are multiple holes, we need to be more careful. As an
example, Let's take a look at the following code:

  f :: Show a => a -> String
  f x = show (_b (show _a))

Here there are two holes, `_a` and `_b`. Suppose _a :: a0_a1pd[tau:2] and
_b :: a1_a1po[tau:2]. Then, the simple constraints passed to
findValidHoleFits are:

  [[W] $dShow_a1pe {0}:: Show a0_a1pd[tau:2] (CNonCanonical),
    [W] $dShow_a1pp {0}:: Show a1_a1po[tau:2] (CNonCanonical)]

When we are looking for a match for the hole `_a`, we filter the simple
constraints to the "Relevant constraints", by throwing out any constraints
which do not mention a variable mentioned in the type of the hole. For hole
`_a`, we will then only require that the `$dShow_a1pe` constraint is solved,
since that is the only constraint that mentions any free type variables
mentioned in the hole constraint for `_a`, namely `a_a1pd[tau:2]`, and
similarly for the hole `_b` we only require that the `$dShow_a1pe` constraint
is solved.

Note [Leaking errors]
~~~~~~~~~~~~~~~~~~~~~
When considering candidates, GHC believes that we're checking for validity in
actual source. However, As evidenced by #15321, #15007 and #15202, this can
cause bewildering error messages. The solution here is simple: if a candidate
would cause the type checker to error, it is not a valid hole fit, and thus it
is discarded.

Note [Speeding up valid hole-fits]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
To fix #16875 we noted that a lot of time was being spent on unnecessary work.

When we'd call `tcCheckHoleFit hole hole_ty ty`, we would end up by generating
a constraint to show that `hole_ty ~ ty`, including any constraints in `ty`. For
example, if `hole_ty = Int` and `ty = Foldable t => (a -> Bool) -> t a -> Bool`,
we'd have `(a_a1pa[sk:1] -> Bool) -> t_t2jk[sk:1] a_a1pa[sk:1] -> Bool ~# Int`
from the coercion, as well as `Foldable t_t2jk[sk:1]`. By adding a flag to
`TcSEnv` and adding a `runTcSEarlyAbort`, we can fail as soon as we hit
an insoluble constraint. Since we don't need the result in the case that it
fails, a boolean `False` (i.e. "it didn't work" from `runTcSEarlyAbort`)
is sufficient.

We also check whether the type of the hole is an immutable type variable (i.e.
a skolem). In that case, the only possible fits are fits of exactly that type,
which can only come from the locals. This speeds things up quite a bit when we
don't know anything about the type of the hole. This also helps with degenerate
fits like (`id (_ :: a)` and `head (_ :: [a])`) when looking for fits of type
`a`, where `a` is a skolem.
-}

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
data HoleFitSortingAlg = HFSNoSorting      -- Do not sort the fits at all
                       | HFSBySize         -- Sort them by the size of the match
                       | HFSBySubsumption  -- Sort by full subsumption
                deriving (Eq, Ord)

getHoleFitSortingAlg :: TcM HoleFitSortingAlg
getHoleFitSortingAlg =
    do { shouldSort <- goptM Opt_SortValidHoleFits
       ; subsumSort <- goptM Opt_SortBySubsumHoleFits
       ; sizeSort <- goptM Opt_SortBySizeHoleFits
       -- We default to sizeSort unless it has been explicitly turned off
       -- or subsumption sorting has been turned on.
       ; return $ if not shouldSort
                    then HFSNoSorting
                    else if subsumSort
                         then HFSBySubsumption
                         else if sizeSort
                              then HFSBySize
                              else HFSNoSorting }

-- If enabled, we go through the fits and add any associated documentation,
-- by looking it up in the module or the environment (for local fits)
addHoleFitDocs :: [HoleFit] -> TcM [HoleFit]
addHoleFitDocs fits =
  do { showDocs <- goptM Opt_ShowDocsOfHoleFits
     ; if showDocs
       then do { dflags <- getDynFlags
               ; mb_local_docs <- extractDocs dflags =<< getGblEnv
               ; (mods_without_docs, fits') <- mapAccumLM (upd mb_local_docs) Set.empty fits
               ; report mods_without_docs
               ; return fits' }
       else return fits }
  where
   msg = text "GHC.Tc.Errors.Hole addHoleFitDocs"
   upd mb_local_docs mods_without_docs (TcHoleFit fit@(HoleFit {hfCand = cand})) =
     let name = getName cand in
     do { mb_docs <- if hfIsLcl fit
                     then pure mb_local_docs
                     else mi_docs <$> loadInterfaceForName msg name
        ; case mb_docs of
            { Nothing -> return (Set.insert (nameOrigin name) mods_without_docs, TcHoleFit fit)
            ; Just docs -> do
                { let doc = lookupUniqMap (docs_decls docs) name
                ; return $ (mods_without_docs, TcHoleFit (fit {hfDoc = map hsDocString <$> doc})) }}}
   upd _ mods_without_docs fit@(RawHoleFit {}) = pure (mods_without_docs, fit)
   nameOrigin name = case nameModule_maybe name of
     Just m  -> Right m
     Nothing ->
       Left $ case nameSrcLoc name of
         -- Nondeterminism is fine, this is used only to display a warning
         RealSrcLoc r _ -> NonDetFastString $ srcLocFile r
         UnhelpfulLoc s -> NonDetFastString s
   report mods = do
     { let warning =
             text "WARNING: Couldn't find any documentation for the following modules:" $+$
             nest 2
                  (pprWithCommas (either ppr ppr) (Set.toList mods) $+$
                   text "Make sure the modules are compiled with '-haddock'.")
     ; warnPprTrace (not $ Set.null mods) "addHoleFitDocs" warning (pure ())
     }

-- For pretty printing hole fits, we display the name and type of the fit,
-- with added '_' to represent any extra arguments in case of a non-zero
-- refinement level.
pprHoleFit :: HoleFitDispConfig -> HoleFit -> SDoc
pprHoleFit _ (RawHoleFit sd) = sd
pprHoleFit (HFDC sWrp sWrpVars sTy sProv sMs) (TcHoleFit (HoleFit {..})) =
 hang display 2 provenance
 where tyApp = sep $ zipWithEqual "pprHoleFit" pprArg vars hfWrap
         where pprArg b arg = case binderFlag b of
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
           unwrapTypeVars :: Type -> [ForAllTyBinder]
           unwrapTypeVars t = vars ++ case splitFunTy_maybe unforalled of
                               Just (_, _, _, unfunned) -> unwrapTypeVars unfunned
                               _ -> []
             where (vars, unforalled) = splitForAllForAllTyBinders t
       holeVs = sep $ map (parens . (text "_" <+> dcolon <+>) . ppr) hfMatches
       holeDisp = if sMs then holeVs
                  else sep $ replicate (length hfMatches) $ text "_"
       occDisp = case hfCand of
                   GreHFCand gre   -> pprPrefixOcc (greName gre)
                   NameHFCand name -> pprPrefixOcc name
                   IdHFCand id_    -> pprPrefixOcc id_
       tyDisp = ppWhen sTy $ dcolon <+> ppr hfType
       has = not . null
       wrapDisp = ppWhen (has hfWrap && (sWrp || sWrpVars))
                   $ text "with" <+> if sWrp || not sTy
                                     then occDisp <+> tyApp
                                     else tyAppVars
       docs = case hfDoc of
                Just d -> pprHsDocStrings d
                _ -> empty
       funcInfo = ppWhen (has hfMatches && sTy) $
                    text "where" <+> occDisp <+> tyDisp
       subDisp = occDisp <+> if has hfMatches then holeDisp else tyDisp
       display =  subDisp $$ nest 2 (funcInfo $+$ docs $+$ wrapDisp)
       provenance = ppWhen sProv $ parens $
             case hfCand of
                 GreHFCand gre -> pprNameProvenance gre
                 NameHFCand name -> text "bound at" <+> ppr (getSrcLoc name)
                 IdHFCand id_ -> text "bound at" <+> ppr (getSrcLoc id_)

getLocalBindings :: TidyEnv -> CtLoc -> TcM [Id]
getLocalBindings tidy_orig ct_loc
 = do { (env1, _) <- liftZonkM $ zonkTidyOrigin tidy_orig (ctLocOrigin ct_loc)
      ; go env1 [] (removeBindingShadowing $ ctl_bndrs lcl_env) }
  where
    lcl_env = ctLocEnv ct_loc

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
                  -> [CtEvidence]
                  -- ^ The  unsolved simple constraints in the implication for
                  -- the hole.
                  -> Hole
                  -> TcM (TidyEnv, ValidHoleFits)
findValidHoleFits tidy_env implics simples h@(Hole { hole_sort = ExprHole _
                                                   , hole_loc  = ct_loc
                                                   , hole_ty   = hole_ty }) =
  do { rdr_env <- getGlobalRdrEnv
     ; lclBinds <- getLocalBindings tidy_env ct_loc
     ; maxVSubs <- maxValidHoleFits <$> getDynFlags
     ; sortingAlg <- getHoleFitSortingAlg
     ; dflags <- getDynFlags
     ; hfPlugs <- tcg_hf_plugins <$> getGblEnv
     ; let findVLimit = if sortingAlg > HFSNoSorting then Nothing else maxVSubs
           refLevel = refLevelHoleFits dflags
           hole = TypedHole { th_relevant_cts =
                                listToBag (relevantCtEvidence hole_ty simples)
                            , th_implics      = implics
                            , th_hole         = Just h }
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
           -- If the hole is a rigid type-variable, then we only check the
           -- locals, since only they can match the type (in a meaningful way).
           only_locals = any isImmutableTyVar $ getTyVar_maybe hole_ty
           to_check = if only_locals then locals
                      else locals ++ syntax ++ globals
     ; cands <- foldM (flip ($)) to_check candidatePlugins
     ; traceTc "numPlugins are:" $ ppr (length candidatePlugins)
     ; (searchDiscards, subs) <-
        tcFilterHoleFits findVLimit hole (hole_ty, []) cands
     ; (tidy_env, tidy_subs) <- liftZonkM $ zonkSubs tidy_env subs
     ; tidy_sorted_subs <- sortFits sortingAlg tidy_subs
     ; let apply_plugin :: [HoleFit] -> ([HoleFit] -> TcM [HoleFit]) -> TcM [HoleFit]
           apply_plugin fits plug = plug fits
     ; plugin_handled_subs <- foldM apply_plugin (map TcHoleFit tidy_sorted_subs) fitPlugins
     ; let (pVDisc, limited_subs) = possiblyDiscard maxVSubs plugin_handled_subs
           vDiscards = pVDisc || searchDiscards
     ; subs_with_docs <- addHoleFitDocs limited_subs
     ; let subs = Fits subs_with_docs vDiscards
     -- Refinement hole fits. See Note [Valid refinement hole fits include ...]
     ; (tidy_env, rsubs) <-
       if refLevel >= Just 0
       then
         do { maxRSubs <- maxRefHoleFits <$> getDynFlags
            -- We can use from just, since we know that Nothing >= _ is False.
            ; let refLvls = [1..(fromJust refLevel)]
            -- We make a new refinement type for each level of refinement, where
            -- the level of refinement indicates number of additional arguments
            -- to allow.
            ; ref_tys <- mapM mkRefTy refLvls
            ; traceTc "ref_tys are" $ ppr ref_tys
            ; let findRLimit = if sortingAlg > HFSNoSorting then Nothing
                                                            else maxRSubs
            ; refDs :: [(Bool, [TcHoleFit])]
                 <- mapM (flip (tcFilterHoleFits findRLimit hole) cands) ref_tys
            ; (tidy_env, tidy_rsubs :: [TcHoleFit])
                 <- liftZonkM $ zonkSubs tidy_env $ concatMap snd refDs
            ; tidy_sorted_rsubs :: [TcHoleFit] <- sortFits sortingAlg tidy_rsubs
            -- For refinement substitutions we want matches
            -- like id (_ :: t), head (_ :: [t]), asTypeOf (_ :: t),
            -- and others in that vein to appear last, since these are
            -- unlikely to be the most relevant fits.
            ; (tidy_env, tidy_hole_ty) <- liftZonkM $ zonkTidyTcType tidy_env hole_ty
            ; let hasExactApp = any (tcEqType tidy_hole_ty) . hfWrap
                  exact, not_exact :: [TcHoleFit]
                  (exact, not_exact) = partition hasExactApp tidy_sorted_rsubs
                  fits :: [HoleFit] = map TcHoleFit (not_exact ++ exact)
            ; plugin_handled_rsubs <- foldM apply_plugin fits fitPlugins
            ; let (pRDisc, exact_last_rfits) =
                    possiblyDiscard maxRSubs $ plugin_handled_rsubs
                  rDiscards = pRDisc || any fst refDs
            ; rsubs_with_docs <- addHoleFitDocs exact_last_rfits
            ; return (tidy_env, Fits rsubs_with_docs rDiscards) }
       else return (tidy_env, Fits [] False)
     ; traceTc "findingValidHoleFitsFor }" empty
     ; let hole_fits = ValidHoleFits subs rsubs
     ; return (tidy_env, hole_fits) }
  where
    -- We extract the TcLevel from the constraint.
    hole_lvl = ctLocLevel ct_loc

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
      where newTyVars = replicateM refLvl $ setLvl <$> newOpenFlexiTyVar
            setLvl = flip setMetaTyVarTcLevel hole_lvl
            wrapWithVars vars = mkVisFunTysMany (map mkTyVarTy vars) hole_ty

    sortFits :: HoleFitSortingAlg    -- How we should sort the hole fits
             -> [TcHoleFit]     -- The subs to sort
             -> TcM [TcHoleFit]
    sortFits HFSNoSorting subs = return subs
    sortFits HFSBySize subs
        = (++) <$> sortHoleFitsBySize (sort lclFits)
               <*> sortHoleFitsBySize (sort gblFits)
        where (lclFits, gblFits) = span hfIsLcl subs
    -- To sort by subsumption, we invoke the sortByGraph function, which
    -- builds the subsumption graph for the fits and then sorts them using a
    -- graph sort.  Since we want locals to come first anyway, we can sort
    -- them separately. The substitutions are already checked in local then
    -- global order, so we can get away with using span here.
    -- We use (<*>) to expose the parallelism, in case it becomes useful later.
    sortFits HFSBySubsumption subs
        = (++) <$> sortHoleFitsByGraph (sort lclFits)
               <*> sortHoleFitsByGraph (sort gblFits)
        where (lclFits, gblFits) = span hfIsLcl subs

    -- Based on the flags, we might possibly discard some or all the
    -- fits we've found.
    possiblyDiscard :: Maybe Int -> [HoleFit] -> (Bool, [HoleFit])
    possiblyDiscard (Just max) fits = (fits `lengthExceeds` max, take max fits)
    possiblyDiscard Nothing fits = (False, fits)


-- We don't (as of yet) handle holes in types, only in expressions.
findValidHoleFits env _ _ _ = return (env, noValidHoleFits)

-- See Note [Relevant constraints]
relevantCtEvidence :: Type -> [CtEvidence] -> [CtEvidence]
relevantCtEvidence hole_ty simples
  = if isEmptyVarSet (fvVarSet hole_fvs)
    then []
    else filter isRelevant simples
  where hole_fvs = tyCoFVsOfType hole_ty
        hole_fv_set = fvVarSet hole_fvs
        -- We filter out those constraints that have no variables (since
        -- they won't be solved by finding a type for the type variable
        -- representing the hole) and also other holes, since we're not
        -- trying to find hole fits for many holes at once.
        isRelevant ctev = not (isEmptyVarSet fvs) &&
                          (fvs `intersectsVarSet` hole_fv_set)
          where fvs = tyCoVarsOfCtEv ctev

-- We zonk the hole fits so that the output aligns with the rest
-- of the typed hole error message output.
zonkSubs :: TidyEnv -> [TcHoleFit] -> ZonkM (TidyEnv, [TcHoleFit])
zonkSubs = zonkSubs' []
  where zonkSubs' zs env [] = return (env, reverse zs)
        zonkSubs' zs env (hf:hfs) = do { (env', z) <- zonkSub env hf
                                        ; zonkSubs' (z:zs) env' hfs }

        zonkSub :: TidyEnv -> TcHoleFit -> ZonkM (TidyEnv, TcHoleFit)
        zonkSub env hf@HoleFit{hfType = ty, hfMatches = m, hfWrap = wrp}
            = do { (env, ty') <- zonkTidyTcType env ty
                ; (env, m')   <- zonkTidyTcTypes env m
                ; (env, wrp') <- zonkTidyTcTypes env wrp
                ; let zFit = hf {hfType = ty', hfMatches = m', hfWrap = wrp'}
                ; return (env, zFit ) }

-- | Sort by size uses as a measure for relevance the sizes of the different
-- types needed to instantiate the fit to the type of the hole.
-- This is much quicker than sorting by subsumption, and gives reasonable
-- results in most cases.
sortHoleFitsBySize :: [TcHoleFit] -> TcM [TcHoleFit]
sortHoleFitsBySize = return . sortOn sizeOfFit
  where sizeOfFit :: TcHoleFit -> TypeSize
        sizeOfFit = sizeTypes . nubBy tcEqType .  hfWrap

-- Based on a suggestion by phadej on #ghc, we can sort the found fits
-- by constructing a subsumption graph, and then do a topological sort of
-- the graph. This makes the most specific types appear first, which are
-- probably those most relevant. This takes a lot of work (but results in
-- much more useful output), and can be disabled by
-- '-fno-sort-valid-hole-fits'.
sortHoleFitsByGraph :: [TcHoleFit] -> TcM [TcHoleFit]
sortHoleFitsByGraph fits = go [] fits
  where tcSubsumesWCloning :: TcType -> TcType -> TcM Bool
        tcSubsumesWCloning ht ty = withoutUnification fvs (tcSubsumes ht ty)
          where fvs = tyCoFVsOfTypes [ht,ty]
        go :: [(TcHoleFit, [TcHoleFit])] -> [TcHoleFit] -> TcM [TcHoleFit]
        go sofar [] = do { traceTc "subsumptionGraph was" $ ppr sofar
                         ; return $ uncurry (++) $ partition hfIsLcl topSorted }
          where toV (hf, adjs) = (hf, hfId hf, map hfId adjs)
                (graph, fromV, _) = graphFromEdges $ map toV sofar
                topSorted = map ((\(h,_,_) -> h) . fromV) $ topSort graph
        go sofar (hf:hfs) =
          do { adjs <- filterM (tcSubsumesWCloning (hfType hf) . hfType) fits
             ; go ((hf, adjs):sofar) hfs }

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
               -> TcM (Bool, [TcHoleFit])
               -- ^ We return whether or not we stopped due to hitting the limit
               -- and the fits we found.
tcFilterHoleFits (Just 0) _ _ _ = return (False, []) -- Stop right away on 0
tcFilterHoleFits limit typed_hole ht@(hole_ty, _) candidates =
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
    go :: [TcHoleFit]           -- What we've found so far.
       -> VarSet              -- Ids we've already checked
       -> Maybe Int           -- How many we're allowed to find, if limited
       -> (TcType, [TcTyVar]) -- The type, and its refinement variables.
       -> [HoleFitCandidate]  -- The elements we've yet to check.
       -> TcM (Bool, [TcHoleFit])
    go subs _ _ _ [] = return (False, reverse subs)
    go subs _ (Just 0) _ _ = return (True, reverse subs)
    go subs seen maxleft ty (el:elts) =
        -- See Note [Leaking errors]
        tryTcDiscardingErrs discard_it $
        do { traceTc "lookingUp" $ ppr el
           ; maybeThing <- lookup el
           ; case maybeThing of
               Just (id, id_ty) | not_trivial id ->
                       do { fits <- fitsHole ty id_ty
                          ; case fits of
                              Just (wrp, matches) -> keep_it id id_ty wrp matches
                              _ -> discard_it }
               _ -> discard_it }
        where
          -- We want to filter out undefined and the likes from GHC.Err (#17940)
          not_trivial id = nameModule_maybe (idName id) `notElem` [Just gHC_INTERNAL_ERR, Just gHC_INTERNAL_UNSAFE_COERCE]

          lookup :: HoleFitCandidate -> TcM (Maybe (Id, Type))
          lookup (IdHFCand id) = return (Just (id, idType id))
          lookup hfc = do { thing <- tcLookup name
                          ; return $ case thing of
                                       ATcId {tct_id = id} -> Just (id, idType id)
                                       AGlobal (AnId id)   -> Just (id, idType id)
                                       AGlobal (AConLike (RealDataCon con)) ->
                                           Just (dataConWrapId con, dataConNonlinearType con)
                                       _ -> Nothing }
            where name = case hfc of
                           GreHFCand gre   -> greName gre
                           NameHFCand name -> name
          discard_it = go subs seen maxleft ty elts
          keep_it eid eid_ty wrp ms = go (fit:subs) (extendVarSet seen eid)
                                 ((\n -> n - 1) <$> maxleft) ty elts
            where
              fit = HoleFit { hfId = eid, hfCand = el, hfType = eid_ty
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
         -- We'd like to avoid refinement suggestions like `id _ _` or
         -- `head _ _`, and only suggest refinements where our all phantom
         -- variables got unified during the checking. This can be disabled
         -- with the `-fabstract-refinement-hole-fits` flag.
         -- Here we do the additional handling when there are refinement
         -- variables, i.e. zonk them to read their final value to check for
         -- abstract refinements, and to report what the type of the simulated
         -- holes must be for this to be a match.
         ; if fits then do {
              -- Zonking is expensive, so we only do it if required.
              z_wrp_tys <- liftZonkM $ zonkTcTypes (unfoldWrapper wrp)
            ; if null ref_vars
              then return (Just (z_wrp_tys, []))
              else do { let -- To be concrete matches, matches have to
                            -- be more than just an invented type variable.
                            fvSet = fvVarSet fvs
                            notAbstract :: TcType -> Bool
                            notAbstract t = case getTyVar_maybe t of
                                              Just tv -> tv `elemVarSet` fvSet
                                              _ -> True
                            allConcrete = all notAbstract z_wrp_tys
                      ; z_vars  <- liftZonkM $ zonkTcTyVars ref_vars
                      ; let z_mtvs = mapMaybe getTyVar_maybe z_vars
                      ; allFilled <- not <$> anyM isFlexiTyVar z_mtvs
                      ; allowAbstract <- goptM Opt_AbstractRefHoleFits
                      ; if allowAbstract || (allFilled && allConcrete )
                        then return $ Just (z_wrp_tys, z_vars)
                        else return Nothing }}
           else return Nothing }
     where fvs = mkFVs ref_vars `unionFV` hole_fvs `unionFV` tyCoFVsOfType ty
           hole = typed_hole { th_hole = Nothing }



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
  where restore tv = do { traceTc "withoutUnification: restore flexi" (ppr tv)
                        ; writeTcRef (metaTyVarRef tv) Flexi }
        fuvs = fvVarList free_vars

-- | Reports whether first type (ty_a) subsumes the second type (ty_b),
-- discarding any errors. Subsumption here means that the ty_b can fit into the
-- ty_a, i.e. `tcSubsumes a b == True` if b is a subtype of a.
tcSubsumes :: TcSigmaType -> TcSigmaType -> TcM Bool
tcSubsumes ty_a ty_b = fst <$> tcCheckHoleFit dummyHole ty_a ty_b
  where dummyHole = TypedHole { th_relevant_cts = emptyBag
                              , th_implics      = []
                              , th_hole         = Nothing }

-- | A tcSubsumes which takes into account relevant constraints, to fix
-- #14273. This makes sure that when checking whether a type fits the hole,
-- the type has to be subsumed by type of the hole as well as fulfill all
-- constraints on the type of the hole.
tcCheckHoleFit :: TypedHole   -- ^ The hole to check against
               -> TcSigmaType
               -- ^ The type of the hole to check against (possibly modified,
               -- e.g. refined with additional holes for refinement hole-fits.)
               -> TcSigmaType -- ^ The type to check whether fits.
               -> TcM (Bool, HsWrapper)
               -- ^ Whether it was a match, and the wrapper from hole_ty to ty.
tcCheckHoleFit _ hole_ty ty | hole_ty `eqType` ty
    = return (True, idHsWrapper)
tcCheckHoleFit (TypedHole {..}) hole_ty ty = discardErrs $
  do { -- We wrap the subtype constraint in the implications to pass along the
       -- givens, and so we must ensure that any nested implications and skolems
       -- end up with the correct level. The implications are ordered so that
       -- the innermost (the one with the highest level) is first, so it
       -- suffices to get the level of the first one (or the current level, if
       -- there are no implications involved).
       innermost_lvl <- case th_implics of
                          [] -> getTcLevel
                          -- imp is the innermost implication
                          (imp:_) -> return (ic_tclvl imp)
     ; (wrap, wanted) <- setTcLevel innermost_lvl $ captureConstraints $
                         tcSubTypeSigma orig (ExprSigCtxt NoRRC) ty hole_ty
     ; traceTc "Checking hole fit {" empty
     ; traceTc "wanteds are: " $ ppr wanted
     ; if | isEmptyWC wanted, isEmptyBag th_relevant_cts
          -> do { traceTc "}" empty
                ; return (True, wrap) }

          | checkInsoluble wanted -- See Note [Fast path for tcCheckHoleFit]
          -> return (False, wrap)

          | otherwise
          -> do { fresh_binds <- newTcEvBinds
                 -- The relevant constraints may contain HoleDests, so we must
                 -- take care to clone them as well (to avoid #15370).
                ; cloned_relevants <- mapBagM cloneWantedCtEv th_relevant_cts
                  -- We wrap the WC in the nested implications, for details, see
                  -- Note [Checking hole fits]
                ; let wrapInImpls cts = foldl (flip (setWCAndBinds fresh_binds)) cts th_implics
                      final_wc  = wrapInImpls $ addSimples wanted $
                                  mapBag mkNonCanonical cloned_relevants
                  -- We add the cloned relevants to the wanteds generated
                  -- by the call to tcSubType_NC, for details, see
                  -- Note [Relevant constraints]. There's no need to clone
                  -- the wanteds, because they are freshly generated by the
                  -- call to`tcSubtype_NC`.
                ; traceTc "final_wc is: " $ ppr final_wc
                  -- See Note [Speeding up valid hole-fits]
                ; (rem, _) <- tryTc $ runTcSEarlyAbort $ simplifyTopWanteds final_wc
                ; traceTc "}" empty
                ; return (any isSolvedWC rem, wrap) } }
  where
    orig = ExprHoleOrigin (hole_occ <$> th_hole)

    setWCAndBinds :: EvBindsVar         -- Fresh ev binds var.
                  -> Implication        -- The implication to put WC in.
                  -> WantedConstraints  -- The WC constraints to put implic.
                  -> WantedConstraints  -- The new constraints.
    setWCAndBinds binds imp wc
      = mkImplicWC $ unitBag $ imp { ic_wanted = wc , ic_binds = binds }

{- Note [Fast path for tcCheckHoleFit]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In `tcCheckHoleFit` we compare (with `tcSubTypeSigma`) the type of the hole
with the type of zillions of in-scope functions, to see which would "fit".
Most of these checks fail!  They generate obviously-insoluble constraints.
For these very-common cases we don't want to crank up the full constraint
solver.  It's much more efficient to do a quick-and-dirty check for insolubility.

Now, `tcSubTypeSigma` uses the on-the-fly unifier in GHC.Tc.Utils.Unify,
it has already done the dirt-simple unification. So our quick-and-dirty
check can simply look for constraints like (Int ~ Bool).  We don't need
to worry about (Maybe Int ~ Maybe Bool).

The quick-and-dirty check is in `checkInsoluble`. It can make a big
difference: For test hard_hole_fits, compile-time allocation goes down by 37%!
-}


checkInsoluble :: WantedConstraints -> Bool
-- See Note [Fast path for tcCheckHoleFit]
checkInsoluble (WC { wc_simple = simples })
  = any is_insol simples
  where
    is_insol ct = case classifyPredType (ctPred ct) of
                    EqPred r t1 t2 -> definitelyNotEqual (eqRelRole r) t1 t2
                    _              -> False

definitelyNotEqual :: Role -> TcType -> TcType -> Bool
-- See Note [Fast path for tcCheckHoleFit]
-- Specifically, does not need to recurse under type constructors
definitelyNotEqual r t1 t2
  = go t1 t2
  where
    go t1 t2
      | Just t1' <- coreView t1 = go t1' t2
      | Just t2' <- coreView t2 = go t1 t2'

    go (TyConApp tc _) t2 | isGenerativeTyCon tc r = go_tc tc t2
    go t1 (TyConApp tc _) | isGenerativeTyCon tc r = go_tc tc t1
    go (FunTy {ft_af = af1}) (FunTy {ft_af = af2}) = af1 /= af2
    go _ _ = False

    go_tc :: TyCon -> TcType -> Bool
    -- The TyCon is generative, and is not a saturated FunTy
    go_tc tc1 (TyConApp tc2 _) | isGenerativeTyCon tc2 r = tc1 /= tc2
    go_tc _ (FunTy {})    = True
    go_tc _ (ForAllTy {}) = True
    go_tc _ _ = False
