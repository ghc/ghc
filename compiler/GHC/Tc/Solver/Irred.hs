{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecursiveDo #-}

module GHC.Tc.Solver.Irred(
     solveIrred, isOpaqueTermIndex
  ) where

import GHC.Prelude

import qualified GHC.Core as Core
import GHC.Core.Coercion
import GHC.Core.Dependent ( hasNativeDependentExpr )
import GHC.Core.Map.Type ( emptyTypeMap, extendTypeMap, lookupTypeMap )
import GHC.Core.Predicate
import GHC.Core.TyCon ( TyCon, TyConBndrVis(..), isPromotedDataCon, tyConBinders )
import GHC.Core.Type
import GHC.Core.TyCo.Compare ( tcEqType )
import GHC.Core.TyCo.FVs ( mkUnivCoDeps, univCoDepsHoles )
import GHC.Core.TyCo.Rep ( Type(..) )
import GHC.Tc.Types.Constraint
import GHC.Tc.Solver.InertSet
import GHC.Tc.Solver.Dict( matchLocalInst, chooseInstance )
import GHC.Tc.Solver.Monad
import GHC.Tc.Types.Evidence
import GHC.Tc.Utils.TcType ( isMetaTyVar )

import GHC.Types.Basic( SwapFlag(..) )
import GHC.Types.Var ( Id, VarBndr(..) )
import GHC.Types.Var.Set ( isEmptyVarSet )

import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Data.Bag
import GHC.Data.TrieMap ( alterTM )

import Control.Applicative ( (<|>) )
import Data.Maybe ( fromMaybe, isJust, listToMaybe )
import Data.Void( Void )


{- *********************************************************************
*                                                                      *
*                      Irreducibles
*                                                                      *
********************************************************************* -}

solveIrred :: IrredCt -> SolverStage Void
solveIrred irred
  = do { simpleStage $ traceTcS "solveIrred:" (ppr irred)
       ; tryInertIrreds irred
       ; tryQCsIrredCt irred
       ; simpleStage (updInertIrreds irred)
       ; stopWithStage (irredCtEvidence irred) "Kept inert IrredCt" }


{- *********************************************************************
*                                                                      *
*                      Inert Irreducibles
*                                                                      *
********************************************************************* -}

-- Two pieces of irreducible evidence: if their types are *exactly identical*
-- we can rewrite them. We can never improve using this:
-- if we want ty1 :: Constraint and have ty2 :: Constraint it clearly does not
-- mean that (ty1 ~ ty2)
tryInertIrreds :: IrredCt -> SolverStage ()
tryInertIrreds irred
  = Stage $ do { ics <- getInertCans
               ; try_inert_irreds ics irred }

try_inert_irreds :: InertCans -> IrredCt -> TcS (StopOrContinue ())

try_inert_irreds inerts irred_w@(IrredCt { ir_ev = ev_w, ir_reason = reason })
  | let (matching_irreds, others) = findMatchingIrreds (inert_irreds inerts) ev_w
  , ((irred_i, swap) : _rest) <- bagToList matching_irreds
        -- See Note [Multiple matching irreds]
  , let ev_i = irredCtEvidence irred_i
        ct_i = CIrredCan irred_i
  , not (isInsolubleReason reason) || isGiven ev_i || isGiven ev_w
        -- See Note [Insoluble irreds]
  = do { traceTcS "iteractIrred" $
         vcat [ text "wanted:" <+> (ppr ct_w $$ ppr (ctOrigin ct_w))
              , text "inert: " <+> (ppr ct_i $$ ppr (ctOrigin ct_i)) ]
       ; case solveOneFromTheOther ct_i ct_w of
            KeepInert -> do { setIrredIfWanted ev_w swap ev_i
                            ; return (Stop ev_w (text "Irred equal:KeepInert" <+> ppr ct_w)) }
            KeepWork ->  do { setIrredIfWanted ev_i swap ev_w
                            ; updInertCans (updIrreds (\_ -> others))
                            ; continueWith () } }

  | CtWanted (WantedCt { ctev_dest = dest }) <- ev_w
  , EqPred NomEq lhs rhs <- classifyPredType (ctEvPred ev_w)
  , isOpaqueTermIndex lhs || isOpaqueTermIndex rhs
  , Just co <- findRetainedIndexProof (inert_irreds inerts) lhs rhs
  = do { let holes = foldr (flip addRewriter) emptyCoHoleSet
                          (univCoDepsHoles (mkUnivCoDeps [co]))
       ; setWantedEq dest (CPH co holes)
       ; stopWith ev_w "Solved by transported index Given" }

  -- See Note [Using a promoted head from a retained index Given].
  | CtWanted (WantedCt { ctev_dest = dest }) <- ev_w
  , EqPred NomEq lhs rhs <- classifyPredType (ctEvPred ev_w)
  , Just (swap, index, tc, meta, expected) <-
      promoted_wanted NotSwapped lhs rhs <|> promoted_wanted IsSwapped rhs lhs
  = do { binders <- getNativeBinderScope
       ; unfilled <- isUnfilledMetaTyVar meta
       ; case if isEmptyVarSet binders && unfilled
              then findPromotedIndexGiven (inert_irreds inerts) index tc
              else Nothing of
           Nothing -> continueWith ()
           Just (known, known_co) ->
             do { (child, hole) <- newWantedEq (ctEvLoc ev_w) (ctEvRewriters ev_w)
                                              Nominal known expected
                ; emitChildEqs ev_w (unitBag (mkNonCanonical (CtWanted child)))
                ; let co = maybeSymCo swap (known_co `mkTransCo` mkHoleCo hole)
                ; setWantedEq dest (CPH co (unitCoHoleSet hole))
                ; stopWith ev_w "Used promoted head from retained index Given" } }

  | otherwise
  = continueWith ()

  where
    ct_w = CIrredCan irred_w
    promoted_wanted swap (TermVarTy index) ty@(TyConApp tc [TyVarTy meta])
      | isNativeIndexId index
      , isPromotedDataCon tc
      , [Bndr _ AnonTCB] <- tyConBinders tc
      , isMetaTyVar meta
      = Just (swap, index, tc, meta, ty)
    promoted_wanted _ _ _ = Nothing

-- | Find the nominal Given that fixes an argument of this promoted head.
-- See Note [Using a promoted head from a retained index Given].
findPromotedIndexGiven :: InertIrreds -> Id -> TyCon -> Maybe (Type, Coercion)
findPromotedIndexGiven irreds index head_tc =
    foldIrreds (\irred rest -> match irred <|> rest) irreds Nothing
  where
    match irred
      | ev@CtGiven {} <- irredCtEvidence irred
      , EqPred NomEq lhs rhs <- classifyPredType (ctEvPred ev)
      = let co = ctEvCoercion ev
        in orient lhs rhs co <|> orient rhs lhs (mkSymCo co)
      | otherwise = Nothing

    orient (TermVarTy witness) ty@(TyConApp tc [arg]) co
      | witness == index, tc == head_tc
      , not (hasNativeDependentExpr (Core.Type arg))
      = Just (ty, co)
    orient _ _ _ = Nothing

{- Note [Using a promoted head from a retained index Given]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Matching a Volume at a known successor can put S layers ~# field in
scope. Passing its stack to firstLayer asks for field ~# S alpha.
Exact irred matching works if the call supplies @layers. For a fresh
erased alpha, compose the nominal Given with a Wanted S layers ~# S
alpha. Ordinary decomposition and unification check the predecessor.

Look for the same retained Id and a saturated unary promoted head.
The argument from the Given must contain no retained syntax. Keep the
metavariable level checks in the ordinary solver. The child has promoted
heads at both endpoints and cannot repeat this rule. The proof uses the
in-scope Given and the child hole. A private runtime-binder comparison
keeps its existing read-only scope.
-}

-- | Is this the type of an occurrence of a rigid runtime index?
isOpaqueTermIndex :: Type -> Bool
isOpaqueTermIndex (TermVarTy v) = isNativeOpaqueValueId v
isOpaqueTermIndex _ = False

-- | Recover a nominal index equality from scoped homogeneous Givens.
-- See Note [Using casted index Givens].
findRetainedIndexProof :: InertIrreds -> Type -> Type -> Maybe Coercion
findRetainedIndexProof irreds lhs rhs = direct <|> chained
  where
    direct
      | typeKind lhs `tcEqType` typeKind rhs = Nothing
      | otherwise = listToMaybe
          [ co
          | (left, right, co) <- edges
          , left `sameEndpoint` lhs
          , right `sameEndpoint` rhs ]

    chained
      | isOpaqueTermIndex lhs = findPath lhs rhs
      | otherwise = mkSymCo <$> findPath rhs lhs

    findPath start target = go emptyTypeMap [(start, [])]
      where
        go _ [] = Nothing
        go seen ((current, path) : pending)
          | sameEndpoint current target
          , co:rest <- path
          = Just (foldl' (flip mkTransCo) co rest)
          | not (isOpaqueTermIndex current) || isJust (lookupTypeMap seen current)
          = go seen pending
          | otherwise
          = go (extendTypeMap seen current ()) (foldr push pending next)
          where
            next = fromMaybe [] (lookupTypeMap graph current)
            push (endpoint, co) todo = (endpoint, co:path) : todo

    graph = foldl' addArc emptyTypeMap edges

    addArc arcs (from, to, co)
      | isOpaqueTermIndex from = alterTM from (Just . ((to, co):) . fromMaybe []) arcs
      | otherwise = arcs

    sameEndpoint left right = left `tcEqType` right && typeKind left `tcEqType` typeKind right

    edges = foldIrreds addEdges irreds []

    addEdges irred rest
      | ev@CtGiven {} <- irredCtEvidence irred
      , EqPred NomEq left right <- classifyPredType (ctEvPred ev)
      = givenEdges left right (ctEvCoercion ev) rest
      | otherwise = rest

    givenEdges left@(CastTy {}) right co rest =
      uncastEdge left right co (uncastEdge right left (mkSymCo co) rest)
    givenEdges left right@(CastTy {}) co rest =
      uncastEdge right left (mkSymCo co) rest
    givenEdges left right co rest
      | isOpaqueTermIndex left || isOpaqueTermIndex right
      , typeKind left `tcEqType` typeKind right
      = bothDirections left right co rest
      | otherwise = rest

    uncastEdge (CastTy ty kind_co) other co rest =
      bothDirections ty other (mkGReflRightCo Nominal ty kind_co `mkTransCo` co) rest
    uncastEdge _ _ _ rest = rest

    bothDirections left right co rest =
      (left, right, co) : (right, left, mkSymCo co) : rest

{- Note [Using casted index Givens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An HRefl match on xs :~~: 'VNil binds a kind equality and a nominal
Given ('VNil |> ck) ~# xs. Keeping the cast keeps both kinds equal.
Reconstructing HRefl can still ask for the original heterogeneous
index equality. First try the ordinary matching rule. If it fails,
use the scoped Given with one endpoint cast and compare the endpoints
before transport, including their kinds. Apply the kind coercion in
the Wanted proof.

A retained package can carry xs :~~: 'VNil and ys :~~: xs. Matching both
proofs supplies two Givens. The Wanted ys :~~: 'VNil can use their
composition. Undo a kind cast if the Given has one. Follow the available
nominal proofs through opaque retained indices and compose the path to
the Wanted endpoint. The Givens stay homogeneous in their original scopes.

Canonical type-variable rewriting uses the terminating substitution in
the inert equalities. An opaque retained index cannot be a substitution
key. Here the direct lookup remains the fast path. If it fails, index
the scoped nominal Givens by their retained endpoints. Use TypeMap for
both adjacency and visited endpoints. The keys include the retained Id
and its kind. A path can follow each endpoint at most once. Each visit
adds its outgoing proofs to the work list once. A proof with a
non-retained endpoint can finish the path. It cannot extend the search.
Compose only the successful path. This creates no new constraints.

See Note [Opaque runtime index equalities] in GHC.Tc.Solver.Equality.
-}

setIrredIfWanted :: CtEvidence -> SwapFlag -> CtEvidence -> TcS ()
-- Irreds can be equalities or dictionaries
setIrredIfWanted ev_dest swap ev_source
  | CtWanted (WantedCt { ctev_dest = dest }) <- ev_dest
  = case dest of
      HoleDest {} -> setWantedEq dest $
                     CPH { cph_co    = maybeSymCo swap (ctEvCoercion ev_source)
                         , cph_holes = ctEvCoHoleSet ev_source }
                     -- See Note [Holes in evidence for equal irreducibles].

      EvVarDest {} -> assertPpr (swap==NotSwapped) (ppr ev_dest $$ ppr ev_source) $
                         -- findMatchingIrreds only returns IsSwapped for equalities
                      setWantedDict dest EvCanonical (ctEvTerm ev_source)
  | otherwise
  = return ()

{- Note [Holes in evidence for equal irreducibles]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When a matching irreducible solves a Wanted equality, its coercion
comes from the source constraint. A Given contributes no hole. A Wanted
contributes its source hole, also when the coercion is symmetric.
Recording the destination hole here makes zonkCoHoleSet follow that
hole back to itself when another Wanted asks for its rewriters.
See Note [Coercion holes] in GHC.Core.TyCo.Rep.
-}

{- Note [Multiple matching irreds]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
You might think that it's impossible to have multiple irreds all match the
work item; after all, interactIrred looks for matches and solves one from the
other. However, note that interacting insoluble, non-droppable irreds does not
do this matching. We thus might end up with several insoluble, non-droppable,
matching irreds in the inert set. When another irred comes along that we have
not yet labeled insoluble, we can find multiple matches. These multiple matches
cause no harm, but it would be wrong to ASSERT that they aren't there (as we
once had done). This problem can be tickled by typecheck/should_compile/holes.

Note [Insoluble irreds]
~~~~~~~~~~~~~~~~~~~~~~~
We don't allow an /insoluble/ Wanted to be solved from another identical
Wanted.  We want to keep all the insoluble Wanteds distinct, so that we get
distinct error messages with -fdefer-type-errors

However we /do/ allow an insoluble constraint (Given or Wanted) to be solved
from an identical insoluble Given.  This might seem a little odd, but there is
lots of discussion in #23413 and #17543.  We currently implement the PermissivePlan
of #23413.  An alternative would be the LibertarianPlan, but that is harder to
implemnent.

By "identical" we include swapping.  See Note [Solving irreducible equalities]
in GHC.Tc.Solver.InertSet.

Test cases that are involved bkpfail24.run, T15450, GivenForallLoop, T20189, T8392a.
-}

{- *********************************************************************
*                                                                      *
*                      Quantified constraints
*                                                                      *
********************************************************************* -}

tryQCsIrredCt :: IrredCt -> SolverStage ()
-- Try local quantified constraints for
-- and CIrredCan e.g.  (c a)
tryQCsIrredCt (IrredCt { ir_ev = ev })
  = Stage $ case ev of
      CtGiven {}
        -> continueWith ()
      CtWanted wev@(WantedCt { ctev_loc = loc })
        -> do { res <- matchLocalInst (wantedCtPredType wev) loc
              ; case res of
                  OneInst {} -> do { chooseInstance wev res
                                   ; stopWith ev "Irred (solved wanted)" }
                  _          -> continueWith () }
