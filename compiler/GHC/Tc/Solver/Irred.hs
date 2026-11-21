{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecursiveDo #-}

module GHC.Tc.Solver.Irred(
     solveIrred, isOpaqueTermIndex
  ) where

import GHC.Prelude

import GHC.Core.Coercion
import GHC.Core.Predicate
import GHC.Core.Type
import GHC.Core.TyCo.Compare ( tcEqType )
import GHC.Core.TyCo.FVs ( mkUnivCoDeps, univCoDepsHoles )
import GHC.Core.TyCo.Rep ( Type(..) )
import GHC.Tc.Types.Constraint
import GHC.Tc.Solver.InertSet
import GHC.Tc.Solver.Dict( matchLocalInst, chooseInstance )
import GHC.Tc.Solver.Monad
import GHC.Tc.Types.Evidence

import GHC.Types.Basic( SwapFlag(..) )

import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Data.Bag

import Control.Applicative ( (<|>) )
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
  , not (typeKind lhs `tcEqType` typeKind rhs)
  , Just co <- findCastedIndexGiven (inert_irreds inerts) lhs rhs
  = do { let holes = foldr (flip addRewriter) emptyCoHoleSet
                          (univCoDepsHoles (mkUnivCoDeps [co]))
       ; setWantedEq dest (CPH co holes)
       ; stopWith ev_w "Solved by transported index Given" }

  | otherwise
  = continueWith ()

  where
    ct_w = CIrredCan irred_w

-- | Is this the type of an occurrence of a rigid runtime index?
isOpaqueTermIndex :: Type -> Bool
isOpaqueTermIndex (TermVarTy v) = isNativeOpaqueValueId v
isOpaqueTermIndex _ = False

-- | Recover the nominal index equality from a homogeneous Given.
-- See Note [Using casted index Givens].
findCastedIndexGiven :: InertIrreds -> Type -> Type -> Maybe Coercion
findCastedIndexGiven irreds lhs rhs =
    foldIrreds (\irred rest -> match irred <|> rest) irreds Nothing
  where
    match irred
      | ev@CtGiven {} <- irredCtEvidence irred
      , EqPred NomEq left right <- classifyPredType (ctEvPred ev)
      = let co = ctEvCoercion ev
        in orient left right co <|> orient right left (mkSymCo co)
      | otherwise = Nothing

    orient (CastTy ty kind_co) other co
      | ty `tcEqType` lhs, other `tcEqType` rhs
      = Just (mkGReflRightCo Nominal ty kind_co `mkTransCo` co)
    orient other (CastTy ty kind_co) co
      | other `tcEqType` lhs, ty `tcEqType` rhs
      = Just (co `mkTransCo` mkSymCo (mkGReflRightCo Nominal ty kind_co))
    orient _ _ _ = Nothing

{- Note [Using casted index Givens]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
An HRefl match on xs :~~: 'VNil binds a kind equality and a nominal
Given ('VNil |> ck) ~# xs. Keeping the cast keeps both kinds equal.
Reconstructing HRefl can still ask for the original heterogeneous
index equality. First try the ordinary matching rule. If it fails,
use the scoped Given with one endpoint cast and compare the endpoints
before transport, including their kinds. Apply the kind coercion in
the Wanted proof. No new Given or runtime index assignment is needed.

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
