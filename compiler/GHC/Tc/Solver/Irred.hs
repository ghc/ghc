{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecursiveDo #-}

module GHC.Tc.Solver.Irred(
     solveIrred
  ) where

import GHC.Prelude

import GHC.Tc.Types.Constraint
import GHC.Tc.Solver.InertSet
import GHC.Tc.Solver.Dict( matchLocalInst, chooseInstance )
import GHC.Tc.Solver.Monad
import GHC.Tc.Types.Evidence

import GHC.Types.Basic( SwapFlag(..) )

import GHC.Utils.Outputable
import GHC.Utils.Panic

import GHC.Data.Bag

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

  | otherwise
  = continueWith ()

  where
    ct_w = CIrredCan irred_w

setIrredIfWanted :: CtEvidence -> SwapFlag -> CtEvidence -> TcS ()
-- Irreds can be equalities or dictionaries
setIrredIfWanted ev_dest swap ev_source
  | CtWanted (WantedCt { ctev_dest = dest }) <- ev_dest
  = case dest of
      HoleDest hole -> setWantedEq dest $
                       CPH { cph_co    = maybeSymCo swap (ctEvCoercion ev_source)
                           , cph_holes = unitCoHoleSet hole }

      EvVarDest {} -> assertPpr (swap==NotSwapped) (ppr ev_dest $$ ppr ev_source) $
                         -- findMatchingIrreds only returns IsSwapped for equalities
                      setWantedDict dest EvCanonical (ctEvTerm ev_source)
  | otherwise
  = return ()

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
      CtWanted wev@(WantedCt { ctev_loc = loc, ctev_pred = pred })
        -> do { res <- matchLocalInst pred loc
              ; case res of
                  OneInst {} -> do { chooseInstance wev res
                                   ; stopWith ev "Irred (solved wanted)" }
                  _          -> continueWith () }
