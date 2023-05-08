{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecursiveDo #-}

module GHC.Tc.Solver.Irred(
     solveIrred, tryInertIrreds, tryQCsIrredCt
  ) where

import GHC.Prelude

import GHC.Tc.Types.Constraint
import GHC.Tc.Solver.InertSet
import GHC.Tc.Solver.Dict( matchLocalInst, chooseInstance )
import GHC.Tc.Solver.Monad
import GHC.Tc.Types.Evidence

import GHC.Core.Coercion
import GHC.Core.InstEnv ( Coherence(..) )

import GHC.Types.Basic( SwapFlag(..) )

import GHC.Utils.Outputable


import GHC.Data.Bag


{- *********************************************************************
*                                                                      *
*                      Irreducibles
*                                                                      *
********************************************************************* -}

solveIrred :: IrredCt -> SolverStage Ct
solveIrred irred
  = do { tryInertIrreds irred
       ; tryQCsIrredCt irred
       ; return (CIrredCan irred) }

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
  | isInsolubleReason reason
               -- For insolubles, don't allow the constraint to be dropped
               -- which can happen with solveOneFromTheOther, so that
               -- we get distinct error messages with -fdefer-type-errors
  = continueWith ()

  | let (matching_irreds, others) = findMatchingIrreds (inert_irreds inerts) ev_w
  , ((irred_i, swap) : _rest) <- bagToList matching_irreds
        -- See Note [Multiple matching irreds]
  , let ev_i = irredCtEvidence irred_i
        ct_i = CIrredCan irred_i
  = do { traceTcS "iteractIrred" $
         vcat [ text "wanted:" <+> (ppr ct_w $$ ppr (ctOrigin ct_w))
              , text "inert: " <+> (ppr ct_i $$ ppr (ctOrigin ct_i)) ]
       ; case solveOneFromTheOther ct_i ct_w of
            KeepInert -> do { setEvBindIfWanted ev_w IsCoherent (swap_me swap ev_i)
                            ; return (Stop ev_w (text "Irred equal:KeepInert" <+> ppr ct_w)) }
            KeepWork ->  do { setEvBindIfWanted ev_i IsCoherent (swap_me swap ev_w)
                            ; updInertIrreds (\_ -> others)
                            ; continueWith () } }

  | otherwise
  = continueWith ()

  where
    ct_w = CIrredCan irred_w

    swap_me :: SwapFlag -> CtEvidence -> EvTerm
    swap_me swap ev
      = case swap of
           NotSwapped -> ctEvTerm ev
           IsSwapped  -> evCoercion (mkSymCo (evTermCoercion (ctEvTerm ev)))


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
  | isGiven ev
  = Stage $ continueWith ()

  | otherwise
  = Stage $ do { res <- matchLocalInst pred loc
               ; case res of
                    OneInst {} -> chooseInstance ev res
                    _          -> continueWith () }
  where
    loc  = ctEvLoc ev
    pred = ctEvPred ev
