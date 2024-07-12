{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module RunCmm
  ( evalGraph
  )
where

-- Using a `ControlTestMonad` to provide observations,
-- simulate the execution of a `CmmGraph`.

import Prelude hiding (succ)

import GHC.Cmm
import GHC.Cmm.Dataflow.Block
import GHC.Cmm.Dataflow.Graph
import GHC.Cmm.Dataflow.Label.NonDet
import GHC.Cmm.Switch
import GHC.Utils.Panic

import ControlTestMonad

evalGraph :: forall stmt exp m .
             ControlTestMonad stmt exp m
            => (Label -> Block CmmNode O O -> stmt)
            -> (Label -> CmmExpr -> exp)
            -> CmmGraph
            -> m ()
evalGraph stmt exp g = run (g_entry g)
  where GMany NothingO blockmap NothingO = g_graph g
        run :: Label -> m ()
        run label = do
          takeAction @stmt @exp (stmt label (actionOf label))
          case lastNode (blockOf label) of
            CmmBranch l -> run l
            CmmCondBranch e t f _ -> do
                      b <- evalPredicate @stmt @exp (exp label e)
                      run (if b then t else f)
            CmmSwitch e targets -> do
                      i <- evalEnum @stmt @exp (exp label e) $
                           extendRight $ switchTargetsRange targets
                      run $ labelIn i targets

            CmmCall { cml_cont = Just l } -> run l
            CmmCall { cml_cont = Nothing } -> return ()
            CmmForeignCall { succ = l } -> run l

        blockOf lbl =
             mapFindWithDefault (panic "GHC.Cmm.ControlFlow.Run.eval") lbl blockmap
        actionOf lbl = middle
            where (_, middle, _) = blockSplit $ blockOf lbl



-- | Adapt between different representations of ranges
extendRight :: Integral n => (n, n) -> (n, n)
extendRight (lo, hi) = (lo, hi + 1)

labelIn :: Integer -> SwitchTargets -> Label
labelIn i targets =
    case [lbl | (j, lbl) <- switchTargetsCases targets, j == i]
      of [lbl] -> lbl
         [] -> case switchTargetsDefault targets of
                 Just lbl -> lbl
                 Nothing -> panic "GHC.Cmm.ControlFlow.Run.labelIn: no default"
         (_ : _ : _) -> panic "GHC.Cmm.ControlFlow.Run: too many matches"
