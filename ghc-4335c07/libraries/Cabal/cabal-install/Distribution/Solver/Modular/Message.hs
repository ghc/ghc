{-# LANGUAGE BangPatterns #-}

module Distribution.Solver.Modular.Message (
    Message(..),
    showMessages
  ) where

import qualified Data.List as L
import Prelude hiding (pi)

import Distribution.Text -- from Cabal

import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.Tree
         ( FailReason(..), POption(..) )
import Distribution.Solver.Types.ConstraintSource
import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.Progress

data Message =
    Enter           -- ^ increase indentation level
  | Leave           -- ^ decrease indentation level
  | TryP QPN POption
  | TryF QFN Bool
  | TryS QSN Bool
  | Next (Goal QPN)
  | Success
  | Failure ConflictSet FailReason

-- | Transforms the structured message type to actual messages (strings).
--
-- Takes an additional relevance predicate. The predicate gets a stack of goal
-- variables and can decide whether messages regarding these goals are relevant.
-- You can plug in 'const True' if you're interested in a full trace. If you
-- want a slice of the trace concerning a particular conflict set, then plug in
-- a predicate returning 'True' on the empty stack and if the head is in the
-- conflict set.
--
-- The second argument indicates if the level numbers should be shown. This is
-- recommended for any trace that involves backtracking, because only the level
-- numbers will allow to keep track of backjumps.
showMessages :: ([Var QPN] -> Bool) -> Bool -> Progress Message a b -> Progress String a b
showMessages p sl = go [] 0
  where
    -- The stack 'v' represents variables that are currently assigned by the
    -- solver.  'go' pushes a variable for a recursive call when it encounters
    -- 'TryP', 'TryF', or 'TryS' and pops a variable when it encounters 'Leave'.
    -- When 'go' processes a package goal, or a package goal followed by a
    -- 'Failure', it calls 'atLevel' with the goal variable at the head of the
    -- stack so that the predicate can also select messages relating to package
    -- goal choices.
    go :: [Var QPN] -> Int -> Progress Message a b -> Progress String a b
    go !_ !_ (Done x)                           = Done x
    go !_ !_ (Fail x)                           = Fail x
    -- complex patterns
    go !v !l (Step (TryP qpn i) (Step Enter (Step (Failure c fr) (Step Leave ms)))) =
        goPReject v l qpn [i] c fr ms
    go !v !l (Step (TryF qfn b) (Step Enter (Step (Failure c fr) (Step Leave ms)))) =
        (atLevel (F qfn : v) l $ "rejecting: " ++ showQFNBool qfn b ++ showFR c fr) (go v l ms)
    go !v !l (Step (TryS qsn b) (Step Enter (Step (Failure c fr) (Step Leave ms)))) =
        (atLevel (S qsn : v) l $ "rejecting: " ++ showQSNBool qsn b ++ showFR c fr) (go v l ms)
    go !v !l (Step (Next (Goal (P qpn) gr)) (Step (TryP qpn' i) ms@(Step Enter (Step (Next _) _)))) =
        (atLevel (P qpn : v) l $ "trying: " ++ showQPNPOpt qpn' i ++ showGR gr) (go (P qpn : v) l ms)
    go !v !l (Step (Next (Goal (P qpn) gr)) ms@(Fail _)) =
        (atLevel (P qpn : v) l $ "unknown package: " ++ showQPN qpn ++ showGR gr) $ go v l ms
        -- the previous case potentially arises in the error output, because we remove the backjump itself
        -- if we cut the log after the first error
    go !v !l (Step (Next (Goal (P qpn) gr)) ms@(Step (Failure _c Backjump) _)) =
        (atLevel (P qpn : v) l $ "unknown package: " ++ showQPN qpn ++ showGR gr) $ go v l ms
    go !v !l (Step (Next (Goal (P qpn) gr)) (Step (Failure c fr) ms)) =
        let v' = P qpn : v
        in (atLevel v' l $ showPackageGoal qpn gr) $ (atLevel v' l $ showFailure c fr) (go v l ms)
    go !v !l (Step (Failure c Backjump) ms@(Step Leave (Step (Failure c' Backjump) _)))
        | c == c' = go v l ms
    -- standard display
    go !v !l (Step Enter                    ms) = go v          (l+1) ms
    go !v !l (Step Leave                    ms) = go (drop 1 v) (l-1) ms
    go !v !l (Step (TryP qpn i)             ms) = (atLevel (P qpn : v) l $ "trying: " ++ showQPNPOpt qpn i) (go (P qpn : v) l ms)
    go !v !l (Step (TryF qfn b)             ms) = (atLevel (F qfn : v) l $ "trying: " ++ showQFNBool qfn b) (go (F qfn : v) l ms)
    go !v !l (Step (TryS qsn b)             ms) = (atLevel (S qsn : v) l $ "trying: " ++ showQSNBool qsn b) (go (S qsn : v) l ms)
    go !v !l (Step (Next (Goal (P qpn) gr)) ms) = (atLevel (P qpn : v) l $ showPackageGoal qpn gr) (go v l ms)
    go !v !l (Step (Next _)                 ms) = go v l ms -- ignore flag goals in the log
    go !v !l (Step (Success)                ms) = (atLevel v l $ "done") (go v l ms)
    go !v !l (Step (Failure c fr)           ms) = (atLevel v l $ showFailure c fr) (go v l ms)

    showPackageGoal :: QPN -> QGoalReason -> String
    showPackageGoal qpn gr = "next goal: " ++ showQPN qpn ++ showGR gr

    showFailure :: ConflictSet -> FailReason -> String
    showFailure c fr = "fail" ++ showFR c fr

    -- special handler for many subsequent package rejections
    goPReject :: [Var QPN]
              -> Int
              -> QPN
              -> [POption]
              -> ConflictSet
              -> FailReason
              -> Progress Message a b
              -> Progress String a b
    goPReject v l qpn is c fr (Step (TryP qpn' i) (Step Enter (Step (Failure _ fr') (Step Leave ms))))
      | qpn == qpn' && fr == fr' = goPReject v l qpn (i : is) c fr ms
    goPReject v l qpn is c fr ms =
        (atLevel (P qpn : v) l $ "rejecting: " ++ L.intercalate ", " (map (showQPNPOpt qpn) (reverse is)) ++ showFR c fr) (go v l ms)

    -- write a message, but only if it's relevant; we can also enable or disable the display of the current level
    atLevel :: [Var QPN] -> Int -> String -> Progress String a b -> Progress String a b
    atLevel v l x xs
      | sl && p v = let s = show l
                    in  Step ("[" ++ replicate (3 - length s) '_' ++ s ++ "] " ++ x) xs
      | p v       = Step x xs
      | otherwise = xs

showQPNPOpt :: QPN -> POption -> String
showQPNPOpt qpn@(Q _pp pn) (POption i linkedTo) =
  case linkedTo of
    Nothing  -> showPI (PI qpn i) -- Consistent with prior to POption
    Just pp' -> showQPN qpn ++ "~>" ++ showPI (PI (Q pp' pn) i)

showGR :: QGoalReason -> String
showGR UserGoal            = " (user goal)"
showGR (DependencyGoal dr) = " (dependency of " ++ showDependencyReason dr ++ ")"

showFR :: ConflictSet -> FailReason -> String
showFR _ InconsistentInitialConstraints   = " (inconsistent initial constraints)"
showFR _ (Conflicting ds)                 = " (conflict: " ++ L.intercalate ", " (L.map showDep ds) ++ ")"
showFR _ CannotInstall                    = " (only already installed instances can be used)"
showFR _ CannotReinstall                  = " (avoiding to reinstall a package with same version but new dependencies)"
showFR _ Shadowed                         = " (shadowed by another installed package with same version)"
showFR _ Broken                           = " (package is broken)"
showFR _ (GlobalConstraintVersion vr src) = " (" ++ constraintSource src ++ " requires " ++ display vr ++ ")"
showFR _ (GlobalConstraintInstalled src)  = " (" ++ constraintSource src ++ " requires installed instance)"
showFR _ (GlobalConstraintSource src)     = " (" ++ constraintSource src ++ " requires source instance)"
showFR _ (GlobalConstraintFlag src)       = " (" ++ constraintSource src ++ " requires opposite flag selection)"
showFR _ ManualFlag                       = " (manual flag can only be changed explicitly)"
showFR c Backjump                         = " (backjumping, conflict set: " ++ showConflictSet c ++ ")"
showFR _ MultipleInstances                = " (multiple instances)"
showFR c (DependenciesNotLinked msg)      = " (dependencies not linked: " ++ msg ++ "; conflict set: " ++ showConflictSet c ++ ")"
showFR c CyclicDependencies               = " (cyclic dependencies; conflict set: " ++ showConflictSet c ++ ")"
-- The following are internal failures. They should not occur. In the
-- interest of not crashing unnecessarily, we still just print an error
-- message though.
showFR _ (MalformedFlagChoice qfn)        = " (INTERNAL ERROR: MALFORMED FLAG CHOICE: " ++ showQFN qfn ++ ")"
showFR _ (MalformedStanzaChoice qsn)      = " (INTERNAL ERROR: MALFORMED STANZA CHOICE: " ++ showQSN qsn ++ ")"
showFR _ EmptyGoalChoice                  = " (INTERNAL ERROR: EMPTY GOAL CHOICE)"

constraintSource :: ConstraintSource -> String
constraintSource src = "constraint from " ++ showConstraintSource src
