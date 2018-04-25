module Distribution.Solver.Modular.Log
    ( Log
    , logToProgress
    ) where

import Prelude ()
import Distribution.Solver.Compat.Prelude

import Data.List as L

import Distribution.Solver.Types.Progress

import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Message
import Distribution.Solver.Modular.Tree (FailReason(..))
import qualified Distribution.Solver.Modular.ConflictSet as CS
import Distribution.Verbosity

-- | The 'Log' datatype.
--
-- Represents the progress of a computation lazily.
--
-- Parameterized over the type of actual messages and the final result.
type Log m a = Progress m (ConflictSet, ConflictMap) a

messages :: Progress step fail done -> [step]
messages = foldProgress (:) (const []) (const [])

data Exhaustiveness = Exhaustive | BackjumpLimitReached

-- | Postprocesses a log file. Takes as an argument a limit on allowed backjumps.
-- If the limit is 'Nothing', then infinitely many backjumps are allowed. If the
-- limit is 'Just 0', backtracking is completely disabled.
logToProgress :: Verbosity -> Maybe Int -> Log Message a -> Progress String String a
logToProgress verbosity mbj l =
    let es = proc (Just 0) l -- catch first error (always)
        ms = proc mbj l
    in go es es -- trace for first error
          (showMessages (const True) True ms) -- run with backjump limit applied
  where
    -- Proc takes the allowed number of backjumps and a 'Progress' and explores the
    -- messages until the maximum number of backjumps has been reached. It filters out
    -- and ignores repeated backjumps. If proc reaches the backjump limit, it truncates
    -- the 'Progress' and ends it with the last conflict set. Otherwise, it leaves the
    -- original result.
    proc :: Maybe Int -> Log Message b -> Progress Message (Exhaustiveness, ConflictSet, ConflictMap) b
    proc _        (Done x)                          = Done x
    proc _        (Fail (cs, cm))                   = Fail (Exhaustive, cs, cm)
    proc mbj'     (Step x@(Failure cs Backjump) xs@(Step Leave (Step (Failure cs' Backjump) _)))
      | cs == cs'                                   = Step x (proc mbj'           xs) -- repeated backjumps count as one
    proc (Just 0) (Step   (Failure cs Backjump)  _) = Fail (BackjumpLimitReached, cs, mempty) -- No final conflict map available
    proc (Just n) (Step x@(Failure _  Backjump) xs) = Step x (proc (Just (n - 1)) xs)
    proc mbj'     (Step x                       xs) = Step x (proc mbj'           xs)

    -- The first two arguments are both supposed to be the log up to the first error.
    -- That's the error that will always be printed in case we do not find a solution.
    -- We pass this log twice, because we evaluate it in parallel with the full log,
    -- but we also want to retain the reference to its beginning for when we print it.
    -- This trick prevents a space leak!
    --
    -- The third argument is the full log, ending with either the solution or the
    -- exhaustiveness and final conflict set.
    go :: Progress Message (Exhaustiveness, ConflictSet, ConflictMap) b
       -> Progress Message (Exhaustiveness, ConflictSet, ConflictMap) b
       -> Progress String  (Exhaustiveness, ConflictSet, ConflictMap) b
       -> Progress String String b
    go ms (Step _ ns)        (Step x xs)           = Step x (go ms ns xs)
    go ms r                  (Step x xs)           = Step x (go ms r  xs)
    go ms (Step _ ns)        r                     = go ms ns r
    go ms (Fail (_, cs', _)) (Fail (exh, cs, cm))  = Fail $
        "Could not resolve dependencies:\n" ++
        unlines (messages $ showMessages (L.foldr (\ v _ -> v `CS.member` cs') True) False ms) ++
        case exh of
            Exhaustive ->
                "After searching the rest of the dependency tree exhaustively, "
                ++ "these were the goals I've had most trouble fulfilling: "
                ++ showCS cm cs
              where
                showCS = if verbosity > normal
                         then CS.showCSWithFrequency
                         else CS.showCSSortedByFrequency
            BackjumpLimitReached ->
                "Backjump limit reached (" ++ currlimit mbj ++
                "change with --max-backjumps or try to run with --reorder-goals).\n"
              where currlimit (Just n) = "currently " ++ show n ++ ", "
                    currlimit Nothing  = ""
    go _  _                  (Done s)              = Done s
    go _  (Done _)           (Fail _)              = Fail $
        -- Should not happen: Second argument is the log up to first error,
        -- third one is the entire log. Therefore it should never happen that
        -- the second log finishes with 'Done' and the third log with 'Fail'.
        "Could not resolve dependencies; something strange happened."
