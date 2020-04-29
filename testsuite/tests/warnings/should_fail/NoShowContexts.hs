module NoShowContexts where

g :: [Int]
g = take 3

-- Taken from tcfail207. The usual error message is:
--
-- testsuite/tests/warnings/should_fail/NoShowContexts.hs:4:5: error:
--     • Couldn't match expected type ‘[Int]’
--                   with actual type ‘[a0] -> [a0]’
--     • Probable cause: ‘take’ is applied to too few arguments
--       In the expression: take 3
--       In an equation for ‘g’: g = take 3
--
-- Given -fno-diagnostics-show-contexts, The last two lines beginning with 'In'
-- should be removed, but the line before that, starting with 'Probable cause'
-- shouldn't since it is marked as a landmark context.
