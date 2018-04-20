module T15056 where

import T15056a

blam x = foo 3 [1..x]
-- We expect fold/build to fire
