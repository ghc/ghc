module ShouldSucceed where

-- From a bug report by Sven Panne.

foo = bar
   where bar = \_ -> (truncate boing, truncate boing)
         boing = 0
