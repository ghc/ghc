
-- Showed up a bug in bodySplitTyConApp

module ShouldFail where

p :: f b
p = error "urk"

g x = case p of
        True -> ()
