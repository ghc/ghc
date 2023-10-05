module A where

import O

x _ = 1 -- Generate a silly warning, so we know A was really compiled
x _ = 2 -- (and thus the reason B fails is that the bug is fixed)
