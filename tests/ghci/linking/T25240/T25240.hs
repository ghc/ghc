{-# LANGUAGE TemplateHaskell #-}

{-

When the RTS linker loads the T25240a module to run the pure foo splice, it
tries to resolve the func symbol even if this function isn't required to run the
splice code, i.e., its dead code. This test checks that by passing the
--optimistic-linking flag the RTS linker continues to link even in the presence
of unknown symbols.

-}

module T25240 where

import T25240a

$(pure foo)
