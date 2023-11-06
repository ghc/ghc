{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | This module just wraps the continuation primops so they can be used in
-- 'ST'. This isn't provided anywhere in @base@ because it's still very unsafe!
module ContST where

import GHC.Prim
import GHC.Types
import GHC.ST

data PromptTag s a = PromptTag (PromptTag# s a)

newPromptTag :: ST s (PromptTag s a)
newPromptTag = ST (\s -> case newPromptTag# s of
  (# s', tag #) -> (# s, PromptTag tag #))

prompt :: PromptTag s a -> ST s a -> ST s a
prompt (PromptTag tag) (ST m) = ST (prompt# tag m)

control0 :: PromptTag s a -> ((ST s b -> ST s a) -> ST s a) -> ST s b
control0 (PromptTag tag) f =
  ST (control0# tag (\k -> case f (\(ST a) -> ST (k a)) of ST b -> b))

reset :: PromptTag s a -> ST s a -> ST s a
reset = prompt

shift :: PromptTag s a -> ((ST s b -> ST s a) -> ST s a) -> ST s b
shift tag f = control0 tag (\k -> reset tag (f (\m -> reset tag (k m))))
