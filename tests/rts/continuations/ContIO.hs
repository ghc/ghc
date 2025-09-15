{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | This module just wraps the continuation primops so they can be used in
-- 'IO'. This isn't provided anywhere in @base@ because it's still very unsafe!
module ContIO where

import GHC.Prim
import GHC.Types

data PromptTag a = PromptTag (PromptTag# a)

newPromptTag :: IO (PromptTag a)
newPromptTag = IO (\s -> case newPromptTag# s of
  (# s', tag #) -> (# s, PromptTag tag #))

prompt :: PromptTag a -> IO a -> IO a
prompt (PromptTag tag) (IO m) = IO (prompt# tag m)

control0 :: PromptTag a -> ((IO b -> IO a) -> IO a) -> IO b
control0 (PromptTag tag) f =
  IO (control0# tag (\k -> case f (\(IO a) -> IO (k a)) of IO b -> b))

reset :: PromptTag a -> IO a -> IO a
reset = prompt

shift :: PromptTag a -> ((IO b -> IO a) -> IO a) -> IO b
shift tag f = control0 tag (\k -> reset tag (f (\m -> reset tag (k m))))
