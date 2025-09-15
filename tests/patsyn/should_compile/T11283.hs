{-# LANGUAGE PatternSynonyms, RecordWildCards #-}
module T11283 where
data P = MkP Bool
pattern S{x} = MkP x
d = S{x = True}
e = S{..}
f S{x=x} = x
