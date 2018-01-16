{-# LANGUAGE LambdaCase #-}
module T10498a where

-- ghc-7.10 would show the unhelpful error message:
--
-- T10498a.hs:10:5:
--     parse error in if statement: missing required else clause

foo =
    if True
    then
        \case ->
            1 -> 2
    else id
