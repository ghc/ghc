module OpaqueNoRebox3 where

f :: Int -> Int
f x = x `seq` (x + 1)
{-# OPAQUE f #-}

-- Historical note:
--
-- Since no W/W happens for f because it is OPAQUE, currently, the worker for g
-- does the dreaded reboxing of p similar to what is mentioned in
-- https://gitlab.haskell.org/ghc/ghc/-/issues/13143
--
-- 16-Nov-2021, Sebastian Graf says:
-- "Right, this is again not related to correct handling of OPAQUE but rather a
-- weakness in boxity analysis at the moment. this is because when boxity
-- analysis sees a  `Case`, it will look at its  `Alt`s. If one of the `Alt`
-- says `Unboxed`, we let the `Unboxed` win. We'd only say Boxed if all the Alts
-- had Boxed occs or if the scrutinee (or any of the occurrences that happen as
-- part of the same trace, guaranteed) had a Boxed occ. It's kind of a necessary
-- work-around until we have boxity analysis integrate with CPR analysis."
--
-- See Note [The OPAQUE pragma and avoiding the reboxing of arguments]
--
-- 16-Mar-2022:
-- With https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7609 merged, we no
-- longer get a reboxing worker for g
g :: Bool -> Bool -> Bool -> Int -> Int
g = \w w1 w2 p ->
  let fail_ = case w1 of
              False -> case w2 of
                 False -> g w True w2 p
                 True  -> f (f p)
              True -> error "patError"
  in case w of
        False -> case w1 of
          False -> fail_
          True -> case w2 of
            False -> p + 1
            True -> fail_
        True -> case w1 of
          False -> fail_
          True -> case w2 of
            _ -> f p
