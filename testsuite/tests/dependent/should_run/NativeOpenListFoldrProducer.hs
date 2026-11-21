{-# LANGUAGE CPP #-}
module NativeOpenListFoldrProducer
    ( direct, local, wrongHead, wrongTail, wrongSeed, wrongCallback, wrongOwner
    , forceStep, forceSeed, forceResult, foldBits, foldChars, forceBitStep, forceBitSeed ) where

{-# NOINLINE [2] direct #-}
direct :: (a -> b -> b) -> b -> [a] -> b
direct _ z [] = z
direct k z (x : xs) = k x (direct k z xs)

{-# INLINE [0] local #-}
local :: (a -> b -> b) -> b -> [a] -> b
local k z = go
  where
    go [] = z
    go (x : xs) = k x (go xs)

foldBits :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool
foldBits _ seed [] = seed
foldBits step seed (x : xs) = step x (foldBits step seed xs)

{-# INLINE [0] foldChars #-}
foldChars :: (Char -> Maybe Bool -> Maybe Bool) -> Maybe Bool -> [Char] -> Maybe Bool
foldChars step seed = go
  where
    go [] = seed
    go (x : xs) = step x (go xs)

forceBitStep :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool
forceBitStep step seed xs = step `seq` case xs of
    [] -> seed
    x : rest -> step x (forceBitStep step seed rest)

forceBitSeed :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool
forceBitSeed step seed xs = seed `seq` case xs of
    [] -> seed
    x : rest -> step x (forceBitSeed step seed rest)

wrongHead :: (a -> b -> b) -> b -> [a] -> b
wrongHead _ z [] = z
wrongHead k z (_ : xs) = k (error "changed head") (wrongHead k z xs)

wrongTail :: (a -> b -> b) -> b -> [a] -> b
wrongTail _ z [] = z
wrongTail k z (x : _) = k x (wrongTail k z [])

wrongSeed :: (a -> b -> b) -> b -> [a] -> b
wrongSeed _ z [] = z
wrongSeed k z (x : xs) = k x (wrongSeed k (error "changed seed") xs)

wrongCallback :: (a -> b -> b) -> b -> [a] -> b
wrongCallback _ z [] = z
wrongCallback k z (x : xs) = k x (wrongCallback (\_ _ -> z) z xs)

wrongOwner :: (a -> b -> b) -> b -> [a] -> b
wrongOwner _ z [] = z
wrongOwner k z (x : xs) = k x (direct k z xs)

forceStep :: (a -> b -> b) -> b -> [a] -> b
forceStep k z xs = k `seq` case xs of
    [] -> z
    x : rest -> k x (forceStep k z rest)

forceSeed :: (a -> b -> b) -> b -> [a] -> b
forceSeed k z xs = z `seq` case xs of
    [] -> z
    x : rest -> k x (forceSeed k z rest)

forceResult :: (a -> b -> b) -> b -> [a] -> b
forceResult _ z [] = z
forceResult k z (x : xs) = let rest = forceResult k z xs in rest `seq` k x rest

#ifdef CHANGE_FOLD
{-# RULES "foldr/nil" [~1] forall k z xs. direct k z xs = z #-}
#endif
