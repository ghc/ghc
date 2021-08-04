{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE CPP              #-}

-- | Internal utilities.
module Safe.Util(
    fromNoteModule, fromNoteEitherModule,
    liftMay,
    (.^), (.^^), (.^^^),
    eitherToMaybe,
    withFrozenCallStack
    ) where

import Data.Maybe
import Safe.Partial

-- Let things work through ghci alone
#if __GLASGOW_HASKELL__ >= 800
import GHC.Stack
#else
withFrozenCallStack :: a -> a
withFrozenCallStack = id
#endif


(.^) :: Partial => (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.^) f g x1 x2 = f (g x1 x2)

(.^^) :: Partial => (b -> c) -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> c
(.^^) f g x1 x2 x3 = f (g x1 x2 x3)

(.^^^) :: Partial => (b -> c) -> (a1 -> a2 -> a3 -> a4 -> b) -> a1 -> a2 -> a3 -> a4 -> c
(.^^^) f g x1 x2 x3 x4 = f (g x1 x2 x3 x4)

liftMay :: (a -> Bool) -> (a -> b) -> (a -> Maybe b)
liftMay test func val = if test val then Nothing else Just $ func val

fromNoteModule :: Partial => String -> String -> String -> Maybe a -> a
fromNoteModule modu note func = fromMaybe (error msg)
    where msg = modu ++ "." ++ func ++ (if null note then "" else ", " ++ note)

fromNoteEitherModule :: Partial => String -> String -> String -> Either String a -> a
fromNoteEitherModule modu note func = either (error . msg) id
    where msg ex = modu ++ "." ++ func ++ " " ++ ex ++ (if null note then "" else ", " ++ note)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just
