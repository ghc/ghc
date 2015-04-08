{-# LANGUAGE FlexibleInstances #-}

module Expression.ArgList (
    ArgList (..),
    ArgsTeller,
    fromPlain,
    tellArgs
    ) where

import Data.Monoid

data ArgList = Plain [String]
             | PackageKey String
             | PackageDeps String
             | PackageDepKeys String

type ArgsTeller = ArgList -> Maybe [String]

-- Monoid instance for args-tellers (asks them one by one)
instance Monoid ArgsTeller where
    mempty        = const Nothing
    p `mappend` q = \a -> getFirst $ First (p a) <> First (q a)

fromPlain :: ArgsTeller
fromPlain (Plain list) = Just list
fromPlain _            = Nothing

tellArgs :: ArgsTeller -> ArgList -> ArgList
tellArgs t a = case t a of
    Just list -> Plain list
    Nothing   -> a

