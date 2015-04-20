{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, FlexibleInstances #-}

module Expression.Simplify (
    Simplify (..), linearise, fromSettings,
    ) where

import Base hiding (Args)
import Ways
import Package
import Expression.PG
import Expression.Args
import Expression.Build

-- Simplify expressions by constant propagation
class Simplify a where
    simplify :: a -> a
    simplify = id

-- Linearise a build expression into a list. Returns Nothing if the given
-- expression cannot be uniquely evaluated due to remaining variables.
-- Overlay subexpressions are linearised in arbitrary order.
linearise :: (Predicate p, Simplify (PG p v)) => PG p v -> Maybe [v]
linearise = go . simplify
  where
    go Epsilon         = Just []
    go (Vertex v)      = Just [v]
    go (Overlay   p q) = (++) <$> go p <*> go q -- TODO: union
    go (Sequence  p q) = (++) <$> go p <*> go q
    go (Condition _ _) = Nothing

fromArgs :: Args -> BuildExpression (Maybe [String])
fromArgs (Plain s) = return $ Just [s]
fromArgs (Fold Id          s) = return    $                     fromSettings s
fromArgs (Fold Concat      s) = singleton $ concat          <$> fromSettings s
fromArgs (Fold ConcatPath  s) = singleton $ concatPath      <$> fromSettings s
fromArgs (Fold ConcatSpace s) = singleton $ intercalate " " <$> fromSettings s
fromArgs _ = return Nothing

singleton :: Maybe String -> BuildExpression (Maybe [String])
singleton (Just s) = return $ Just [s]
singleton Nothing  = return Nothing

concatPath :: [FilePath] -> FilePath
concatPath [] = ""
concatPath [f] = f
concatPath (f : fs) = f </> concatPath fs

fromSettings :: Settings -> Maybe [String]
fromSettings settings = case linearise (settings >>= fromArgs) of
    Just list -> concatMaybes list
    Nothing   -> Nothing
  where
    concatMaybes :: [Maybe [a]] -> Maybe [a]
    concatMaybes [] = Just []
    concatMaybes (Just a : as) = case concatMaybes as of
        Just rest -> Just $ a ++ rest
        Nothing   -> Nothing
    concatMaybes (Nothing : _) = Nothing

instance Simplify BuildPredicate where
    simplify p @ (Evaluated _) = p
    simplify p @ (Unevaluated _) = p
    simplify (Not p) = case p' of
        Evaluated bool -> Evaluated (not bool)
        _              -> Not p'
      where p' = simplify p
    simplify (And p q)
        | p' == false = false
        | q' == false = false
        | p' == true  = q'
        | q' == true  = p'
        | otherwise   = And p' q'
      where
        p' = simplify p
        q' = simplify q
    simplify (Or p q)
        | p' == true  = true
        | q' == true  = true
        | p' == false = q'
        | q' == false = p'
        | otherwise   = Or p' q'
      where
        p' = simplify p
        q' = simplify q

-- Nothing to simplify here
instance Simplify Way where
instance Simplify Package where
instance Simplify TargetDir where
instance Simplify (Maybe [String]) where

-- Only Fold can be simplified in Args
instance Simplify Args where
    simplify (Fold combine settings) = Fold combine (simplify settings)
    simplify a = a

instance (Simplify p, Simplify v, Predicate p, Eq p, Eq v) => Simplify (PG p v)
  where
    simplify Epsilon = Epsilon
    simplify (Vertex v) = Vertex $ simplify v
    simplify (Overlay l r)
        | l' == Epsilon = r'
        | r' == Epsilon = l'
        | l' == r'      = l'
        | otherwise     = Overlay l' r'
      where
        l' = simplify l
        r' = simplify r
    simplify (Sequence l r)
        | l' == Epsilon = r'
        | r' == Epsilon = l'
        | otherwise     = Sequence l' r'
      where
        l' = simplify l
        r' = simplify r
    simplify (Condition l r)
        | l' == true    = r'
        | l' == false   = Epsilon
        | r' == Epsilon = Epsilon
        | otherwise     = Condition l' r'
      where
        l' = simplify l
        r' = simplify r
