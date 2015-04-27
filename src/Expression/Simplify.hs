{-# LANGUAGE NoImplicitPrelude, FlexibleContexts, FlexibleInstances #-}

module Expression.Simplify (
    Simplify (..), linearise, fromSettings,
    ) where

import Base hiding (Args)
import Ways
import Package
import Expression.PG
import Expression.Settings
import qualified Expression.BuildPredicate as BP
import           Expression.BuildPredicate hiding (rewrite)
import Expression.BuildExpression

-- Simplify expressions by constant propagation
class Simplify a where
    simplify :: a -> a
    simplify = id

-- Linearise a build expression into a list. Returns Nothing if the given
-- expression cannot be uniquely evaluated due to remaining variables.
-- Overlay subexpressions are linearised in arbitrary order.
-- TODO: topological sort
linearise :: (Predicate p, Simplify (PG p v)) => PG p v -> Maybe [v]
linearise = go . simplify
  where
    go     = rewrite (Just []) fv fo fs fc
    fv v   = Just [v]
    fo l r = (++) <$> go l <*> go r -- TODO: merge
    fs l r = (++) <$> go l <*> go r
    fc _ _ = Nothing

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
    simplify = BP.rewrite fromBool variable simplifyN simplifyA simplifyO
      where
        simplifyN p
            | p' == true  = false
            | p' == false = true
            | otherwise   = not p'
          where p' = simplify p
        simplifyA p q
            | p' == false = false
            | q' == false = false
            | p' == true  = q'
            | q' == true  = p'
            | otherwise   = p' && q'
          where
            p' = simplify p
            q' = simplify q
        simplifyO p q
            | p' == true  = true
            | q' == true  = true
            | p' == false = q'
            | q' == false = p'
            | otherwise   = p' || q'
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
    simplify = rewrite empty (return . simplify) simplifyO simplifyS simplifyC
      where
        simplifyO l r
            | l' == empty = r'
            | r' == empty = l'
            | l' == r'    = l'
            | otherwise   = l' <|> r'
          where
            l' = simplify l
            r' = simplify r
        simplifyS l r
            | l' == empty = r'
            | r' == empty = l'
            | otherwise   = l' |> r'
          where
            l' = simplify l
            r' = simplify r
        simplifyC l r
            | l' == true  = r'
            | l' == false = empty
            | r' == empty = empty
            | otherwise   = l' ? r'
          where
            l' = simplify l
            r' = simplify r
