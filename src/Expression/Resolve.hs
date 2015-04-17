{-# LANGUAGE FlexibleInstances #-}

module Expression.Resolve (
    ResolveConfig (..)
    ) where

import Base
import Oracles.Base
import Expression.PG
import Expression.Predicate
import Expression.Base
import Expression.Build

-- Resolve configuration variables
class ResolveConfig a where
    resolveConfig :: a -> Action a
    -- resolveConfig = return . id

instance ResolveConfig BuildPredicate where
    resolveConfig p @ (Evaluated _) = return p

    resolveConfig (Unevaluated (ConfigVariable key value)) = do
        lookup <- askConfig key
        return $ Evaluated $ lookup == value

    resolveConfig p @ (Unevaluated _) = return p

    resolveConfig (Not p) = do
        p' <- resolveConfig p
        return $ Not p'

    resolveConfig (And p q) = do
        p' <- resolveConfig p
        q' <- resolveConfig q
        return $ And p' q'

    resolveConfig (Or p q) = do
        p' <- resolveConfig p
        q' <- resolveConfig q
        return $ Or p' q'

instance ResolveConfig (BuildExpression v) where
    resolveConfig Epsilon = return Epsilon

    resolveConfig v @ (Vertex _) = return v -- TODO: go deeper

    resolveConfig (Overlay l r) = do
            l' <- resolveConfig l
            r' <- resolveConfig r
            return $ Overlay l' r'

    resolveConfig (Sequence l r) = do
            l' <- resolveConfig l
            r' <- resolveConfig r
            return $ Sequence l' r'

    resolveConfig (Condition l r) = do
            l' <- resolveConfig l
            r' <- resolveConfig r
            return $ Condition l' r'
