{-# LANGUAGE FlexibleInstances #-}

module Expression.Resolve (
    Resolve (..)
    ) where

import Base hiding (Args)
import Package
import Ways
import Util
import Oracles.Base
import Expression.PG
import Expression.Base

-- Resolve unevaluated variables by calling the associated oracles
class Resolve a where
    resolve :: a -> Action a
    resolve = return . id

-- Nothing to resolve for expressions containing FilePaths, Packages or Ways
instance Resolve FilePath where
instance Resolve Package where
instance Resolve Way where

--data Args
--    = Plain String           -- a plain old string argument: e.g., "-O2"
--    | BuildPath              -- evaluates to build path: "libraries/base"
--    | BuildDir               -- evaluates to build directory: "dist-install"
--    | Input                  -- evaluates to input file(s): "src.c"
--    | Output                 -- evaluates to output file(s): "src.o"
--    | Config String          -- evaluates to the value of a given config key
--    | ConfigList String
--    | BuilderPath Builder    -- evaluates to the path to a given builder
--    | PackageData String     -- looks up value a given key in package-data.mk
--    | PackageDataList String
--    | BootPkgConstraints     -- evaluates to boot package constraints
--    | Fold Combine Settings  -- fold settings using a given combine method

instance Resolve Args where
    resolve (Config key) = do
        value <- askConfig key
        return $ Plain value

    resolve (ConfigList key) = do
        values <- words <$> askConfig key
        return $ Fold Id $ argsOrdered values

    resolve (BuilderPath builder) = do
        path <- showArg builder
        return $ Plain $ unifyPath path

    --resolve (PackageData key) = ...
    --resolve (PackageDataList key) = ...
    --resolve (BootPkgConstraints) = ...

    resolve (Fold op settings) = do
        settings' <- resolve settings
        return $ Fold op settings'

    resolve a = return a


instance Resolve BuildPredicate where
    resolve p @ (Evaluated _) = return p

    resolve (Unevaluated (ConfigVariable key value)) = do
        lookup <- askConfig key
        return $ Evaluated $ lookup == value

    resolve p @ (Unevaluated _) = return p

    resolve (Not p) = do
        p' <- resolve p
        return $ Not p'

    resolve (And p q) = do
        p' <- resolve p
        q' <- resolve q
        return $ And p' q'

    resolve (Or p q) = do
        p' <- resolve p
        q' <- resolve q
        return $ Or p' q'

instance Resolve v => Resolve (BuildExpression v) where
    resolve Epsilon = return Epsilon

    resolve (Vertex v) = do
        v' <- resolve v
        return $ Vertex v'

    resolve (Overlay l r) = do
            l' <- resolve l
            r' <- resolve r
            return $ Overlay l' r'

    resolve (Sequence l r) = do
            l' <- resolve l
            r' <- resolve r
            return $ Sequence l' r'

    resolve (Condition l r) = do
            l' <- resolve l
            r' <- resolve r
            return $ Condition l' r'
