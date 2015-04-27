{-# LANGUAGE NoImplicitPrelude, FlexibleInstances #-}

module Expression.Resolve (
    Resolve (..), evaluate
    ) where

import Base hiding (Args, arg)
import Package
import Ways
import Util
import Control.Monad
import Expression.Simplify
import Oracles.Base
import Oracles.PackageData
import Expression.PG
import Expression.Derived
import Expression.Settings
import Expression.BuildExpression
import qualified Expression.BuildPredicate as BP
import           Expression.BuildPredicate hiding (rewrite)

-- Resolve unevaluated variables by calling the associated oracles
class Resolve a where
    resolve :: a -> Action a
    resolve = return . id

evaluate :: (Simplify a, Resolve a) => a -> Action a
evaluate = resolve . simplify

-- Nothing to resolve for expressions containing FilePaths, Packages or Ways
instance Resolve TargetDir where
instance Resolve Package where
instance Resolve Way where

instance Resolve Args where
    resolve (EnvironmentParameter (Config Single key)) = do
        value <- askConfig key
        return $ Plain value

    resolve (EnvironmentParameter (Config Multiple key)) = do
        values <- words <$> askConfig key
        return $ Fold Id $ argsOrdered values -- TODO: dedup 'Fold Id'

    resolve (EnvironmentParameter (BuilderPath builder)) = do
        value <- showArg builder
        return $ Plain value

    resolve a @ (EnvironmentParameter (PackageData _ _ Nothing _)) = return a
    resolve a @ (EnvironmentParameter (PackageData _ _ _ Nothing)) = return a

    resolve (EnvironmentParameter
            (PackageData Single key (Just path) (Just dir))) = do
        value <- askPackageData (path </> dir) key
        return $ Plain value

    resolve (EnvironmentParameter
            (PackageData Multiple key (Just path) (Just dir))) = do
        values <- words <$> askPackageData (path </> dir) key
        return $ Fold Id $ argsOrdered values

    resolve (EnvironmentParameter (PackageConstraints pkgs)) = do
        pkgs' <- evaluate $ pkgs
        constraints <- case linearise pkgs' of
            Nothing      -> redError "Cannot determine boot packages."
            Just pkgList -> forM pkgList $ \pkg -> do
                let cabal  = pkgPath pkg </> pkgCabal pkg
                    prefix = dropExtension (pkgCabal pkg) ++ " == "
                need [cabal]
                content <- lines <$> liftIO (readFile cabal)
                let vs = filter (("ersion:" `isPrefixOf`) . drop 1) content
                case vs of
                    [v] -> return $ arg (prefix ++ dropWhile (not . isDigit) v)
                    _   -> redError $ "Cannot determine package version in '"
                                    ++ cabal ++ "'."
        return $ Fold Id (argPairs "--constraint" $ msum constraints)

    resolve (Fold op settings) = do
        settings' <- resolve settings
        return $ Fold op settings'

    resolve a = return a

instance Resolve BuildPredicate where
    resolve = BP.rewrite (return . fromBool) fv (fmap not . resolve) fa fo
      where
        fv (ConfigVariable key value) = do
            lookup <- askConfig key
            return $ fromBool (lookup == value)
        fv v = return $ variable v

        fa p q = (&&) <$> resolve p <*> resolve q
        fo p q = (||) <$> resolve p <*> resolve q

-- TODO: implement with a bimap
instance Resolve v => Resolve (BuildExpression v) where
    resolve = rewrite (return empty) (fmap return . resolve) fo fs fc
      where
        fo l r = (<|>) <$> resolve l <*> resolve r
        fs l r = ( |>) <$> resolve l <*> resolve r
        fc l r = ( ? ) <$> resolve l <*> resolve r
