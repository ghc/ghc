{-# LANGUAGE CPP #-}
#ifndef MIN_VERSION_template_haskell
#define MIN_VERSION_template_haskell(x,y,z) 1
#endif
-- template-haskell is only safe since GHC-8.2
#if __GLASGOW_HASKELL__ >= 802
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
module Data.Proxy.TH
  ( pr
#if MIN_VERSION_template_haskell(2,8,0)
  , pr1
#endif
  ) where

import Data.Char
#if __GLASGOW_HASKELL__ < 710
import Data.Functor
#endif
#if __GLASGOW_HASKELL__ < 707
import Data.Version (showVersion)
import Paths_tagged
#endif
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

proxy_d, proxy_tc :: Name
#if __GLASGOW_HASKELL__ >= 707
proxy_d  = mkNameG_d "base" "Data.Proxy" "Proxy"
proxy_tc = mkNameG_tc "base" "Data.Proxy" "Proxy"
#else
proxy_d  = mkNameG_d taggedPackageKey "Data.Proxy" "Proxy"
proxy_tc = mkNameG_tc taggedPackageKey "Data.Proxy" "Proxy"

-- note: On 7.10+ this would use CURRENT_PACKAGE_KEY if we still housed the key.
taggedPackageKey :: String
taggedPackageKey = "tagged-" ++ showVersion version
#endif

proxyTypeQ :: TypeQ -> TypeQ
proxyTypeQ t = appT (conT proxy_tc) t

proxyExpQ :: TypeQ -> ExpQ
proxyExpQ t = sigE (conE proxy_d) (proxyTypeQ t)

proxyPatQ :: TypeQ -> PatQ
proxyPatQ t = sigP (conP proxy_d []) (proxyTypeQ t)

-- | A proxy value quasiquoter. @[pr|T|]@ will splice an expression
-- @Proxy::Proxy T@, while @[pr|A,B,C|]@ will splice in a value of
-- @Proxy :: Proxy [A,B,C]@.

-- TODO: parse a richer syntax for the types involved here so we can include spaces, applications, etc.
pr :: QuasiQuoter
pr = QuasiQuoter (mkProxy proxyExpQ) (mkProxy proxyPatQ) (mkProxy proxyTypeQ) undefined where
  mkProxy :: (TypeQ -> r) -> String -> r
  mkProxy p s = case ts of
    [h@(t:_)]
       | isUpper t -> p $ head <$> cons
       | otherwise -> p $ varT $ mkName h
#if MIN_VERSION_template_haskell(2,8,0)
    _ -> p $ mkList <$> cons
#endif
    where 
      ts = map strip $ splitOn ',' s
      cons = mapM (conT . mkName) ts
#if MIN_VERSION_template_haskell(2,8,0)
      mkList = foldr (AppT . AppT PromotedConsT) PromotedNilT
#endif

#if MIN_VERSION_template_haskell(2,8,0)
-- | Like 'pr', but takes a single type, which is used to produce a
-- 'Proxy' for a single-element list containing only that type. This
-- is useful for passing a single type to a function that wants a list
-- of types.

-- TODO: parse a richer syntax for the types involved here so we can include spaces, applications, etc.
pr1 :: QuasiQuoter
pr1 = QuasiQuoter (mkProxy proxyExpQ) (mkProxy proxyPatQ) (mkProxy proxyTypeQ) undefined where
  sing x = AppT (AppT PromotedConsT x) PromotedNilT
  mkProxy p s = case s of
    t:_ 
      | isUpper t -> p (fmap sing (conT $ mkName s))
      | otherwise -> p (fmap sing (varT $ mkName s))
    _ -> error "Empty string passed to pr1"
#endif

-- | Split on a delimiter.
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn d = go where
  go [] = []
  go xs = case t of
      [] -> [h]
      (_:t') -> h : go t' 
    where (h,t) = break (== d) xs

-- | Remove white space from both ends of a 'String'.
strip :: String -> String
strip = takeWhile (not . isSpace) . dropWhile isSpace
