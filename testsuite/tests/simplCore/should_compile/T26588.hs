module T26588 ( getOptionSettingFromText ) where

import           Control.Applicative ( Const(..) )
import           Data.Map (Map)
import qualified Data.Map.Strict as Map

------------------------------------------------------------------------
-- ConfigState

data ConfigLeaf
data ConfigTrie = ConfigTrie !(Maybe ConfigLeaf) !ConfigMap

type ConfigMap = Map Int ConfigTrie

freshLeaf :: [Int] -> ConfigLeaf -> ConfigTrie
freshLeaf [] l     = ConfigTrie (Just l) mempty
freshLeaf (a:as) l = ConfigTrie Nothing (Map.singleton a (freshLeaf as l))

adjustConfigTrie :: Functor t => [Int] -> (Maybe ConfigLeaf -> t (Maybe ConfigLeaf)) -> Maybe (ConfigTrie) -> t (Maybe ConfigTrie)
adjustConfigTrie     as f Nothing                 = fmap (freshLeaf as) <$> f Nothing
adjustConfigTrie (a:as) f (Just (ConfigTrie x m)) = Just . ConfigTrie x <$> adjustConfigMap a as f m
adjustConfigTrie     [] f (Just (ConfigTrie x m)) = g <$> f x
  where g Nothing | Map.null m = Nothing
        g x' = Just (ConfigTrie x' m)

adjustConfigMap :: Functor t => Int -> [Int] -> (Maybe ConfigLeaf -> t (Maybe ConfigLeaf)) -> ConfigMap -> t ConfigMap
adjustConfigMap a as f = Map.alterF (adjustConfigTrie as f) a

getOptionSettingFromText :: Int -> [Int] -> ConfigMap -> IO ()
getOptionSettingFromText p ps = getConst . adjustConfigMap p ps f
  where
    f _ = Const (return ())
