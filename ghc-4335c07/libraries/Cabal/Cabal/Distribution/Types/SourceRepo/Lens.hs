module Distribution.Types.SourceRepo.Lens (
    T.SourceRepo,
    module Distribution.Types.SourceRepo.Lens,
    ) where

import Prelude()
import Distribution.Compat.Prelude
import Distribution.Compat.Lens

import Distribution.Types.SourceRepo (SourceRepo, RepoKind, RepoType)
import qualified Distribution.Types.SourceRepo as T

repoKind :: Lens' SourceRepo RepoKind
repoKind f s = fmap (\x -> s { T.repoKind = x }) (f (T.repoKind s))
{-# INLINE repoKind #-}

repoType :: Lens' SourceRepo (Maybe RepoType)
repoType f s = fmap (\x -> s { T.repoType = x }) (f (T.repoType s))
{-# INLINE repoType #-}

repoLocation :: Lens' SourceRepo (Maybe String)
repoLocation f s = fmap (\x -> s { T.repoLocation = x }) (f (T.repoLocation s))
{-# INLINE repoLocation #-}

repoModule :: Lens' SourceRepo (Maybe String)
repoModule f s = fmap (\x -> s { T.repoModule = x }) (f (T.repoModule s))
{-# INLINE repoModule #-}

repoBranch :: Lens' SourceRepo (Maybe String)
repoBranch f s = fmap (\x -> s { T.repoBranch = x }) (f (T.repoBranch s))
{-# INLINE repoBranch #-}

repoTag :: Lens' SourceRepo (Maybe String)
repoTag f s = fmap (\x -> s { T.repoTag = x }) (f (T.repoTag s))
{-# INLINE repoTag #-}

repoSubdir :: Lens' SourceRepo (Maybe FilePath)
repoSubdir f s = fmap (\x -> s { T.repoSubdir = x }) (f (T.repoSubdir s))
{-# INLINE repoSubdir #-}
