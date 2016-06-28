{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Oracles.DirectoryContent (
    getDirectoryContent, directoryContentOracle, Exclude(..), ExcludeNot(..)
    ) where

import Base
import System.Directory.Extra

newtype DirectoryContent = DirectoryContent (Exclude, ExcludeNot, FilePath)
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
newtype Exclude = Exclude [FilePattern]
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)
newtype ExcludeNot = ExcludeNot [FilePattern]
    deriving (Binary, Eq, Hashable, NFData, Show, Typeable)

-- | Get the directory content. 'Exclude' and 'ExcludeNot' are a list of file
-- patterns matched with '?=='.
getDirectoryContent :: Exclude -> ExcludeNot -> FilePath -> Action [FilePath]
getDirectoryContent exclude excludeNot dir =
    askOracle $ DirectoryContent (exclude, excludeNot, dir)

directoryContentOracle :: Rules ()
directoryContentOracle = void $ addOracle oracle
  where
    oracle :: DirectoryContent -> Action [FilePath]
    oracle (DirectoryContent (Exclude exclude, ExcludeNot excludeNot, dir)) =
        liftIO $ filter test <$> listFilesInside (return . test) dir
      where
        test a = include' a || not (exclude' a)
        exclude' a = any (?== a) exclude
        include' a = any (?== a) excludeNot
