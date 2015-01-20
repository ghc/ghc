{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 709
{-# LANGUAGE Safe #-}
#elif __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

module Directory (
        Permissions( Permissions, readable, writable, executable, searchable ),
        createDirectory, removeDirectory, removeFile,
        renameDirectory, renameFile, getDirectoryContents,
        getCurrentDirectory, setCurrentDirectory,
        doesFileExist, doesDirectoryExist,
        getPermissions, setPermissions,
        getModificationTime
    ) where

import System.Directory hiding (Permissions,
                                readable, writable, executable, searchable,
                                getPermissions, setPermissions)
import qualified System.Directory as SD

data Permissions = Permissions {
                       readable   :: Bool,
                       writable   :: Bool,
                       executable :: Bool,
                       searchable :: Bool
                   }
    deriving (Eq, Ord, Read, Show)

getPermissions :: FilePath -> IO Permissions
getPermissions fp = do perm <- SD.getPermissions fp
                       return $ Permissions {
                                    readable   = SD.readable perm,
                                    writable   = SD.writable perm,
                                    executable = SD.executable perm,
                                    searchable = SD.searchable perm
                                }

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions fp perm = let mkPerm = setOwnerReadable   (readable   perm)
                                    . setOwnerWritable   (writable  perm)
                                    . setOwnerExecutable (executable perm)
                                    . setOwnerSearchable (searchable perm)
                         in SD.setPermissions fp (mkPerm emptyPermissions)

