module Main (main) where
import qualified Util as T
import qualified CanonicalizePath
import qualified CopyFile001
import qualified CopyFile002
import qualified CopyFileWithMetadata
import qualified CreateDirectory001
import qualified CreateDirectoryIfMissing001
import qualified CurrentDirectory001
import qualified Directory001
import qualified DoesDirectoryExist001
import qualified DoesPathExist
import qualified FileTime
import qualified FindFile001
import qualified GetDirContents001
import qualified GetDirContents002
import qualified GetFileSize
import qualified GetHomeDirectory001
import qualified GetPermissions001
import qualified LongPaths
import qualified MakeAbsolute
import qualified PathIsSymbolicLink
import qualified RemoveDirectoryRecursive001
import qualified RemovePathForcibly
import qualified RenameDirectory
import qualified RenameFile001
import qualified RenamePath
import qualified Safe
import qualified T8482
import qualified WithCurrentDirectory

main :: IO ()
main = T.testMain $ \ _t -> do
  T.isolatedRun _t "CanonicalizePath" CanonicalizePath.main
  T.isolatedRun _t "CopyFile001" CopyFile001.main
  T.isolatedRun _t "CopyFile002" CopyFile002.main
  T.isolatedRun _t "CopyFileWithMetadata" CopyFileWithMetadata.main
  T.isolatedRun _t "CreateDirectory001" CreateDirectory001.main
  T.isolatedRun _t "CreateDirectoryIfMissing001" CreateDirectoryIfMissing001.main
  T.isolatedRun _t "CurrentDirectory001" CurrentDirectory001.main
  T.isolatedRun _t "Directory001" Directory001.main
  T.isolatedRun _t "DoesDirectoryExist001" DoesDirectoryExist001.main
  T.isolatedRun _t "DoesPathExist" DoesPathExist.main
  T.isolatedRun _t "FileTime" FileTime.main
  T.isolatedRun _t "FindFile001" FindFile001.main
  T.isolatedRun _t "GetDirContents001" GetDirContents001.main
  T.isolatedRun _t "GetDirContents002" GetDirContents002.main
  T.isolatedRun _t "GetFileSize" GetFileSize.main
  T.isolatedRun _t "GetHomeDirectory001" GetHomeDirectory001.main
  T.isolatedRun _t "GetPermissions001" GetPermissions001.main
  T.isolatedRun _t "LongPaths" LongPaths.main
  T.isolatedRun _t "MakeAbsolute" MakeAbsolute.main
  T.isolatedRun _t "PathIsSymbolicLink" PathIsSymbolicLink.main
  T.isolatedRun _t "RemoveDirectoryRecursive001" RemoveDirectoryRecursive001.main
  T.isolatedRun _t "RemovePathForcibly" RemovePathForcibly.main
  T.isolatedRun _t "RenameDirectory" RenameDirectory.main
  T.isolatedRun _t "RenameFile001" RenameFile001.main
  T.isolatedRun _t "RenamePath" RenamePath.main
  T.isolatedRun _t "Safe" Safe.main
  T.isolatedRun _t "T8482" T8482.main
  T.isolatedRun _t "WithCurrentDirectory" WithCurrentDirectory.main
