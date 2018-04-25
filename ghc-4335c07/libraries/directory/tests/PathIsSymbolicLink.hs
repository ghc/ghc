{-# LANGUAGE CPP #-}
module PathIsSymbolicLink where
#include "util.inl"
import TestUtils

main :: TestEnv -> IO ()
main _t = do
  supportsSymbolicLinks <- supportsSymlinks
  when supportsSymbolicLinks $ do

    createFileLink "x" "y"
    createDirectoryLink "a" "b"

    T(expect) () =<< pathIsSymbolicLink "y"
    T(expect) () =<< pathIsSymbolicLink "b"
    T(expectEq) () "x" =<< getSymbolicLinkTarget "y"
    T(expectEq) () "a" =<< getSymbolicLinkTarget "b"
    T(expectEq) () False =<< doesFileExist "y"
    T(expectEq) () False =<< doesDirectoryExist "b"

    writeFile "x" ""
    createDirectory "a"

    T(expect) () =<< doesFileExist "y"
    T(expect) () =<< doesDirectoryExist "b"

    removeFile "y"
    removeDirectoryLink "b"

    T(expectIOErrorType) () isDoesNotExistError (pathIsSymbolicLink "y")
    T(expectIOErrorType) () isDoesNotExistError (pathIsSymbolicLink "b")
    T(expectEq) () False =<< doesFileExist "y"
    T(expectEq) () False =<< doesDirectoryExist "b"
