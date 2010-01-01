-- GHC 6.6 compiled YHC wrong; this is a cutdown testcase (from trac #1171).

module Main where

import Directory

data Error
    = ErrorFileNone
    | ErrorFileMany 
        FilePath    -- file you were looking for

raiseError ErrorFileNone = error "Error: File not found"
raiseError (ErrorFileMany file) = error $ "Error: Found file multiple times: "++file

data PackageData = PackageData [FilePath] deriving Show

getModule :: PackageData -> String -> IO ()
getModule (PackageData rs@(root:rest)) file =
        do local <- testPackage root
           res <- testPackage root
           print (local, res)
           case (local,res) of
                ([x], _) -> return ()
                (_, [x]) -> return ()
                ([], []) -> raiseError $ ErrorFileNone
                (as, bs) -> if as++bs == [] then error "Empty as++bs" else raiseError $ ErrorFileMany file
    where
        testPackage pkg =
            do
                bHi <- doesFileExist ""
                return [("","") | bHi]

main = getModule (PackageData ["7"]) "13"

