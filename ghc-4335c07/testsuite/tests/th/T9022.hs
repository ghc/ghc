module Main where

import Language.Haskell.TH

main = putStrLn $ pprint foo

foo :: Dec
foo = barD
  where
       barD = FunD ( mkName "bar" )
                   [ Clause manyArgs (NormalB barBody) [] ]

       barBody = DoE [letxStmt, retxStmt]
       letxStmt = LetS [ ValD (VarP xName) (NormalB $ LitE $ IntegerL 5) [] ]
       retxStmt = NoBindS $ AppE returnVarE xVarE
       xName = mkName "x"
       returnVarE = VarE $ mkName "return"
       xVarE = VarE xName
       manyArgs = map argP [0..9]
       argP n = VarP $ mkName $ "arg" ++ show n
