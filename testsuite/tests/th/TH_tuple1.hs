{-# LANGUAGE TemplateHaskell #-}

-- Test the use of tupleDataName, tupleTypeName

module ShouldCompile where

import Language.Haskell.TH

tp1 = $( sigE (appsE [conE (tupleDataName 2),
                     litE (integerL 1),
                     litE (integerL 2)])
              (appT (appT (conT (tupleTypeName 2))
                          (conT ''Integer))
                    (conT ''Integer))
        )

tp2 = $( sigE (appsE [conE (tupleDataName 1),
                     litE (integerL 1)])
              (appT (conT (tupleTypeName 1))
                    (conT ''Integer))
        )
