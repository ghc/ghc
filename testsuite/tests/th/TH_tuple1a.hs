{-# LANGUAGE TemplateHaskell #-}
-- Test the use of tupleDataName, tupleTypeName
module TH_tuple1a where

import Language.Haskell.TH

tp2, tp1, tp2u, tp1u :: Q Exp
tp2 = sigE (appsE [conE (tupleDataName 2),
                  litE (integerL 1),
                  litE (integerL 2)])
           (appT (appT (conT (tupleTypeName 2))
                       (conT ''Integer))
                       (conT ''Integer))

tp1 = sigE (appsE [conE (tupleDataName 1),
                  litE (integerL 1)])
           (appT (conT (tupleTypeName 1))
                 (conT ''Integer))

tp2u = sigE (appsE [conE (unboxedTupleDataName 2),
                   litE (integerL 1),
                   litE (integerL 2)])
            (appT (appT (conT (unboxedTupleTypeName 2))
                        (conT ''Integer))
                  (conT ''Integer))

tp1u = sigE (appsE [conE (unboxedTupleDataName 1),
                   litE (integerL 1)])
            (appT (conT (unboxedTupleTypeName 1))
                  (conT ''Integer))
