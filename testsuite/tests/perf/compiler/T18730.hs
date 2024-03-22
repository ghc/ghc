{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -funfolding-case-scaling=5 #-}

module T18730 where

import T18730_A (Gen)

genFields :: Gen [(String, Int)]
genFields =
  mapM
    (\(f, g) -> (f,) <$> g)
    [ ("field", genIntField)
    , ("field_10", genIntField)
    , ("field_10", genIntField)
    , ("field_10", genIntField)
    , ("field_10", genIntField)
    , ("field_10", genIntField)
    , ("field_10", genIntField)
    , ("field_10", genIntField)
    , ("field_10", genIntField)
    , ("field_10", genIntField)
    , ("field_10", genIntField)
    ]

genIntField :: Gen Int
genIntField = pure 0
