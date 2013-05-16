{-# LANGUAGE TemplateHaskell #-}
module T5358 where

import Language.Haskell.TH

t1, t2 :: Int
t1 x = x
t2 x = x

prop_x1 x = t1 x == t2 x

$(return [])

runTests = $( do VarI _ t _ _ <- reify (mkName "prop_x1")
                 error $ ("runTest called error: " ++ pprint t)
            )
