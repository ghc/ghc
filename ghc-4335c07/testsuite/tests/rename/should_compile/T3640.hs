{-# LANGUAGE NamedFieldPuns #-}

module T3640 where

data Record = Record { f1, f2, f3 :: Int }

goodPun Record{f1,f2,f3} = f1 + f2 + f3

badPun r = f1 + f2 + f3
    where Record{f1=f1,f2,f3} = r

