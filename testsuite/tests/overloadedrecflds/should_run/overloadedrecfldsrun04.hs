{-# LANGUAGE OverloadedRecordFields, TemplateHaskell #-}

import GHC.Records
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- Splice in a datatype with field...
$(return [DataD [] (mkName "R") [] [RecC (mkName "MkR") [(mkName "foo", NotStrict, ConT ''Int)]] []])

-- New TH story means reify only sees R if we do this:
$(return [])

-- ... and check that we can inspect it
main = do  putStrLn $(do { info <- reify ''R
                         ; lift (pprint info) })
           putStrLn $(do { insts <- reifyInstances ''Has [ConT ''R, LitT (StrTyLit "foo"), ConT ''Int]
                         ; lift (pprint insts) })
           print (foo (MkR { foo = 42 }))
