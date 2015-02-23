-- Test that OverloadedRecordFields works with TemplateHaskell

{-# LANGUAGE OverloadedRecordFields, TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- Splice in a datatype with field...
$(return [DataD [] (mkName "R") [] [RecC (mkName "MkR") [(mkName "foo", NotStrict, ConT ''Int)]] []])

-- New TH story means reify only sees R if we do this:
$(return [])

-- ... and check that we can inspect it
main = do  putStrLn $(do { info <- reify ''R
                         ; lift (pprint info) })
           print (foo (MkR { foo = 42 }))
