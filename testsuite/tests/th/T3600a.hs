{-# LANGUAGE TemplateHaskell #-}
module T3600a where

import Language.Haskell.TH

test :: Q [Dec]
test = do
     let args1 = [] :: [String]
         args2 = [] :: String
         args3 = "x" :: String
         body = [| (testFun1 args1, testFun2 args2, testFun2 args3)  |]
         decNm = mkName "myFunction"
     (:[]) `fmap` funD decNm [clause [] (normalB body) []]

testFun1 :: [String] -> String
testFun1 _ = "hello"

testFun2 :: String -> String
testFun2 _ = "goodbye"
