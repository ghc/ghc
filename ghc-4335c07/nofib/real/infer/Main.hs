module Main where

import Parse
import Shows
import Term
import Type
import Environment
import InferMonad
import Substitution	( Sub )
import MaybeM		( Maybe )
import Infer

main          =  interact
                   ( showsString (show testEnv ++ prompt)
                   . concat
                   . map readInferShow
                   . lines
                   )

readInferShow :: String -> String
readInferShow =  useP ("Failed to parse" ++ prompt)   (
                 lexactlyP reads                      `eachP` (\t ->
                 useI ("Failed to type" ++ prompt)    (
                 inferTerm testEnv t                  `eachI` (\tt ->
                 show t ++ " : " ++ show tt ++ prompt))))
testEnv       :: Env
testEnv       =  read
                   (   "[ unit   : x -> List x,"
                   ++  "  append : List x -> List x -> List x,"
                   ++  "  fix    : (x -> x) -> x ]"
                   )
prompt        :: String
prompt        =  "\n? "
