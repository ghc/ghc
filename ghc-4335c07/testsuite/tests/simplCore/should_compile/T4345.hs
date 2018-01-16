-- This one elicited a bug in the simplifier
-- that produces a Lint out-of-scope error

module T4345 where

isNull :: IO Bool
isNull = error "urk"

wrapMatchAll :: IO (Maybe ())
wrapMatchAll = do
   nsub <- undefined
   let loop True = do atEnd <- isNull
                      return Nothing
       loop False = loop False
   result <- undefined
   loop undefined
