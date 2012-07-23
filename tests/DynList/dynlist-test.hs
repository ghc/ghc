{-# LANGUAGE ScopedTypeVariables#-}

import DynList

main = do
  dl <- newDynList
  dl <- addToDynList dl "String"
  dl <- addToDynList dl (7::Int)
  v::String <- readDynList dl
  print v
  v::Int <- readDynList dl
  print $ show v
  ov <- updateDynList dl "NewString"
  print ("OldValue: " ++ ov)
  v::String <- readDynList dl
  print v
  v::Int <- readDynList dl
  print $ show v
