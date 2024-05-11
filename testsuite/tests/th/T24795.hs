{-# LANGUAGE TemplateHaskell #-}

-- print 10 twice, not 1
main = do
  print $ $(let x = 10::Int in [| let x = 1 in $[| x |] |])
  print $ $(let x = 10::Int in let q = [| x |] in [| let x = 1 in $q |])
