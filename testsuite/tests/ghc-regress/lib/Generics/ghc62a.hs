import Data.Generics

-- file = "Tst.hs"

d  = (["hi","head"],("ho",(True,"head")))
d3 = (["hi","head"],"ho",(True,"head"))

main = print $ everywhere (id `extT` worker) d3
  where
    worker "head" = "tail"
    worker s = s
