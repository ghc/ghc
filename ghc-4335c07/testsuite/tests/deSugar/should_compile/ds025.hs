-- !!! ds025 -- overloaded assoc -- AbsBinds

module ShouldCompile where

ehead xs loc | null xs = error ("4"++loc)
             | True = head xs

assoc key lst loc
   = if (null res) then error ("1"++loc++"2"++(show key))
                   else (ehead res "3")
     where res = [ val | (key',val) <- lst, key==key']

assocMaybe :: (Eq a) => a -> [(a,b)] -> Maybe b
assocMaybe key lst
 = if (null res) then Nothing else (Just (head res))
   where res =  [ val | (key',val) <- lst, key==key']
