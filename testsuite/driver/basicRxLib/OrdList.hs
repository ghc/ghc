module OrdList where

{- an OrdList is an ordered list, with no duplicates, and set like
   operations on it
-}

type OrdList a = [a]


emptyOL :: Ord a => OrdList a 
emptyOL = []

unionOL :: Ord a => OrdList a -> OrdList a -> OrdList a
unionOL s1 s2 = foldr addEl s2 s1

listToOL :: Ord a => [a] -> OrdList a
listToOL xs = (foldr addEl [] xs)
              
addEl :: Ord a =>
         a		-- element
      -> OrdList a      -- ordered list
      -> OrdList a      -- list with element added to it
addEl e [] = [e]
addEl e (n:ns) = if n == e then
                   n:ns
                 else if n<e then 
                   n:(addEl e ns)
                 else
                   e:n:ns

intersectOL :: Ord a => OrdList a -> OrdList a -> OrdList a
intersectOL s1 s2 = foldr (inboth s2) [] s1
                  where
                     inboth :: Ord a => OrdList a -> a -> OrdList a -> OrdList a
                     inboth s e new = if e `elem` s then 
                                       addEl e new
                                      else 
                                       new                

singletonOL :: Ord a => a -> OrdList a
singletonOL x = [x]

minusOL :: Ord a => OrdList a -> OrdList a -> OrdList a
minusOL s1 s2 = foldr minusel s1 s2
                 where
                    minusel e [] = []
                    minusel e (n:ns) = if n == e then
                                         ns
                                       else if n<e then
                                         n:(minusel e ns)
                                       else
                                         n:ns

member :: Ord a => a -> OrdList a -> Bool
member x [] = False
member x (s:ss) = if x == s then
                    True
                  else if x>s then
                    member x ss
                  else
                    False

foldS :: (a -> b -> b) -> b -> OrdList a -> b 
foldS = foldr

mapS :: (a -> b) -> OrdList a -> OrdList b
mapS = map



