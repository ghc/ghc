 {-# OPTIONS  -XMultiParamTypeClasses  -XFunctionalDependencies  -XFlexibleInstances #-}  
module HaskellBug where

data Relation c -- The basic Relation
      = Rel { relnm :: String -- The name of the relation
            , relsrc :: c -- Source concept
            , reltrg :: c -- ^Target concept
            }
        deriving Eq

-- This declaration is ok; should not get an error here
class (Eq concept)=> Association rel concept | rel -> concept where
     source, target :: rel -> concept
      -- e.g. Declaration Concept -> Concept
     sign  :: rel -> (concept,concept)
     sign x = (source x,target x)
     homogeneous :: rel -> Bool
     homogeneous s = source s == target s

instance (Eq c)=>Association (Relation c) c where
     source = relsrc
     target = reltrg

-- This declaration has a kind error
-- The error should be reported here
class (Eq c, Association r c) => Morphic r c where
     multiplicities :: r c -> [c]
     multiplicities _ = []
