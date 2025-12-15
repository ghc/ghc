{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# LANGUAGE FunctionalDependencies, UndecidableInstances, DataKinds
#-}

module T6015a where

import Prelude hiding ((++))

data T a = T

class ((a :: [k]) ++ (b :: [k])) (c :: [k]) | a b -> c  
instance ('[] ++ b) b  
instance (a ++ b) c => ((x ': a) ++ b) (x ': c)

test = T :: ('[True] ++ '[]) l => T l
