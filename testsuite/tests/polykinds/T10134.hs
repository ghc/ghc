{-# LANGUAGE DataKinds, TypeOperators, ConstraintKinds, TypeFamilies, NoMonoLocalBinds, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module T10134 where

import GHC.TypeLits
import T10134a
import Prelude

type Positive n = ((n-1)+1)~n

data Dummy n d = Dummy { vec :: Vec n (Vec d Bool) }

nextDummy :: Positive (2*(n+d)) => Dummy n d -> Dummy n d
nextDummy d = Dummy { vec = vec dFst }
   where (dFst,dSnd) = nextDummy' d

nextDummy' :: Positive (2*(n+d)) => Dummy n d -> ( Dummy n d, Bool )
nextDummy' _ = undefined
