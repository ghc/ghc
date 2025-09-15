{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitLevelImports #-}
{-# LANGUAGE TypeApplications #-}

module SI36 where

import Data.Kind ( Constraint )

import SI36_A
import SI36_C1
import quote SI36_C2
import splice SI36_C3

need :: forall (c :: Constraint). c => ()
need = ()

c1B1, c2B2, c3B3 :: ()
c1B1 = need @( C1 "B1" ) -- OK: normal import of C1, which normal imports B1 which provides it
c2B2 = need @( C2 "B2" ) -- } NO: not available at any levels, because modules only export
c3B3 = need @( C3 "B3" ) -- }   instances at level 0

c1C1, c1C2, c1C3 :: ()
c1C1 = need @( C1 "C1" ) -- OK: available at level 0
c1C2 = need @( C1 "C2" ) -- NO: only at level 1
c1C3 = need @( C1 "C3" ) -- NO: only at level -1

c2C1, c2C2, c2C3 :: ()
c2C1 = need @( C2 "C1" ) -- OK
c2C2 = need @( C2 "C2" ) -- NO: only at level 1
c2C3 = need @( C2 "C3" ) -- NO: only at level -1

c3C1, c3C2, c3C3 :: ()
c3C1 = need @( C3 "C1" ) -- OK
c3C2 = need @( C3 "C2" ) -- NO: only at level 1
c3C3 = need @( C3 "C3" ) -- NO: only at level -1
