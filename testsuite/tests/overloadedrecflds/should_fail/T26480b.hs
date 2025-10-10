{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}

module T26480b where

import Prelude
import Data.Proxy
import GHC.TypeLits
import GHC.Records


setField
  :: forall (fld :: Symbol) rec ty
  .  HasField fld rec ty => ty -> rec -> rec
setField _ r = r

data N = N { no :: H }

data D = MkD{ field1 :: G }

data G = MkG { xyzzywyzzydyzzy :: H }

data H = MkH { field2 :: Int }

-- Direct usage of 'getField'
test1 :: G -> H
test1 = getField @"xyzzywyzzydyzza"

test1' :: N -> H
test1' = getField @"xyzzywyzzydyzzy"

test1'' :: N -> H
test1'' = getField @"ayzzywyzzydyzzy"

-- Record dot, applied
test2a :: G -> H
test2a g = g.xyzzywyzzydyzzb

test2b :: D -> H
test2b g = g.field1.xyzzywyzzydyzzc

-- Record dot, bare selector
test3a :: G -> H
test3a = (.xyzzywyzzydyzzd)

test3b :: D ->H
test3b = (.field1.xyzzywyzzydyzze)

-- Overloaded record update
test4a :: G -> G
test4a d = d { xyzzywyzzydyzzf = MkG ( MkH 3 ) }

test4b :: D -> D
test4b d = d { field1.xyzzywyzzydyzzg = MkH 3 }
