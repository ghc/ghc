{-# LANGUAGE DataKinds, MagicHash, TypeFamilies, TypeApplications #-}

import HasFieldFail01_A (T(MkT))

import GHC.Records (HasField(..))

-- This should fail to solve the HasField constraint, because foo is
-- not in scope.
main = print (getField @"foo" (MkT 42) :: Int)
