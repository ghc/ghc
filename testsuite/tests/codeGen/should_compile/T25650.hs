module T25650 (baz_foo, baz_bar) where

import Data.Word

data A
  = A1 {-# UNPACK #-} !Word32
  | A2 {-# UNPACK #-} !B

data B = B1 | B2

foo = A1 10
bar = A2 B2

data C = C {-# UNPACK #-} !A

-- Static closures of C must be laid out as below (#25650).  On 32-bit
-- targets a W32 field is a plain word.

-- CHECK-LABEL: T25650.baz_foo_closure:
-- CHECK-NEXT:  const T25650.C_con_info;
-- WS64-NEXT:   const 10 :: W32;
-- WS32-NEXT:   const 10;
-- CHECK-NEXT:  const 1 :: W8;
-- CHECK-NEXT:  const 0 :: W8;
-- CHECK-NEXT:  const 0 :: W16;
baz_foo = C foo

-- CHECK-LABEL: T25650.baz_bar_closure:
-- CHECK-NEXT:  const T25650.C_con_info;
-- WS64-NEXT:   const 2 :: W32;
-- WS32-NEXT:   const 2;
-- CHECK-NEXT:  const 2 :: W8;
-- CHECK-NEXT:  const 0 :: W8;
-- CHECK-NEXT:  const 0 :: W16;
baz_bar = C bar
