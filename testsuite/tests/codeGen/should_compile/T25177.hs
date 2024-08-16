{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}

-- only export bar!
module T25177 (bar) where

import GHC.Exts

data D = D !Word# !Int#

{-# OPAQUE foo #-}
-- foo has an absent demand on D's Int#
foo :: D -> Word
foo (D a _) = W# a


bar :: Int# -> IO ()
bar !x = do
  -- we allocate a D:
  --  - used twice: otherwise it is inlined
  --  - whose second arg:
  --    - has an absent demand
  --    - is an unboxed Int# (hence won't be replaced by an "absentError blah"
  --    but by a LitRubbish)
  --
  -- GHC should detect that `17# +# x` is absent. Then it should lift `d` to the
  -- top-level. This is checked by dumping Core with -ddump-simpl.
  let d = D 10## (17# +# x)
  let !r1 = foo d -- luckily CSE doesn't kick in before floating-out `d`...
  let !r2 = foo d -- otherwise, pass a additional dummy argument to `foo`
  pure ()
