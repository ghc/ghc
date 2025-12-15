{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

import GHC.Int (Int(I#))
import GHC.Word (Word(W#))
import GHC.Exts (Int#,Word#,(+#))
import GHC.Types
import Data.Coerce (coerce)

main :: IO ()
main = do
  print (I# (coerce (Foo 5#)))

newtype Foo = Foo Int#
