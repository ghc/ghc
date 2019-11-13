{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

import GHC.Exts
import GHC.CString

main :: IO ()
main = print (I# (cstringLength# "hello_world"#))
