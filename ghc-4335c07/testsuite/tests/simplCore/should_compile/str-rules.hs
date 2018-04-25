{-# LANGUAGE MagicHash #-}
import GHC.CString (unpackFoldrCString#, unpackCString#)
import GHC.Base (eqString)
main :: IO ()
main = do
  let mix c n = fromEnum c + n
  n <- readLn

  print $
    -- We expect the two literals to be concatenated, resulting in "@@@ ab"
    unpackFoldrCString# "@@@ a"# mix
      (unpackFoldrCString# "b"# mix n)

  if eqString (unpackCString# "x"#) (unpackCString# "y"#)
    then putStrLn $ unpackCString# "@@@ c"# -- this should be optimized out
    else putStrLn $ unpackCString# "@@@ d"#

  if eqString (unpackCString# "foo"#) (unpackCString# "foo"#)
    then putStrLn $ unpackCString# "@@@ e"#
    else putStrLn $ unpackCString# "@@@ f"# -- this should be optimized out
