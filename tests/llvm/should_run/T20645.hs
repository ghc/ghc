-- Minimal reproducer for https://gitlab.haskell.org/ghc/ghc/-/issues/20645
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ExtendedLiterals #-}
import GHC.Exts
import GHC.Word
import Numeric (showHex)

opaqueInt8# :: Int8# -> Int8#
opaqueInt8# x = x
{-# OPAQUE opaqueInt8# #-}

main :: IO ()
main = let !x = opaqueInt8# 109#Int8
           !y = opaqueInt8#   1#Int8
       in putStrLn $ flip showHex "" (W# ( pext8#
              (word8ToWord# (int8ToWord8# (0#Int8 `subInt8#` x     )))
              (word8ToWord# (int8ToWord8# (y      `subInt8#` 4#Int8)))
          ))
