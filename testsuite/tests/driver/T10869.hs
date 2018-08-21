{-# LANGUAGE CPP #-}

module T10869 where
import T10869A

main :: IO()
#if defined(__GLASGOW_HASKELL__)
main = writeMsg
#endif
