{-# LANGUAGE CPP #-}

main :: IO ()
main = do
#if defined(__GLASGOW_HASKELL_LLVM__)
  putStrLn $ "__GLASGOW_HASKELL_LLVM__ = " ++ show __GLASGOW_HASKELL_LLVM__
#else
#error __GLASGOW_HASKELL_LLVM__ is not defined
#endif
