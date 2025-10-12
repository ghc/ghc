{-# LANGUAGE GHC_CPP #-}
module Example6 where

data GHCVersion = GHC910
           | GHC912
     deriving (Eq, Ord, Show)

ghcVersion :: GHCVersion
#if MIN_VERSION_ghc(9,12,0)
ghcVersion = GHC912
#else
ghcVersion = GHC910
#endif

testDirs :: [FilePath]
testDirs =
  case ghcVersion of
    GHC910 -> ["pre-ghc910", "ghc910"]
    GHC912 -> ["pre-ghc910", "ghc910", "ghc912"]
