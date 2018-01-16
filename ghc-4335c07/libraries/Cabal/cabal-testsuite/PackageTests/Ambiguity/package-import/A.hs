{-# LANGUAGE PackageImports #-}

import qualified "p" Dupe as PDupe
import qualified "q" Dupe as QDupe

main = putStrLn (PDupe.pkg ++ " " ++ QDupe.pkg)

