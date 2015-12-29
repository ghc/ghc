module Settings.Builders.Common (includesArgs) where

import Expression

includes :: [FilePath]
includes = [ "includes", "includes/dist-derivedconstants/header" ]

includesArgs :: Args
includesArgs = append $ map ("-I" ++) includes
