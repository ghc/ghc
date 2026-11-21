{-# LANGUAGE BangPatterns, DeepSubsumption, MonoLocalBinds, NoExperimentalDependentTypes #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}

module RetainedPackageFormatClient (formatPackage) where

import RetainedPackageFormat

-- | Reopen the package and format its result using the Show superclass.
formatPackage :: forall a. Printable a => Bool -> OpenValue a -> String
formatPackage tag package =
    let !(state, readState) = relay id package
    in let format :: Formatter -> String
           format formatter = formattedText (formatter tag @a (readState state))
       in format formatValue
