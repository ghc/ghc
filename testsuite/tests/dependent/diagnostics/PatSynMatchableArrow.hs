{-# LANGUAGE DataKinds, PatternSynonyms, TypeApplications #-}
module PatSynMatchableArrow where
import GHC.Exts (FUN, M, Multiplicity(..))
pattern P :: FUN @M 'Many Int (Maybe Int)
pattern P x <- Just x
