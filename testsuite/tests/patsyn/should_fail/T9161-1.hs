{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}

module Bug where

pattern PATTERN = ()

wrongLift :: PATTERN
wrongLift = undefined
