{-# language PolyKinds #-}
{-# language FlexibleContexts #-}
{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}

import GHC.Exts (Constraint)

class Ríki (p :: i -> i -> *)
class (Ríki p) => Varpi p q f | f -> p q
instance Varpi () () f => Varpi (->) (->) (Either f) where
