{-# Language RankNTypes       #-}
{-# Language GADTs            #-}
{-# Language TypeApplications #-}
{-# Language PolyKinds        #-}

{-# Options_GHC -dcore-lint #-}
module T15788 where
import Data.Kind

data A :: forall k. Type where
 MkA :: A @k
