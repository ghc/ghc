{-# LANGUAGE CPP #-}

-- | Hack to keep Control.DeepSeq SAFE-inferred
--
-- This module only re-export reasonably safe entities from non-safe
-- modules when there is no safe alternative

#if MIN_VERSION_base(4,9,0) || (MIN_VERSION_base(4,6,0) && !MIN_VERSION_base(4,7,0))
{-# LANGUAGE Safe #-}

module Control.DeepSeq.BackDoor
       {-# WARNING "This module is empty! Do not import me!" #-}
       () where

#else
{-# LANGUAGE Trustworthy #-}

module Control.DeepSeq.BackDoor
    ( module X
    ) where

#if !(MIN_VERSION_base(4,6,0))
-- not SAFE
import GHC.Exts as X ( Down(Down) )
#endif

#if MIN_VERSION_base(4,10,0)
-- Data.Type.Equality SAFE starting with base-4.10
#elif MIN_VERSION_base(4,7,0)
import Data.Type.Equality as X ( (:~:) )
#endif

#endif
