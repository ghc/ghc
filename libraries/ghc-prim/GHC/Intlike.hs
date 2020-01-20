{-# LANGUAGE MagicHash, NoImplicitPrelude #-}

module Intlike where

-- See Note [INTLIKE closures] in StcMiscClosures.cmm
-- for more details.
data StaticBoxedWordType = BWT# Word#
