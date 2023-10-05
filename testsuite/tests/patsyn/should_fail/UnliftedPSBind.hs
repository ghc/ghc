
-- This is testing the printing of the builder really.
{-# LANGUAGE MagicHash, PatternSynonyms #-}
{-# OPTIONS_GHC -Werror -Wunbanged-strict-patterns #-}
module UnliftedPSBind where

import GHC.Exts

pattern P x = I# x

x = ()
  where P x = P 4#
