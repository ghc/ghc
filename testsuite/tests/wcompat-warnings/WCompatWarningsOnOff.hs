-- Test purpose:
-- Ensure that -Wno-compat disables a previously set -Wcompat

{-# OPTIONS_GHC -Wcompat -Wno-compat #-}
{-# LANGUAGE CPP #-}
#include "Template.hs"
