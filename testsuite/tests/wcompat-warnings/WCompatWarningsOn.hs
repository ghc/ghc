-- Test purpose:
-- Ensure that -Wcompat switches on the right warnings

{-# OPTIONS_GHC -Wcompat -Wno-error=compat #-}
{-# LANGUAGE CPP #-}
#include "Template.hs"
