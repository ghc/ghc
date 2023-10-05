-- Test purpose:
-- Ensure that using -Wno-compat does not switch on warnings

{-# OPTIONS_GHC -Wno-compat #-}
{-# LANGUAGE CPP #-}
#include "Template.hs"
