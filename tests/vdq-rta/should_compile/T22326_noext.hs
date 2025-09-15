{-# LANGUAGE NoRequiredTypeArguments #-}  -- VDQ/RTA is disabled
{-# LANGUAGE ExplicitNamespaces #-}

module T22326_noext where

import T22326_noext_def

-- The calls to id_vdq are accepted, even though RequiredTypeArguments are
-- disabled in this module.
--
-- The extension is only required at definition site, not at use sites.
--
s = id_vdq (type String) "Hello"