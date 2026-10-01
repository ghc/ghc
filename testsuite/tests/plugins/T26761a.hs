{-# OPTIONS_GHC -fno-code -fplugin=T26761Plugin #-}
-- | A module with an annotation that we can find from a plugin.
-- The annotations should be visible even when using -fno-code.
-- The annotation is inserted by the plugin itself.
module T26761a where
