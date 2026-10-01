{-# OPTIONS_GHC -fplugin=T26761Plugin #-}
-- | A module that imports another module with an annotation.
-- We want to check that the annotation is found by the plugin even
-- when the imported module uses -fno-code.
module T26761 where
import T26761a
