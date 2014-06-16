{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
-- !!! Scopes in kind checking

-- Exposes a bizarre bug in 4.08.1 
--    TestSh.hs:6:
--	`Shape' is not in scope
--	When checking kinds in `HasConfigValue Shape nodeTypeParms'
--	In the class declaration for `HasShape'

module ShouldCompile where

data Shape value = Box | Circle

class HasConfigValue Shape nodeTypeParms => HasShape nodeTypeParms where {}

class HasConfigValue option configuration where
   ($$$) :: option value -> configuration value -> configuration value


