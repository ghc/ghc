{-# LANGUAGE ImplicitParams, ExistentialQuantification #-}

-- Both these two fail in 6.2.2 

module ShouldCompile where


-- A record with an 'existential' context that binds no
-- type vars, so record selectors should be OK
data Test1 = (?val::Bool) => Test1 { name :: String }
 
instance Show Test1 where
    show p = name p


-- Same, but no record selector; GHC 6.2.2 failed because it tried
-- to derive generic to/from 
data Test2 = (?val::Bool) => Test2 String
f (Test2 s) | ?val = s
