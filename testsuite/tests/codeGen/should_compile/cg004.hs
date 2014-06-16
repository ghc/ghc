module ShouldCompile where

-- 		Killed GHC 6.0 in isCrossDllArg
--
-- ghc-6.0: panic! (the `impossible' happened, GHC version 6.0):
--	coreSyn/CoreUtils.lhs:1188: Non-exhaustive patterns in function isCrossDllArg
--
-- The reason was that newST had the form
--   newST = \ @ v -> GHC.Base.:
--			@ (Environment.Scope v)
--			(case $fScopeOpersScope @ v
--			 of tpl_B1 { Environment.:DScopeOpers tpl_B2 tpl_B3 ->
--			 tpl_B2
--			 })
--			(GHC.Base.[] @ (Environment.Scope v))

class ScopeOpers s where
      emptyScope :: s 
      op :: s -> s

data Scope v = NewScope 

instance ScopeOpers (Scope v) where
      emptyScope = error "emptyScope"
      op = error "op"

newtype SymbolTable v = SymbolTable [Scope v]

newST :: SymbolTable v
newST = SymbolTable [emptyScope]
