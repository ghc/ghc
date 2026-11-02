-- -fsplit-sections must be ignored on Darwin; see
-- Note [Split sections are not supported on Darwin] in GHC.Driver.Session.
--
-- Mach-O cannot name a section after a symbol, so GHC ignores -fsplit-sections
-- on Darwin. Without that, llvmFunSection emits ELF-style ".text.<label>" names
-- and LLVM rejects them. Only reproduces on AArch64/Darwin: on
-- x86_64/Darwin the flag is already dropped for subsections-via-symbols.
-- -Wno-inconsistent-flags suppresses the "Ignoring" warning, whose wording
-- differs between the two Darwin platforms.
module SplitSectionsDarwin (f) where

f :: Int -> Int
f x = x + 1
