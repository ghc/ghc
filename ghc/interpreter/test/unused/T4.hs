--!!! Error detection in class declarations.

-- From the GHC bugs mailing list - this isn't legal Haskell.
-- (reported by Einar Wolfgang Karlsen <ewk@informatik.uni-bremen.de>)

class Silly x where
  dump :: Silly x => x -> String  -- context is illegal
