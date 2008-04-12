
-- Haddock comments are parsed as separate declarations so we
-- need to insert a ';' when using them with explicit layout.
-- This should probably be changed.

module NoLayout where {
  -- | the class 'C'
  ;
  g :: Int;
  g = undefined
 }

