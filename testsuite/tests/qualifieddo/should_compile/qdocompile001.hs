{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QualifiedDo #-}

import Prelude as P

-- Test that the context of the do shows well in the renamer
-- output.
--
-- The nested do in the renamer output must be qualified the
-- same as the outer P.do written in the source program.
--
-- > ==================== Renamer ====================
-- > Main.main
-- >   = print
-- >       $ P.do (x <- [1, 2] |
-- >               y <- P.do y@1 <- [1, 2] -- qualified!
-- >                         [1, 2]
-- >                         y)
-- >               return y
--
main =
  print $ P.do
    x <- [1, 2]
    y@1 <- [1, 2]
    [1, 2]
    P.return y
