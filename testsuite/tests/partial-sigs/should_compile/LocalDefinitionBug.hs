{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
module LocalDefinitionBug where

monoLoc :: forall a. a -> ((a, String), (a, _))
monoLoc x = (g True , g False)
  where
    g :: t -> (a, String)
    g _ = (x, "foo")

-- Test case for (fixed) bug that previously generated the following error message:

-- LocalDefinitionBug.hs:9:16:
--     GHC internal error: ‘a’ is not in scope during type checking, but it passed the renamer
--     tcl_env of environment: [alA :-> Type variable ‘_’ = _,
--                              alC :-> Identifier[x::a, <NotTopLevel>],
--                              alE :-> Type variable ‘t’ = t,
--                              rjF :-> Identifier[monoLoc::a
--                                                          -> ((a, String), (a, _)), <NotTopLevel>]]
--     In the type signature for ‘g’: g :: t -> (a, String)
--     In an equation for ‘monoLoc’:
--         monoLoc x
--           = (g True, g False)
--           where
--               g :: t -> (a, String)
--               g _ = (x, "foo")


-- Fixed by using tcExtendTyVarEnv2 instead of tcExtendTyVarEnv
