{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

type Empty a = ()

foo :: expr a -> expr a -> expr (Empty a)
foo = undefined

newtype Expr a = SPT {run :: String}

pt1 :: forall a ptexpr . ptexpr a -> ptexpr (Empty a)
pt1 a = foo a a

pt2 :: forall a ptexpr . ptexpr a -> ptexpr _
pt2 a = foo a a

main :: IO ()
main = do
    -- This typechecks without any trouble.
    putStrLn $ run $ pt1 @Int @Expr undefined

    -- This should also typecheck, but doesn't since GHC seems to mix up the
    -- order of the type variables.
    putStrLn $ run $ pt2 @Int @Expr undefined
