{-# LANGUAGE TypeFamilies #-}

module Simple15 where

(<$) :: p -> (p -> q) -> q
x <$ f = f x

type family Def p

def :: Def p -> p
def = undefined

data EQU a b = EQU

equ_refl :: EQU a a
equ_refl = EQU

data FOO = FOO
type instance Def FOO = EQU () ()

foo :: FOO
foo = equ_refl <$ def
-- This works:
-- foo = def $ equ_refl

