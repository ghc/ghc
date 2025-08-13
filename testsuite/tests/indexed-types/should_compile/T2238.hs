{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- #2238
-- Notice that class CTF has just one value field, but
-- it also has an equality predicate.
-- See Note [Unary class magic] in GHC.Core.TyCon

module Foo where

data A
data B

-- via functional dependencies

class HowFD a how | a -> how

class HowFD a how => CFD a how where
  cfd :: a -> String
  cfd _ = "cfd"
instance HowFD a how => CFD a how

instance HowFD Bool A

-- via type families

type family HowTF a

class how ~ HowTF a => CTF a how where
  ctf :: a -> String
  ctf _ = "ctf"

instance how ~ HowTF a => CTF a how

type instance HowTF Bool = A
