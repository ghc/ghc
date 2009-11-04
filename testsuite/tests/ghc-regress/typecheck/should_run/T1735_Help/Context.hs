{-# LANGUAGE UndecidableInstances, OverlappingInstances, EmptyDataDecls #-}

{-

(C) 2004 Ralf Laemmel

Context parameterisation and context passing.

-}


module T1735_Help.Context

where

------------------------------------------------------------------------------

--
-- The Sat class from John Hughes' "Restricted Data Types in Haskell"
--

class Sat a
  where
    dict :: a


------------------------------------------------------------------------------

-- No context

data NoCtx a

noCtx :: NoCtx ()
noCtx = undefined

instance Sat (NoCtx a) where dict = undefined


------------------------------------------------------------------------------

-- Pair context

data PairCtx l r a
   = PairCtx { leftCtx  :: l a
             , rightCtx :: r a }

pairCtx :: l () -> r () -> PairCtx l r ()
pairCtx _ _ = undefined

instance (Sat (l a), Sat (r a))
      => Sat (PairCtx l r a)
  where
    dict = PairCtx { leftCtx  = dict
                   , rightCtx = dict }


------------------------------------------------------------------------------
