{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls, FlexibleInstances #-}

module InstContextNorm
where

data EX _x _y (p :: * -> *)
data ANY

class Base p

class Base (Def p) => Prop p where
 type Def p

instance Base ()
instance Prop () where
 type Def () = ()

instance (Base (Def (p ANY))) => Base (EX _x _y p)
instance (Prop (p ANY)) => Prop (EX _x _y p) where
 type Def (EX _x _y p) = EX _x _y p


data FOO x

instance Prop (FOO x) where
 type Def (FOO x) = ()

data BAR

instance Prop BAR where
 type Def BAR = EX () () FOO
  
  -- Needs Base (Def BAR)
  -- And (Def Bar = Ex () () FOO)
  -- so we need Base (Def (Foo ANY))