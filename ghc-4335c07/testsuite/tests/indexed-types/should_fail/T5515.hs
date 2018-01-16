{-# LANGUAGE ConstraintKinds, FlexibleInstances, TypeFamilies,
     MultiParamTypeClasses, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
module T5515 where


class ctx (Arg ctx) => Bome ctx where 
  type BArg ctx
instance ctx a => Bome ctx where 
  type BArg ctx = a

class C f a
class C f (Arg f) => Some f where 
  type Arg f
instance C f a => Some f where 
  type Arg f = a


