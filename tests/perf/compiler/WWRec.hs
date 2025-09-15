{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wno-simplifiable-class-constraints #-}

module WWRec where

class Rule f a where
  get :: Decorator f => f a
class Monad f => Decorator f where
  foo :: Rule f a => f a

data A1 = MkA1 A2
data A2 = MkA2 A3
data A3 = MkA3 A4
data A4 = MkA4 A5
data A5 = MkA5 A6
data A6 = MkA6 A7
data A7 = MkA7 A8
data A8 = MkA8 A9
data A9 = MkA9 A10
data A10 = MkA10 A11
data A11 = MkA11 A12
data A12 = MkA12 A13
data A13 = MkA13 A14
data A14 = MkA14 A15
data A15 = MkA15 A16
data A16 = MkA16 A17
data A17 = MkA17 A18
data A18 = MkA18 A19
data A19 = MkA19 A20
data A20 = MkA20 A21
data A21 = MkA21 A22
data A22 = MkA22 A23
data A23 = MkA23 A24
data A24 = MkA24 A25
data A25 = MkA25 A26
data A26 = MkA26 A27
data A27 = MkA27 A28
data A28 = MkA28 A29
data A29 = MkA29 A30
data A30 = MkA30 A1

instance Rule f A2 => Rule f A1 where get = MkA1 <$> foo
instance Rule f A3 => Rule f A2 where get = MkA2 <$> foo
instance Rule f A4 => Rule f A3 where get = MkA3 <$> foo
instance Rule f A5 => Rule f A4 where get = MkA4 <$> foo
instance Rule f A6 => Rule f A5 where get = MkA5 <$> foo
instance Rule f A7 => Rule f A6 where get = MkA6 <$> foo
instance Rule f A8 => Rule f A7 where get = MkA7 <$> foo
instance Rule f A9 => Rule f A8 where get = MkA8 <$> foo
instance Rule f A10 => Rule f A9 where get = MkA9 <$> foo
instance Rule f A11 => Rule f A10 where get = MkA10 <$> foo
instance Rule f A12 => Rule f A11 where get = MkA11 <$> foo
instance Rule f A13 => Rule f A12 where get = MkA12 <$> foo
instance Rule f A14 => Rule f A13 where get = MkA13 <$> foo
instance Rule f A15 => Rule f A14 where get = MkA14 <$> foo
instance Rule f A16 => Rule f A15 where get = MkA15 <$> foo
instance Rule f A17 => Rule f A16 where get = MkA16 <$> foo
instance Rule f A18 => Rule f A17 where get = MkA17 <$> foo
instance Rule f A19 => Rule f A18 where get = MkA18 <$> foo
instance Rule f A20 => Rule f A19 where get = MkA19 <$> foo
instance Rule f A21 => Rule f A20 where get = MkA20 <$> foo
instance Rule f A22 => Rule f A21 where get = MkA21 <$> foo
instance Rule f A23 => Rule f A22 where get = MkA22 <$> foo
instance Rule f A24 => Rule f A23 where get = MkA23 <$> foo
instance Rule f A25 => Rule f A24 where get = MkA24 <$> foo
instance Rule f A26 => Rule f A25 where get = MkA25 <$> foo
instance Rule f A27 => Rule f A26 where get = MkA26 <$> foo
instance Rule f A28 => Rule f A27 where get = MkA27 <$> foo
instance Rule f A29 => Rule f A28 where get = MkA28 <$> foo
instance Rule f A30 => Rule f A29 where get = MkA29 <$> foo
instance Rule f A1 => Rule f A30 where get = MkA30 <$> foo
