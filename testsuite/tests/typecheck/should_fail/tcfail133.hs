{-# LANGUAGE UndecidableInstances, FlexibleInstances, TypeOperators,
             MultiParamTypeClasses, FunctionalDependencies, DatatypeContexts #-}

-- This one crashed GHC 6.3 due to an error in TcSimplify.add_ors

module Foo where

data Zero   = Zero deriving Show
data One    = One deriving Show
infixl 9 :@
data (Number a, Digit b) => a :@ b = a :@ b deriving Show

class Digit a
instance Digit Zero
instance Digit One

class Number a
instance Number Zero
instance Number One
instance (Number a, Digit b) => Number (a :@ b)

--- Pretty printing of numbers ---
class PrettyNum a where
    prettyNum   :: a -> String

instance PrettyNum Zero where
    prettyNum _ = "0"

instance PrettyNum One where
    prettyNum _ = "1"

instance (Number a, Digit b, PrettyNum a, PrettyNum b)
      => PrettyNum (a :@ b) where
    prettyNum ~(a:@b)
                = prettyNum a ++ prettyNum b

--- Digit addition ---
class (Number a, Digit b, Number c)
   => AddDigit a b c | a b -> c where
    addDigit    :: a -> b -> c
    addDigit    = undefined

instance Number a => AddDigit a Zero a
instance AddDigit Zero One One
instance AddDigit One One (One:@Zero)
instance Number a => AddDigit (a:@Zero) One (a:@One)
instance AddDigit a One a'
      => AddDigit (a:@One) One (a':@Zero)

--- Addition ---
class (Number a, Number b, Number c)
   => Add a b c | a b -> c where
    add     :: a -> b -> c
    add     = undefined

instance Number n => Add n Zero n
instance Add Zero One One
instance Add One One (One:@One)
instance Number n
      => Add (n:@Zero) One (n:@One)
instance AddDigit n One r'
      => Add (n:@One) One (r':@Zero)
instance (Number n1, Digit d1, Number n2, Digit n2
         ,Add n1 n2 nr', AddDigit (d1:@nr') d2 r)
      => Add (n1:@d1) (n2:@d2) r


foo = show $ add (One:@Zero) (One:@One)


-- Add (One:@Zero) (One:@One) c, Show c
-- ==> Number One, Digit Zero, Number One, Digit One
--     Add One One nr', AddDigit (Zero:@nr') One c, Show c
--
-- ==> Add One One nr', AddDigit (Zero:@nr') One c, Show c
-- 
-- ==> Add One One (One:@One), AddDigit (Zero:@(One:@One)) One c, Show c
--
-- ==> AddDigit (Zero:@(One:@One)) One c, Show c
