{-# LANGUAGE NoTraditionalRecordSyntax, NoDoAndIfThenElse #-}

-- module T16270 (type G) where

c = do
  if c then
    False
  else
    True

f = id do { 1 }
g = id \x -> x

data Num a => D a

data Pair a b = Pair { fst :: a, snd :: b }
t = p { fst = 1, snd = True }

z = if True; then (); else ();

data G a where
