data T1 = MkT1Bad                     | MkT1Good                     deriving Show
data T2 = MkT2Bad Int                 | MkT2Good Int                 deriving Show
data T3 = MkT3Bad {-# UNPACK #-} !Int | MkT3Good {-# UNPACK #-} !Int deriving Show
data T4 = MkT4Bad Int                 | MkT4Good Int                 deriving Show
data T5 = MkT5Bad {-# UNPACK #-} !Int | MkT5Good {-# UNPACK #-} !Int deriving Show

{-# RULES

"T1"           app MkT1Bad     = MkT1Good
"T2" forall x. app (MkT2Bad x) = MkT2Good x
"T3" forall x. app (MkT3Bad x) = MkT3Good x
"T4"           app MkT4Bad     = MkT4Good
"T5"           app MkT5Bad     = MkT5Good
  #-}

app = id
{-# NOINLINE app #-}

main = do
  print (app MkT1Bad)
  print (app (MkT2Bad 42))
  print (app (MkT3Bad 42))
  print (app MkT4Bad 42)
  print (app MkT5Bad 42)
