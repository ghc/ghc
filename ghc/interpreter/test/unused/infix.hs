--!!! Testing the printing of infix constructors
data Music = Note
           | Music :+: Music
           | Scale Music
  deriving Show

m = Scale (Note :+: Note)
