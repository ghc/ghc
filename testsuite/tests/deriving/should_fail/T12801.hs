data Container
  = Container [Wibble Int]
  deriving (Eq, Show)

data Wibble a
  = Wibble a
  | Wobble
  deriving (Eq, Functor, Show)
