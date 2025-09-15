import Unsafe.Coerce

data D1 a = C11 a deriving Show
data D2 a b = C21 a | C22 b deriving Show
data D3 a b c = C31 a | C32 b | C33 c deriving Show
data D4 a b c d = C41 a | C42 b | C43 c | C44 d deriving Show
data D5 a b c d e = C51 a | C52 b | C53 c | C54 d | C55 e deriving Show
data D6 a b c d e f = C61 a | C62 b | C63 c | C64 d | C65 e | C66 f deriving Show
data D7 a b c d e f g = C71 a | C72 b | C73 c | C74 d | C75 e | C76 f | C77 g deriving Show
data D8 a b c d e f g h = C81 a | C82 b | C83 c | C84 d | C85 e | C86 f | C87 g | C88 h deriving Show

d1 :: (Show a) => p a -> String
d2 :: (Show a, Show b) => p a b -> String
d3 :: (Show a, Show b, Show c) => p a b c -> String
d4 :: (Show a, Show b, Show c, Show d) => p a b c d -> String
d5 :: (Show a, Show b, Show c, Show d, Show e) => p a b c d e -> String
d6 :: (Show a, Show b, Show c, Show d, Show e, Show f) => p a b c d e f -> String
d7 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => p a b c d e f g -> String
d8 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) => p a b c d e f g h -> String

d1 = show . (unsafeCoerce :: p a -> D1 a)
d2 = show . (unsafeCoerce :: p a b -> D2 a b)
d3 = show . (unsafeCoerce :: p a b c -> D3 a b c)
d4 = show . (unsafeCoerce :: p a b c d -> D4 a b c d)
d5 = show . (unsafeCoerce :: p a b c d e -> D5 a b c d e)
d6 = show . (unsafeCoerce :: p a b c d e f -> D6 a b c d e f)
d7 = show . (unsafeCoerce :: p a b c d e f g -> D7 a b c d e f g)
d8 = show . (unsafeCoerce :: p a b c d e f g h -> D8 a b c d e f g h)
