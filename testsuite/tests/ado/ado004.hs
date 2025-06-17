{-# LANGUAGE ApplicativeDo, GHC2021 #-}
{-# OPTIONS_GHC -ddump-types #-}
module Test where

-- This is a do expression that typechecks with only an Applicative constraint
test1 :: Applicative f => (Int -> f Int) -> f Int
test1 f = do
  x <- f 3
  y <- f 4
  return (x + y)

-- The same using $
test1a :: Applicative f => (Int -> f Int) -> f Int
test1a f = do
  x <- f 3
  y <- f 4
  return $ x + y

-- When one of the statements is a BodyStmt
test1b :: Applicative f => (Int -> f Int) -> f Int
test1b f = do
  x <- f 3
  f 4
  return x

test1c :: Applicative f => (Int -> f Int) -> f Int
test1c f = do
  f 3
  x <- f 4
  return x

-- Test we can also infer the Applicative version of the type
test2 f = do
  x <- f 3
  y <- f 4
  return (x + y)

-- Test we can also infer the Functor version of the type
test2a f = do
  x <- f 3
  return (x + 1)

-- The same using $
test2c f = do
  x <- f 3
  return $ x + 1

-- with a BodyStmt
test2d f = do
  f 3
  return 4

-- Test for just one statement
test2b f = do
  return (f 3)

-- This one will use join
test3 f g = do
  x <- f 3
  y <- f 4
  g y x

-- This one needs a tuple
test4 f g = do
  x <- f 3
  y <- f 4
  let r = g y x
  r

-- This one used to need a big tuple, now it compiles to ApplicativeLastStmt
test5 f g = do
  x01 <- f 01
  x02 <- f 02
  x03 <- f 03
  x04 <- f 04
  x05 <- f 05
  x06 <- f 06
  x07 <- f 07
  x08 <- f 08
  x09 <- f 09
  x11 <- f 11
  x12 <- f 12
  x13 <- f 13
  x14 <- f 14
  x15 <- f 15
  x16 <- f 16
  x17 <- f 17
  x18 <- f 18
  x19 <- f 19
  x20 <- f 20
  x21 <- f 21
  x22 <- f 22
  x23 <- f 23
  x24 <- f 24
  x25 <- f 25
  x26 <- f 26
  x27 <- f 27
  x28 <- f 28
  x29 <- f 29
  x30 <- f 30
  x31 <- f 31
  x32 <- f 32
  x33 <- f 33
  x34 <- f 34
  x35 <- f 35
  x36 <- f 36
  x37 <- f 37
  x38 <- f 38
  x39 <- f 39
  x40 <- f 40
  x41 <- f 41
  x42 <- f 42
  x43 <- f 43
  x44 <- f 44
  x45 <- f 45
  x46 <- f 46
  x47 <- f 47
  x48 <- f 48
  x49 <- f 49
  x50 <- f 50
  x51 <- f 51
  x52 <- f 52
  x53 <- f 53
  x54 <- f 54
  x55 <- f 55
  x56 <- f 56
  x57 <- f 57
  x58 <- f 58
  x59 <- f 59
  x60 <- f 60
  x61 <- f 61
  x62 <- f 62
  x63 <- f 63
  x64 <- f 64
  x65 <- f 65
  x66 <- f 66
  x67 <- f 67
  x68 <- f 68
  x69 <- f 69
  x70 <- f 70
  let r = g x70 x01
  r

-- This one needs a big tuple
test6 f g = do
  x01 <- f 01
  x02 <- f 02
  x03 <- f 03
  x04 <- f 04
  x05 <- f 05
  x06 <- f 06
  x07 <- f 07
  x08 <- f 08
  x09 <- f 09
  x11 <- f 11
  x12 <- f 12
  x13 <- f 13
  x14 <- f 14
  x15 <- f 15
  x16 <- f 16
  x17 <- f 17
  x18 <- f 18
  x19 <- f 19
  x20 <- f 20
  x21 <- f 21
  x22 <- f 22
  x23 <- f 23
  x24 <- f 24
  x25 <- f 25
  x26 <- f 26
  x27 <- f 27
  x28 <- f 28
  x29 <- f 29
  x30 <- f 30
  x31 <- f 31
  x32 <- f 32
  x33 <- f 33
  x34 <- f 34
  x35 <- f 35
  x36 <- f 36
  x37 <- f 37
  x38 <- f 38
  x39 <- f 39
  x40 <- f 40
  x41 <- f 41
  x42 <- f 42
  x43 <- f 43
  x44 <- f 44
  x45 <- f 45
  x46 <- f 46
  x47 <- f 47
  x48 <- f 48
  x49 <- f 49
  x50 <- f 50
  x51 <- f 51
  x52 <- f 52
  x53 <- f 53
  x54 <- f 54
  x55 <- f 55
  x56 <- f 56
  x57 <- f 57
  x58 <- f 58
  x59 <- f 59
  x60 <- f 60
  x61 <- f 61
  x62 <- f 62
  x63 <- f 63
  x64 <- f 64
  x65 <- f 65
  x66 <- f 66
  x67 <- f 67
  x68 <- f 68
  x69 <- f 69
  x70 <- f x01
  x71 <- f 70
  x71 `const`
   [ x01
   , x02
   , x03
   , x04
   , x05
   , x06
   , x07
   , x08
   , x09
   , x11
   , x12
   , x13
   , x14
   , x15
   , x16
   , x17
   , x18
   , x19
   , x20
   , x21
   , x22
   , x23
   , x24
   , x25
   , x26
   , x27
   , x28
   , x29
   , x30
   , x31
   , x32
   , x33
   , x34
   , x35
   , x36
   , x37
   , x38
   , x39
   , x40
   , x41
   , x42
   , x43
   , x44
   , x45
   , x46
   , x47
   , x48
   , x49
   , x50
   , x51
   , x52
   , x53
   , x54
   , x55
   , x56
   , x57
   , x58
   , x59
   , x60
   , x61
   , x62
   , x63
   , x64
   , x65
   , x66
   , x67
   , x68
   , x69
   , x70
   , x71 ]
