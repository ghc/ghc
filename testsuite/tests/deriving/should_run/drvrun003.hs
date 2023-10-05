-- !!! Deriving Show/Read for nullary constructors.
module Main(main) where

data A = B | C deriving ( Show, Read )

data Opt = N | Y A deriving (Show, Read)

x = Y B

{-
 If the Haskell report's specification of how Show instances
 are to be derived is followed to the letter, the code for
 a nullary constructor would put parens around the constructor
 when (showsPrec 10) is used. This would cause

      Y A

 to be showed as
 
      Y (A)

 Overkill, so ghc's derived Show code treats nullary
 constructors specially.
-}

main = do
  print x
  print ((read (show x))::Opt)
  print ((read "Y (B)")::Opt)

