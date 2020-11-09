{-# LANGUAGE PostfixOperators #-}

import Control.Exception

data MyException = MyE
  deriving (Show)

instance Exception MyException

(#) :: Bool -> Bool -> Bool
(#) = throw MyE

main = do
  r <- try (evaluate (seq (True #) ()))
  case r of
    Left MyE -> putStrLn "PostfixOperators ok"
    Right () -> putStrLn "PostfixOperators broken"
