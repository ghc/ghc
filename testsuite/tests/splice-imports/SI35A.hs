{-# LANGUAGE TemplateHaskellQuotes #-}
module SI35A where

-- Define a type for use in Template Haskell
data T = MkT Int

-- Helper function to construct a T
mkT :: Int -> T
mkT = MkT

-- A function that creates a quoted expression using T
quotedT :: Int -> Q Exp
quotedT n = [| mkT n |]

-- Another quoted expression function
quotedAdd :: Q Exp
quotedAdd = [| \x y -> x + y :: Int |]

-- Show instance
instance Show T where
  show (MkT n) = "MkT " ++ show n