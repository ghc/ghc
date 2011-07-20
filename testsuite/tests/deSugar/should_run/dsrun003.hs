-- Tests match on empty field lists 

module Main where

data Person	=  Female {firstName, lastName :: String}
      		|  Male   {firstName, lastName :: String}
      		deriving (Show)

isFemale (Female{})   =  True
isFemale (Male{})     =  False

main = print (isFemale (Female {firstName = "Jane", lastName = "Smith"}))

