module Test24754 where

eh1  =  try (do return r;) <|> (do
                return r)
