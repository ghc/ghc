-- !!! ds030: checks that types substituted into binders
--
module Test where

f x = case x of [] -> (3::Int) ; _ -> (4::Int)
