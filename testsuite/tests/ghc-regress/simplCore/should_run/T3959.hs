{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
module Main(main,f) where

import Data.List (isPrefixOf)
import Data.Dynamic
import Control.Exception as E

data Failure = Failure
    deriving (Show, Typeable)

instance Exception Failure

test = (E.throw Failure >> return ())
          `E.catch` 
       (\(x::Failure) -> return ())

main :: IO ()
main = print =<< test

f :: Bool -> Bool -> Bool
f True = error "urk"
-- f False = \y -> y

{-
Uderlying cause: we call
	  catch# thing handler
and expect that (thing state-token) will
    - either diverge/throw an exception
    - or return (# x,y #)
But it does neither: it returns a PAP, because
    thing = \p q. blah

In particular, 'thing = lvl_sxo' is 
  lvl_sxc :: IO Any
  lvl_sxc = error "urk"

  lvl_sxo :: IO ()
    = lvl_sxc >> return ()

          -- inline (>>) --

    = (\(eta::S#). case lvl_sxc |> g1 eta of ...) |> g2
    where 
      g1 :: IO Any ~ S# -> (# S#, Any #)
      g2 :: S# -> (# S#, () #) -> IO ()

          -- case-of-bottomming function --

    = (\ (eta::S#). lvl_sxc |> g1 |> ug3) |> g2
    where 
      ug3(unsafe) :: S# -> (S#, Any) ~ (# S#, () #)

This is all fine.  But it's crucial that lvl_sxc actually diverges.
Do not eta-expand it to

   lvl_sxc :: IO Any
   lvl_sxc = \eta. error "urk" |> ug4
       where
         ug4(unsafe) :: S# -> (# S#, Any #) ~ IO Any

In contrast, if we had
   case x of
     True  -> \a -> 3
     False -> error "urk"
we can, and must, eta-expand the error

-}