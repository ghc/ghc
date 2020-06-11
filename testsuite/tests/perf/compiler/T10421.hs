-- Exponential with GHC 8.10

module RegBig where

import Prelude

import Control.Applicative
import T10421_Form
import T10421_Y

data Register
  = Register String
             String
             String
             String
             String
             String
             String
             String
             String
             String
             String
             String

registerForm :: a -> IO (FormResult Register)
registerForm _  = do
  (a1, _) <- mreq textField "" Nothing
  (a2, _) <- mreq textField "" Nothing
  (a3, _) <- mreq textField "" Nothing
  (a4, _) <- mreq textField "" Nothing
  (a5, _) <- mreq textField "" Nothing
  (a6, _) <- mreq textField "" Nothing
  (a7, _) <- mreq textField "" Nothing
  (a8, _) <- mreq textField "" Nothing
  (a9, _) <- mreq textField "" Nothing
  (a10, _) <- mreq textField "" Nothing
  (a11, _) <- mreq textField "" Nothing
  (a12, _) <- mreq textField "" Nothing
  return (Register <$> a1
                   <*> a2
                   <*> a3
                   <*> a4
                   <*> a5
                   <*> a6
                   <*> a7
                   <*> a8
                   <*> a9
                   <*> a10
                   <*> a11
                   <*> a12
      )
