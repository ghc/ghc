-- Exponential with GHC 8.10
--
-- This is a smaller version of T10421, but demonstrates the same blow-up

module RegBig where

import Prelude

import Control.Applicative
import T10421a_Form

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

registerForm :: FormResult String   -- a1
             -> FormResult String
             -> FormResult String   -- a3
             -> FormResult String
             -> FormResult String
             -> FormResult String   -- a6
             -> FormResult String   -- a7
             -> FormResult String
             -> FormResult String
             -> FormResult String
             -> FormResult String
             -> FormResult String   -- a12
             -> IO (FormResult Register)

registerForm a1 a2 a3 a4 a5 a6 a7
             a8 a9 a10 a11 a12
 = return (Register <$> a1
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
