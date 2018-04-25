{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module T12133 where

import           GHC.Classes (IP(..))
import           GHC.Exts (Constraint)

-- | From "Data.Constraint":
data Dict :: Constraint -> * where Dict :: a => Dict a

newtype a :- b = Sub (a => Dict b)

infixl 1 \\ -- required comment

(\\) :: a => (b => r) -> (a :- b) -> r
r \\ Sub Dict = r

-- | GHC 7.10.2 type checks this function but GHC 8.0.1 does not unless
-- you modify this example in one of the following ways:
--
--   * uncomments the type signature for 'Sub'
--
--   * flatten the nested pairs of constraints into a triple of constraints
--
--   * replace 'IP sym ty' with 'c9', where 'c9' is a new constraint variable.
--
-- The error message is listed below.
foo :: forall c1 c2 c3 sym ty
    .  (c1, c2) :- c3
    -> (c1, (IP sym ty, c2)) :- (IP sym ty, c3)
foo sp = ( Sub
--           :: ((c1, (IP sym ty, c2)) => Dict (IP sym ty, c3))
--              -> (c1, ((IP sym ty), c2)) :- (IP sym ty, c3)
         )
         ( (Dict \\ sp) :: Dict (IP sym ty, c3) )

{- Compiler error message:

GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling T                ( t.hs, interpreted )

t.hs:44:13: error:
    • Could not deduce: IP sym ty arising from a use of ‘Dict’
      from the context: (c1, (IP sym ty, c2))
        bound by a type expected by the context:
                   (c1, (IP sym ty, c2)) => Dict (IP sym ty, c3)
        at t.hs:(40,10)-(44,49)
      or from: c3
        bound by a type expected by the context:
                   c3 => Dict (IP sym ty, c3)
        at t.hs:44:13-22
    • In the first argument of ‘(\\)’, namely ‘Dict’
      In the first argument of ‘Sub’, namely
        ‘((Dict \\ sp) :: Dict (IP sym ty, c3))’
      In the expression: (Sub) ((Dict \\ sp) :: Dict (IP sym ty, c3))
    • Relevant bindings include
        foo :: (c1, c2) :- c3 -> (c1, (IP sym ty, c2)) :- (IP sym ty, c3)
          (bound at t.hs:40:1)
Failed, modules loaded: none.
-}