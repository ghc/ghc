-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Compositor
-- Copyright   :  (c) Ashley Yakeley 2007
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ashley@semantic.org
-- Stability   :  experimental
-- Portability :  portable

module Control.Compositor where

infixr 1 >>>, <<<

class Compositor comp where
	identity :: comp a a

	-- | Left-to-right composition
	(>>>) :: comp a b -> comp b c -> comp a c

{-# RULES
"identity/left"	forall p .
		identity >>> p = p
"identity/right"	forall p .
		p >>> identity = p
"association"	forall p q r .
		(p >>> q) >>> r = p >>> (q >>> r)
 #-}

instance Compositor (->) where
	identity = id
	p >>> q = q . p

-- | Right-to-left composition
(<<<) :: Compositor comp => comp b c -> comp a b -> comp a c
f <<< g = g >>> f

