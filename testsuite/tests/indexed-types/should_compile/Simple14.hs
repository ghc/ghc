{-# LANGUAGE TypeFamilies, RankNTypes, FlexibleContexts, ScopedTypeVariables #-}

module Simple14 where

data EQ_ x y = EQ_

eqE :: EQ_ x y -> (x~y => EQ_ z z) -> p
eqE = error "eqE"

eqI :: EQ_ x x
eqI = error "eqI"

ntI :: (forall p. EQ_ x y -> p) -> EQ_ x y
ntI = error "ntI" 

foo :: forall m n. EQ_ (Maybe m) (Maybe n)
foo = ntI (`eqE` (eqI :: EQ_ m n))
-- Alternative
-- foo = ntI (\eq -> eq `eqE` (eqI :: EQ_ m n))

-- eq :: EQ_ (Maybe m) (Maybe n)
-- Need (Maybe m ~ Maybe n) =>  EQ_ m n ~ EQ_ zeta zeta
-- which redues to (m~n) => m ~ zeta
-- but then we are stuck