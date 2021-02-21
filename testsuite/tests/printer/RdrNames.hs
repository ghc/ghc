{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash, NoImplicitPrelude, TypeFamilies, UnboxedTuples #-}
module RdrNames where

import Data.Monoid

-- ---------------------------------------------------------------------

--        |  'type' qcname            {% amms (mkTypeImpExp (sLL $1 $> (unLoc $2)))
--                                            [mj AnnType $1,mj AnnVal $2] }

-- Tested in DataFamilies.hs

-- ---------------------------------------------------------------------

--        | '(' qconsym ')'       {% ams (sLL $1 $> (unLoc $2))
--                                       [mo $1,mj AnnVal $2,mc $3] }
ff = (RdrNames.:::) 0 1


-- ---------------------------------------------------------------------

--        | '(' consym ')'        {% ams (sLL $1 $> (unLoc $2))
--                                       [mo $1,mj AnnVal $2,mc $3] }
data FF = ( :::  ) Int Int

-- ---------------------------------------------------------------------

--        | '`' conid '`'         {% ams (sLL $1 $> (unLoc $2))
--                                       [mj AnnBackquote $1,mj AnnVal $2
--                                       ,mj AnnBackquote $3] }
data GG = GG Int Int
gg = 0 `  GG ` 1

-- ---------------------------------------------------------------------

--        | '`' varid '`'         {% ams (sLL $1 $> (unLoc $2))
--                                       [mj AnnBackquote $1,mj AnnVal $2
--                                       ,mj AnnBackquote $3] }
vv = "a" ` mappend  ` "b"

-- ---------------------------------------------------------------------

--        | '`' qvarid '`'        {% ams (sLL $1 $> (unLoc $2))
--                                       [mj AnnBackquote $1,mj AnnVal $2
--                                       ,mj AnnBackquote $3] }
vvq = "a" `  Data.Monoid.mappend ` "b"

-- ---------------------------------------------------------------------

--        | '(' ')'                      {% ams (sLL $1 $> $ getRdrName unitTyCon)
--                                              [mo $1,mc $2] }
-- Tested in Vect.hs

-- ---------------------------------------------------------------------

--        | '(#' '#)'                    {% ams (sLL $1 $> $ getRdrName unboxedUnitTyCon)
--                                              [mo $1,mc $2] }
-- Tested in Vect.hs

-- ---------------------------------------------------------------------

--        | '(' commas ')'        {% ams (sLL $1 $> $ getRdrName (tupleTyCon BoxedTuple
--                                                        (snd $2 + 1)))
--                                       (mo $1:mc $3:(mcommas (fst $2))) }
ng :: (, , ,) Int Int Int Int
ng = undefined

-- ---------------------------------------------------------------------

--        | '(#' commas '#)'      {% ams (sLL $1 $> $ getRdrName (tupleTyCon UnboxedTuple
--                                                        (snd $2 + 1)))
--                                       (mo $1:mc $3:(mcommas (fst $2))) }
-- Tested in Unboxed.hs

-- ---------------------------------------------------------------------

--        | '(' '->' ')'          {% ams (sLL $1 $> $ getRdrName funTyCon)
--                                       [mo $1,mj AnnRarrow $2,mc $3] }

ft :: (->) a b
ft = undefined

fp :: (   ->    ) a b
fp = undefined

type family F a :: * -> * -> *
type instance F Int = (->)
type instance F Char = ( ,  )

-- ---------------------------------------------------------------------

--        | '[' ']'               {% ams (sLL $1 $> $ listTyCon_RDR) [mo $1,mc $2] }
lt :: [] a
lt = undefined

-- ---------------------------------------------------------------------

--        | '[:' ':]'             {% ams (sLL $1 $> $ parrTyCon_RDR) [mo $1,mc $2] }

-- GHC source indicates this constuctor is only available in PrelPArr
-- ltp :: [::] a
-- ltp = undefined

-- ---------------------------------------------------------------------

--        | '(' '~#' ')'          {% ams (sLL $1 $> $ getRdrName eqPrimTyCon)
--                                        [mo $1,mj AnnTildehsh $2,mc $3] }

-- primitive type?
-- Refl Int :: ~# * Int Int
-- Refl Maybe :: ~# (* -> *) Maybe Maybe

-- | A data constructor used to box up all unlifted equalities
--
-- The type constructor is special in that GHC pretends that it
-- has kind (? -> ? -> Fact) rather than (* -> * -> *)
data (~) a b = Eq# ((~#) a b)
data (  ~ ) a b = Eq# (( ~#  ) a b)

data Coercible a b = MkCoercible ((~#) a b)


-- ---------------------------------------------------------------------

--        | '(' qtyconsym ')'             {% ams (sLL $1 $> (unLoc $2))
--                                               [mo $1,mj AnnVal $2,mc $3] }
-- TBD

-- ---------------------------------------------------------------------

--        | '(' '~' ')'                   {% ams (sLL $1 $> $ eqTyCon_RDR)
--                                               [mo $1,mj AnnTilde $2,mc $3] }

-- ---------------------------------------------------------------------

-- tyvarop : '`' tyvarid '`'       {% ams (sLL $1 $> (unLoc $2))
--                                        [mj AnnBackquote $1,mj AnnVal $2
--                                        ,mj AnnBackquote $3] }

-- ---------------------------------------------------------------------


{- From #haskell-emacs
gracjan> did you know that this is legal haskell:
<gracjan> (+ 1) ` fmap {- -} ` [1,2,3]
-}
xxx = (+ 1) ` fmap {- -} ` [1,2,3]
