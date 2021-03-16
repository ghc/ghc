{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -O2 #-}

-- | Updater that doesn't lose sharing
--
-- This module provides a DSL:
--
--  value_to_update <~~ constructor
--                  <~$ first updated arg
--                  <~* second updated arg
--                  <~* etc.
--
-- The following example:
--
--  data T = C0 Int | C1 T T | C2 T T T
--  
--  foo :: T -> T
--  foo = runUpd go
--    where
--      go t = case t of
--        C0 x
--          | x == 0    -> Update (C0 (x + 999))
--          | otherwise -> NoUpdate t
--        C1 x y   -> t <~~ C1 <~$ go x <~* go y
--        C2 x y z -> t <~~ C2 <~$ go x <~* go y <~* go z
--
-- is compiled into the following STG:
--
--  foo_go :: T -> Upd T
--  [GblId, Arity=1, Str=<1L>, Unf=OtherCon []] =
--      {} \r [t_sQZ]
--          case t_sQZ of wild_sR0 [Occ=Once3] {
--            C0 x_sR1 [Occ=Once1!] ->
--                case x_sR1 of {
--                GHC.Types.I# x1_sR3 [Occ=Once1!] ->
--                case x1_sR3 of {
--                  __DEFAULT -> (#,#) [0# wild_sR0];
--                  0# -> (#,#) [1# lvl1_rOn];
--                };
--                };
--            C1 x_sR5 [Occ=Once1] y_sR6 [Occ=Once1] ->
--                case foo_go y_sR6 of {
--                (#,#) ipv_sR8 [Occ=Once1] ipv1_sR9 [Occ=Once1] ->
--                case foo_go x_sR5 of {
--                (#,#) ipv2_sRb [Occ=Once1] ipv3_sRc [Occ=Once1] ->
--                case orI# [ipv2_sRb ipv_sR8] of {
--                  __DEFAULT ->
--                      let {
--                        sat_sRe [Occ=Once1] :: T
--                        [LclId] =
--                            CCCS C1! [ipv3_sRc ipv1_sR9];
--                      } in  (#,#) [1# sat_sRe];
--                  0# -> (#,#) [0# wild_sR0];
--                };
--                };
--                };
--            C2 x_sRf [Occ=Once1] y_sRg [Occ=Once1] z_sRh [Occ=Once1] ->
--                case foo_go z_sRh of {
--                (#,#) ipv_sRj [Occ=Once1] ipv1_sRk [Occ=Once1] ->
--                case foo_go y_sRg of {
--                (#,#) ipv2_sRm [Occ=Once1] ipv3_sRn [Occ=Once1] ->
--                case foo_go x_sRf of {
--                (#,#) ipv4_sRp [Occ=Once1] ipv5_sRq [Occ=Once1] ->
--                case orI# [ipv4_sRp ipv2_sRm] of sat_sRr [Occ=Once1] {
--                __DEFAULT ->
--                case orI# [sat_sRr ipv_sRj] of {
--                  __DEFAULT ->
--                      let {
--                        sat_sRt [Occ=Once1] :: T
--                        [LclId] =
--                            CCCS C2! [ipv5_sRq ipv3_sRn ipv1_sRk];
--                      } in  (#,#) [1# sat_sRt];
--                  0# -> (#,#) [0# wild_sR0];
--                };
--                };
--                };
--                };
--                };
--          };
--
--  foo :: T -> T
--  [GblId, Arity=1, Str=<SL>, Unf=OtherCon []] =
--      {} \r [a1_sRu]
--          case foo_go a1_sRu of {
--          (#,#) ipv_sRw [Occ=Once1!] ipv1_sRx [Occ=Once1] ->
--          case ipv_sRw of {
--            __DEFAULT -> ipv1_sRx;
--            0# -> a1_sRu;
--          };
--          };
--
-- Notice that in this example, C1 and C2 are only allocated when strictly
-- necessary, otherwise the old value (`wild_sR0`) is reused and no allocation
-- occurs.

module GHC.Utils.Update
  ( Upd(Update,NoUpdate,StrictUpdate)
  , unUpd
  , runUpd
  , update
  , bindUpd
  , updateList
  , updateListAccumR
  , updateListAccumL
  , (<~$)
  , (<~*)
  , (<~~)
  )
where

import GHC.Exts (Int#, orI#, (==#), (/=#))

-- | Updater (unlifted)
--
-- First value is a tag: 0 = NoUpdate, 1 = Update
newtype Upd a = Upd (# Int#, a #)

-- | Extract value from Upd
unUpd :: Upd a -> a
unUpd (Upd (# _, a #)) = a

-- | Apply an updating function to a given value
runUpd :: (a -> Upd a) -> a -> a
{-# INLINE runUpd #-}
runUpd upd_a a = case upd_a a of
  NoUpdate _ -> a
  Update a'  -> a'

-- Note [Why ViewPatterns?]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We use ViewPatterns of the form (==# 0#) and (/=# 0#) for NoUpdate and Update
-- respectively to allow the following code:
--
--    case ... of
--      NoUpdate a -> ...
--      Update   a -> ...
--
-- to be compiled into a case with only two branches:
--    
--    case ... of
--      __DEFAULT -> ...
--      0#        -> ...
--
-- If instead we directly matched on 0# and 1# we would get:
--
--    case ... of
--      __DEFAULT -> error inserted by ghc
--      0#        -> ...
--      1#        -> ...

{-# COMPLETE NoUpdate, Update #-}
{-# COMPLETE NoUpdate, StrictUpdate #-}
pattern NoUpdate :: a -> Upd a
pattern NoUpdate a <- Upd (# (==# 0#) -> 1#, a #) -- See Note [Why ViewPatterns?]
  where
    NoUpdate a = Upd (# 0#, a #)

pattern Update :: a -> Upd a
pattern Update a <- Upd (# (/=# 0#) -> 1#, a #) -- See Note [Why ViewPatterns?]
  where
    Update a = Upd (# 1#, a #)

pattern StrictUpdate :: a -> Upd a
pattern StrictUpdate a <- Update a
  where
    StrictUpdate a = let !b = a in Update b

bindUpd :: Upd a -> (a -> Upd b) -> Upd b
bindUpd (NoUpdate a) f = f a
bindUpd (Update   a) f = case f a of
  NoUpdate b -> Update b
  Update   b -> Update b


(<~$) :: (a -> b) -> Upd a -> Upd b
{-# INLINE (<~$) #-}
(<~$) f (Upd (# ta, a #)) = Upd (# ta, f a #)

infixl 4 <~$

(<~*) :: Upd (a -> b) -> Upd a -> Upd b
{-# INLINE (<~*) #-}
(<~*) (Upd (# tf, f #)) (Upd (# ta, a #)) = Upd (# tf `orI#` ta, f a #)

infixl 4 <~*

(<~~) :: a -> Upd a -> Upd a
{-# INLINE (<~~) #-}
(<~~) a ma = update a ma

infixr 0 <~~

update :: a -> Upd a -> Upd a
{-# INLINE update #-}
update a ma = case ma of
  NoUpdate _ -> NoUpdate a
  _          -> ma




-- | Update a list. Share the non-updated trail as much as possible.
{-# INLINE updateList #-}
updateList :: forall a. (a -> Upd a) -> [a] -> Upd [a]
updateList = go
  where
    go :: (a -> Upd a) -> [a] -> Upd [a]
    go _ []       = NoUpdate []
    go f t@(a:as) = t <~~ (:) <~$ f a <~* go f as

-- | Update a list with an accumulator. Share the non-updated trail as much as
-- possible.
{-# INLINE updateListAccumR #-}
updateListAccumR :: forall a env. (env -> a -> (# env, Upd a #)) -> env -> [a] -> (# env, Upd [a] #)
updateListAccumR = go
  where
    go :: (env -> a -> (# env, Upd a #)) -> env -> [a] -> (# env, Upd [a] #)
    go _ env []       = (# env, NoUpdate [] #)
    go f env t@(a:as) = (# env'', t <~~ (:) <~$ a' <~* as' #)
      where
        !(# !env', !as' #) = go f env as
        !(# !env'', !a' #) = f env' a

-- | Update a list with an accumulator. Share the non-updated trail as much as
-- possible.
{-# INLINE updateListAccumL #-}
updateListAccumL :: forall a env. (env -> a -> (# env, Upd a #)) -> env -> [a] -> (# env, Upd [a] #)
updateListAccumL = go
  where
    go :: (env -> a -> (# env, Upd a #)) -> env -> [a] -> (# env, Upd [a] #)
    go _ env []       = (# env, NoUpdate [] #)
    go f env t@(a:as) = (# env'', t <~~ (:) <~$ a' <~* as' #)
      where
        !(# !env'', !as' #) = go f env' as
        !(# !env', !a' #)   = f env a
