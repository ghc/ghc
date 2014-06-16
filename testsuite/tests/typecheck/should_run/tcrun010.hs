{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

-- !!! Functional dependencies
-- This one gave "zonkIdOcc:  FunDep_a11w" in earlier days

module Main (main) where

data ERR a b = EOK a | ERR b deriving (Show)
data Error   = No | Notatall deriving (Show, Eq)


class MonadErr m e | m -> e where
   aerturn ::           e   -> m a
   areturn ::           a   -> m a
   acatch  ::           a   -> (a -> m b) -> (e -> m b) -> m b
   (>>>=)  ::           m a -> (a -> m b) -> m b
   (>>>)   ::           m a -> m b        -> m b

data BP a = BP (Int -> (ERR a Error, Int))

instance MonadErr BP Error where
   aerturn k             = BP $ \s  -> (ERR k, s)
   areturn k             = BP $ \s  -> (EOK k, s)
   acatch  k try handler = BP $ \s  -> let BP try'     = try  k
                                           (r,s1)      = try' s
                                           (BP c2, s2) = case r of
                                                           EOK r -> (areturn r, s1)
                                                           ERR r -> (handler r, s)
                                       in  c2 s2
   a >>> b =  a >>>= \_ -> b

   (BP c1) >>>= fc2      = BP $ \s0 -> let (r,s1) = c1 s0
                                           BP c2 = case r of
                                                     EOK r -> fc2 r
                                                     ERR r -> BP (\s -> (ERR r, s))
                                       in c2 s1

run_BP :: Int -> BP a -> (ERR a Error, Int)
run_BP st (BP bp) = bp st

foo :: (ERR Int Error, Int)
foo = run_BP 111 (aerturn No)

main = print (show foo)
