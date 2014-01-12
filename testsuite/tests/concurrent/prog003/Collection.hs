{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Collection where


class Show e => Col c e | c -> e where
   newCol :: IO c
   insertCol :: c -> e -> IO ()
   deleteCol :: c -> e -> IO Bool
   findCol :: c -> e -> IO Bool
   printCol :: c -> IO ()
   cntCol :: c -> IO Int
