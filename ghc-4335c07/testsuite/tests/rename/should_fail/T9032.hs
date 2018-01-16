{-# LANGUAGE CPP #-}

module T9032 where

#ifdef ERR
import T9032
#endif

f x = x



