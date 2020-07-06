{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash, NoImplicitPrelude, TypeFamilies, UnboxedTuples #-}
module
  Main
     (
           main
     ,
       foo
      )
    where

import {-#  SOURCE   #-}   qualified   Data.List  as  L
import  Data.Map  hiding    ( Map(..)  )

main    =
    putStrLn     "hello"

foo = 1


-- | '(' qconsym ')'       {% amsr (sLL $1 $> (unLoc $2))
--                                  [mop $1,mjA AnnVal $2,mcp $3] }
f1 = (    Main.::: )  0    1

-- | '(' consym ')'        {% amsr (sLL $1 $> (unLoc $2))
--                                [mop $1,mjA AnnVal $2,mcp $3] }
f2 = ( :::   )  0   1

-- | '`' conid '`'         {% amsr (sLL $1 $> (unLoc $2))
--                                  [mj AnnBackquote $1,mjA AnnVal $2
--                                  ,mj AnnBackquote $3] }
-- data GG = GG Int Int
-- gg = 0 `  GG ` 1
