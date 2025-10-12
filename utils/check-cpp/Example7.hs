--
-- (C) Susumu Katayama
--
-- (Typed)IOPairs¾å¤Ç¥Ç¡¼¥¿¤ò¤È¤ë¡¥ghci¾å¤Ç :cmd ¤ò»È¤¤¤Þ¤¯¤ë´¶¤¸¡¥
{-# LANGUAGE RankNTypes, GHC_CPP, TemplateHaskell #-}
{-# OPTIONS -ddump-ghc-cpp -dkeep-comments #-}
module Example7(module Example7, module MagicHaskeller.RunAnalytical) where

import MagicHaskeller.Analytical
#ifdef DEBUG
                                 hiding (rev)
#endif
import MagicHaskeller.Classification(Filtrable)
import MagicHaskeller.RunAnalytical
#ifdef DEBUG
                                 hiding (main)
#endif
import MagicHaskeller.GetTime(batchWrite)

main = do iop <- runQ andL
          let e = getOne iop []
          putStrLn $ pprint e

emptyBK = [d| {} |]
