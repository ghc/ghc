{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module T23203 where

import Language.Haskell.TH

data D = MkD Int

$(do let -- The original example from #23203
         genPat1 :: Q [Dec]
         genPat1 = sequence [
             patSynD name (prefixPatSyn []) unidir wildP
           , pure $ PragmaD $ InlineP name Inline FunLike AllPhases
           ]
           where name = mkName "A"

         -- A slightly more complicated example that also puts an INLINE pragma
         -- on a field name in a record pattern synonym
         genPat2 :: Q [Dec]
         genPat2 = sequence [
             patSynD con_name (recordPatSyn [fld_name]) implBidir (conP 'MkD [varP fld_name])
           , pure $ PragmaD $ InlineP con_name Inline FunLike AllPhases
           , pure $ PragmaD $ InlineP fld_name Inline FunLike AllPhases
           ]
           where con_name = mkName "P"
                 fld_name = mkName "fld"

     decs1 <- genPat1
     decs2 <- genPat2
     pure (decs1 ++ decs2))
