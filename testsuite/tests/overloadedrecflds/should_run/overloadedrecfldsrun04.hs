-- Test that DuplicateRecordFields works with TemplateHaskell

{-# LANGUAGE DuplicateRecordFields, TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

-- Splice in a datatype with field...
$(return [DataD [] (mkName "R") [] Nothing
          [RecC (mkName "MkR") [(mkName "foo", NotStrict, ConT ''Int)]] []])

-- New TH story means reify only sees R if we do this:
$(return [])

-- ... and check that we can inspect it
main = do  putStrLn $(do { info <- reify ''R
                         ; case info of
                             TyConI (DataD _ _ _ _ [RecC _ [(n, _, _)]] _) ->
                                 do { info' <- reify n
                                    ; lift (pprint info ++ "\n" ++ pprint info')
                                    }
                             _ -> error "unexpected result of reify"
                         })
           putStrLn $(do { info <- reify 'foo
                         ; case info of
                             VarI n _ _ ->
                                 do { info' <- reify n
                                    ; lift (pprint info ++ "\n" ++ pprint info')
                                    }
                         })
           print (foo (MkR { foo = 42 }))
