{-#LANGUAGE Arrows, RankNTypes, ScopedTypeVariables, FlexibleContexts,
  TypeSynonymInstances, NoMonomorphismRestriction, FlexibleInstances #-}

valForm initVal vtor label = withInput $
    proc ((),nm,fi) -> do
      s_curr <- keepState initVal -< fi
      valid <- vtor -< s_curr
      case valid of
         Left err -> returnA -< (textField label (Just err) s_curr nm,
                                                   Nothing)
         Right x -> returnA -< (textField label Nothing s_curr nm,
                                Just x)
