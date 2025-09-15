{-# Language TemplateHaskell #-}
module T11046_helper where
import Language.Haskell.TH

check :: String -> Q [Dec]
check x = do mb <- lookupTypeName x
             case mb of
               Nothing -> fail "Bug #11046 is still present."
               Just _  -> return []
