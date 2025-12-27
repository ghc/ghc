module T25901_sub_w3_nc (NameSpace(..), NameScope(..), NameCheck, expect) where

import Language.Haskell.TH (lookupValueName, lookupTypeName, Q)

data NameSpace = TypeName | DataName

data NameScope = InScope | NotInScope

type NameCheck = (NameSpace, NameScope, String)

expect :: NameCheck -> Q ()
expect (nsp, scope, name) = do
  m <- case nsp of
    TypeName -> lookupTypeName  name
    DataName -> lookupValueName name
  case (scope, m) of
    (InScope,    Nothing) -> fail $ name ++ " should be in scope ("     ++ nsStr ++ " namespace) but isn't"
    (NotInScope, Just _)  -> fail $ name ++ " should NOT be in scope (" ++ nsStr ++ " namespace) but is"
    _                     -> return ()
  where nsStr = case nsp of { TypeName -> "type"; DataName -> "data" }
