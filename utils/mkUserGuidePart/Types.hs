module Types where

data FlagType = DynamicFlag
                -- ^ Dynamic flag
              | DynamicSettableFlag
                -- ^ Dynamic flag on which @:set@ can be used in GHCi
              | ModeFlag
                -- ^ A mode of execution (e.g. @--mode@)

data Flag = Flag { flagName :: String
                 , flagDescription :: String
                 , flagType :: FlagType
                 , flagReverse :: String
                 , flagSince :: String
                 }

flag :: Flag
flag = Flag "" "" DynamicFlag "" ""
