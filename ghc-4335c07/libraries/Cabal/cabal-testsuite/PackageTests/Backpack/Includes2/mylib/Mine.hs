module Mine where
import Database
data Mine = Mine Database
mine = "mine" ++ databaseName
