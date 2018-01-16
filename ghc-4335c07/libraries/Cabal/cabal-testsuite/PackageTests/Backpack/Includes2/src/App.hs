module App where
import Database.MySQL
import Database.PostgreSQL
import qualified Mine.MySQL
import qualified Mine.PostgreSQL

app = Mine.MySQL.mine ++ " " ++ Mine.PostgreSQL.mine
