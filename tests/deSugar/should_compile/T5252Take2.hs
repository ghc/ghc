module T5252Take2 where
import qualified T5252Take2a as M

write_message :: M.WriteMessage -> IO Bool
write_message (M.WriteMessage _) = return True
