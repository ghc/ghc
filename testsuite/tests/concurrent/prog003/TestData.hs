
module TestData where

-- Test Data Data Type

-- Operation Tokens
data Op a = Find a | Delete a | Insert a deriving Show

-- A test Data.
--    t_name : Name of the test data.
--    t_threads : Number of concurrent threads to run
--    t_modes   : Concurrent modes selected. Multiple modes
--               will be batch tested in specified sequence
--    t_repeats : Number of runs to conduct on each mode.
--    t_init_list : Elements of the initial list
--    t_tasks : Sequence of operation each node
--              executes. Note this must correspond to
--              number of threads. 
data TestData a = TestData { t_name      :: String
                           , t_threads   :: Int
                           , t_init_list :: [a]
                           , t_tasks     :: [[Op a]] }         
 
instance Show a => Show (TestData a) where
  show tc = "Name: " ++ (t_name tc) ++ "\n" ++
            "Threads: " ++ (show $ t_threads tc) ++ "\n" ++
            "Initial-List:\n" ++ (show $ t_init_list tc) ++ "\n" ++
            "Tasks:\n" ++ (printOps $ t_tasks tc) 
            where
              printOps (op:ops) = (show op) ++ "\n" ++ (printOps ops)
              printOps [] = ""
              
