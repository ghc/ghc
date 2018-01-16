module Env (Env, initEnv, lookupEnv, enterEnv) where

import Ast
import BasicNumber

-- environment is a list of variable bindings.
type Env = [(String, BasicExp)]

-- initial environment
initEnv :: Env -> Env
initEnv _ = [("$prec", (Numb 20))]

-- environment lookup
lookupEnv :: String -> Env -> BasicExp
lookupEnv str []     = BSError
lookupEnv str ((s,bexp):es) = if s==str then bexp
					else lookupEnv str es

-- enter a new element to the environment, remove the duplicate if exists.
enterEnv :: String -> BasicExp -> Env -> Env
enterEnv str bexp env = (str, bexp):env1
			where
				env1 = filter (\c@(s,e) -> s/=str) env






