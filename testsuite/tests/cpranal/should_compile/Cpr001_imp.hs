-- $Id: Cpr001_imp.hs,v 1.1 2001/08/22 12:21:15 simonmar Exp $

module Cpr001_imp where

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

data MS		= MS { instr	:: String
		     , pc	:: Int
		     , mem	:: String
		     , stack	:: String
		     , frames	:: [String]
		     , status	:: Maybe String
		     }


newtype StateTrans s a = ST ( s -> (s, Maybe a))

-- state monad with error handling
-- in case of an error, the state remains
-- as it is and Nothing is returned as value
-- else execution continues

instance Functor (StateTrans s) where
    fmap = liftM

instance Applicative (StateTrans s) where
    pure = return
    (<*>) = ap

instance Monad (StateTrans s) where
    (ST p) >>= k
	= ST (\s0 -> let
		     (s1, r0)	= p s0
		     in
		     case r0 of
		     Just v -> let
                               (ST q) = k v
			       in
			       q s1
		     Nothing -> (s1, Nothing)
	     )
    return v
	= ST (\s -> (s, Just v))


-- machine state transitions

type MachineStateTrans	= StateTrans MS

type MST = MachineStateTrans

{-# NOINLINE setMTerminated #-}
setMTerminated
    = ST (\ms -> (ms { status = Just "Terminated" }, Just ()))

setMSvc call
    = ST (\ms -> (ms { status = Just "Service" }, Just ()))

-- -------------------------------------------------------------------

data Instr
    = LoadI		Int		-- load int const
    | SysCall		String  	-- system call (svc)

