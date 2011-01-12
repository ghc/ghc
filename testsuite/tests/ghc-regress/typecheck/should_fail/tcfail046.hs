-- !! function types in deriving Eq things
-- From a bug report by Dave Harrison <D.A.Harrison@newcastle.ac.uk>

module ShouldFail where


type    Process a = Pid -> Time -> Message a -> ( MessList a,
			     		   	  Continuation a)

data    Continuation a = Do (Process a) deriving Eq


type 	ProcList a = [ (Pid, Status, Process a) ]
data 	Status     = Active | Passive | Busy Integer | Terminated
		     deriving Eq


data Message a = Create (Process a) | Created Pid   | Activate Pid  | 
	      	 Passivate Pid      | Terminate Pid | Wait Pid Time | 
		 Query Pid a        | Data Pid a    | Event         |
	      	 Output Pid String
               deriving Eq

type 	MessList a = [ Message a ]

type	Pid  = Integer
type 	Time = Integer
