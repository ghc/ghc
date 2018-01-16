{------------------------------------------------------------------------------
				    RESULTS

RESULTS which may succeed or fail can be represented in two ways, both of which
have important advantages. When only one possible answer is expected, a tagged
union allows a value of one type (the answer) to be returned in the case of
success, and a value of a different type (a reason) to be returned in case of
failure.  The Result type is presented here as an (abstract) type with the
following functions:

   success a     creates a value representing a successful result with value a
   succeeds x    tests a result to see if it is successful
   answer x      extracts the answer from a successful result

   failure r     creates a failure value with reason r
   fails x       tests a result to see if it is a failure
   reason x      extracts the reason from a failed result

There is a potential confusion with the constructors Success and Failure which
are used by the IO Response type. The Answer and Reason constructors here are
not intended to be used directly in programs.
------------------------------------------------------------------------------}

module Result where

data Result a r = Answer a | Reason r


success a = Answer a

succeeds (Answer a) = True
succeeds _ = False

answer (Answer a) = a


failure r = Reason r

fails = not . succeeds

reason (Reason r) = r


-- The second representation of results, invaluable for use in search
-- algorithms which may produce many answers, is as a list of successful
-- answers. There is no provision for a reason to be given in the case of
-- failure, which is represented by []. The `answers' function converts from
-- the above representation to the new one, otherwise normal list operations
-- are used (eg `null' to test for failure).


answers (Answer a) = [a]
answers _ = []
