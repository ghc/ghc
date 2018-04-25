{------------------------------------------------------------------------------
                                   SEARCHING

The `solve' function is the logical inference mechanism which allows the expert
system to search for solutions to goals, by making deductions from the stored
definitions and from the answers to the questions which it asks the user. This
is essentially the same as the inference mechanism which is built into logic
programming languages, with two main differences.  The first is that the search
algorithm has to be programmed explicitly, and the second is that interaction
with the user cannot be handled as a side effect; questions are returned as
part of the result, and answers are fed in as part of the argument.
------------------------------------------------------------------------------}

module Search where
import Result
import Table
import Knowledge
import Match

-- A call to `solve' returns a list of solutions and questions of type
-- `Solution'. Each solution will be preceded by the questions to which `solve'
-- needs answers in order to form that solution, and the answers to these
-- questions are passed to `solve' in its database argument. A solution
-- consists of an environment giving information about variables, and a list of
-- variable names which are not mentioned in the environment and are therefore
-- available for general use. In particular, the search procedure often calls
-- for a copy of a goal to be made using fresh variables, and the `freshCopy'
-- function performs this, returning a modified solution along with the copy.

data Solution = Soln Environment [String] | Question String

freshCopy (Soln env vs) p =
   ((Soln env (drop n vs)), subst tab p) where
   tab = updateList newTable (zip xs [Var v | v <- take n vs])
   xs = vars p
   n = length xs

-- The arguments to `solve' are: a database of stored definitions and
-- information gained from answers to questions, a partial solution
-- representing the information gained about variables so far in the search,
-- and a goal to be satisfied. The first equation allows questions which are
-- generated deep within the search to be passed up and out in the main
-- solution stream. Compound goals are solved by solving the two subgoals and
-- combining the solutions. In the case of `and', information gained in each
-- solution to the first subgoal is used in solving the second. A simple goal
-- (a relation) is solved either by consulting the stored definitions, or by
-- asking the user a question, depending on the verb in that relation.

solve db (Question q) g = [Question q]

solve db soln (Term "or" [g1,g2]) =
   solve db soln g1 ++ solve db soln g2

solve db soln (Term "and" [g1,g2]) =
   concat [solve db res g2 | res <- solve db soln g1]

solve db soln g =
   if not (null rs) then lookUp db soln g rs else ask info soln g
   where
   (defs,info) = db
   rs = relevant defs g

-- To `lookUp' a simple goal using the list of rules `rs', a fresh copy of each
-- rule is made (to avoid name clashes with variables about which information
-- is already known), and `try' is used to see if the left hand side of the
-- rule matches the goal. If it does, the goal on the right hand side of the
-- rule is used to continue the search for solutions.

lookUp db soln g rs =
   concat [try db soln' g r' | (soln',r') <- copies] where
   copies = [freshCopy soln r | r<-rs]

try db (Soln env vs) g (Term "if" [p,newg]) =
   if fails m then [] else solve db (Soln (answer m) vs) newg
   where
   m = match env g p

-- If the solver must ask a question then that question is returned in the list
-- of solutions. The answer is then looked up in the table `info' of
-- questions-and-answers passed as an argument. If the answer is `yes', then
-- the current partial solution is returned. This assumes that questions
-- contain no variables, eg `the animal has stripes?'. Note that, as with other
-- interactive i/o functions, `ask' must return the question before testing the
-- answer.

ask info (Soln env vs) g =
   Question sp :
   if ans then [Soln env vs] else [] where
   ans = answer (find info sp)
   sp  = showPhrase (subst env g)
	-- SLPJ Nov 99
	-- I've hauled out sp as a common sub expression; it was
	-- duplicated before.  If we don't haul it out, it's a matter
	-- of chance whether GHC spots it or not, and that makes the
	-- numbers wobble around a lot.
