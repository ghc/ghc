{------------------------------------------------------------------------------
                                   KNOWLEDGE

Knowledge, in the form of sentences and phrases with variables in them, is
represented using a tree structure. Simple parsers are provided for rules,
goals, relations and nouns. Functions are provided for converting a text file
into a table of definitions and for accessing the table.
------------------------------------------------------------------------------}

module Knowledge where
import Result
import Table
import Data.List(nub)--1.3

-- The type `Phrase' is a tree-like data structure for storing sentences and
-- phrases. A phrase is usually a term consisting of a word with a list of
-- subphrases. Variables are picked out separately as they have a special role,
-- and a function is provided for extracting a duplicate-free list of the names
-- of the variables in a phrase. Variable names start with capital letters. A
-- single type is used rather than separate types for rules, goals, relations
-- and so on to make it easier to write the matching and searching modules.

data Phrase = Term String [Phrase] | Var String

vars p = nub (names p)  where
   names (Var x) = [x]
   names (Term x ps) = concat [names p | p <- ps]

-- The display function `showPhrase' assumes that the only phrases are
-- variables, nouns, and pairs of subphrases with joining words between them.

showPhrase (Var x) = x
showPhrase (Term x []) = x
showPhrase (Term op [p1,p2]) =
   showPhrase p1 ++ " " ++ op ++ " " ++ showPhrase p2

-- Each parser takes a list of words and returns a Phrase. The parsers for
-- rules, goals and relations involve finding the joining word and the two
-- lists of words on either side with `split', and then parsing the two lists.
-- A rule is a relation and a goal joined by `if'. A goal is a collection of
-- relations joined by `and' and `or', with `and' binding tighter. A relation
-- is two nouns joined by a verb, and a noun is a word, perhaps preceded by
-- `a', `an' or `the' for readability. These parsers are neither very general
-- (no brackets, for instance) nor very efficient, nor do they detect errors.

rule ws = split ws relation "if" goal

goal ws
   | elem "or" ws  = split ws goal "or" goal
   | elem "and" ws = split ws goal "and" goal
   | otherwise     = relation ws

relation ws =
   split ws noun verb noun  where
   verb = head [w | w<-ws, elem w verbs]
   verbs = ["is","describes","has","can","eats"]

noun [a,x] | elem a ["a","an","the"]  =  noun [a++" "++x]
noun [x] | ('A' <= head x) && (head x <= 'Z') = Var x
noun [x]  =  Term x []

split ws f op g =
   Term op [f lhs, g rhs]
   where
   lhs = takeWhile (/=op) ws
   rhs = tail (dropWhile (/=op) ws)

-- The `definitions' function takes a list of text lines and converts it into a
-- table of definitions. Each entry is a verb, together with the rules which
-- are define that verb. Each verb is either completely defined in the table
-- (eg `is', `describes') or is completely undefined so that the user has to be
-- asked (eg `has', `eats'). The `relevant' function extracts from the table
-- the list of rules which are relevant to a given relation.

definitions ls =
   updateList newTable [(v, def v) | v<-verbs] where
   def v = [r | r<-rs, verb r == v]
   verbs = nub [verb r | r<-rs]
   verb (Term "if" [Term v ns, g]) = v
   rs = [rule (words l) | l<-ls]

relevant defs (Term v ns) =
   if fails lookup then [] else answer lookup where
   lookup = find defs v
