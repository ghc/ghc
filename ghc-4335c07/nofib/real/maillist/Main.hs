{- 
		       Mailing List Generator
		       ----------------------
				   
		 Written by Paul Hudak, January 1992.


This program takes an ascii file of the form:

Name1
Address1
City1

Name2
Address2
City2

...

Namen
Addressn
Cityn

where each entry is up to 4 lines each and with at least one blank
line between entries, and writes a new file containing reformatted
entries along with LaTex commands, that, when run through Latex, will
generate a printout of the form:

Name1                       Name2                       Name3
Address1                    Address2                    Address3
City1                       City2                       City3
...

which is suitably spaced to line up with the labels on a standard
8 1/2 X 11, 30-per-page mailing label sheet.

The program prompts the user for the name of the input file, and uses
that name with a ".tex" suffix for the name of the output file.  That
output file may be LaTexed directly, but it expects the following .sty
file, whose name should be "labels.sty":

\documentstyle[11pt]{article}
\textheight=10.5in
\textwidth=9.0in
\topmargin=-1in
\oddsidemargin=-1in
\evensidemargin=-1in
\pagestyle{empty}

\newcommand{\leftspace}{.5in}
\newcommand{\horspace}{0in}
\newcommand{\vertspace}{.23in}

\newcommand{\lpage}[3]
 {\newpage
  \vspace*{.05in}
  \noindent
  #1#2#3}

\newcommand{\sblock}[3]
 {\lline{#1}{#2}{#3}\\}

\newcommand{\lblock}[9]
 {\lline{#1}{#2}{#3}\vspace{\vertspace}\\ 
  \lline{#4}{#5}{#6}\vspace{\vertspace}\\
  \lline{#7}{#8}{#9}\vspace{\vertspace}\\}

\newcommand{\lline}[3]
 {\hspace*{\leftspace}
  \lab{#1}\hspace{\horspace}
  \lab{#2}\hspace{\horspace}
  \lab{#3}}

\newcommand{\lab}[1]
 {\begin{tabular}{p{2.5in}}
  #1
  \end{tabular}}


Desired enhancements: 
---------------------
  allow more than one input file for same output file
  do character conversion for LaTex to avoid having to put "\&", etc. on input

-}


module Main where

import Control.Exception (catch, IOException)

type Line      = String
type Entry     = [Line]
type FileName  = String
type UserInput = [FileName]

maxLineLength = 35 :: Int

main =	do
    putStr "\n\nWelcome to the LaTex Mailing List Generator.\n\
		\(Please type Cntrl-D at file prompt to exit.)\n"
    s <- getContents
    mainLoop (lines s)

mainLoop :: UserInput -> IO ()
mainLoop fns = 
	putStr "\nFile to be converted: " >>
	case fns of
	  []        -> 	putStr "\nGoodbye!\n"
          (fn:fns') ->  catch (readFile fn >>= process (fn ++ ".tex") fns')
                          (\err -> let _ = err :: IOException in
                                   putStr ("\nCan't read " ++fn++ "; try again.\n") >>
				   mainLoop fns')
			

process :: FileName -> UserInput -> String -> IO ()
process out fns rawText = 
	writeFile out "% Latex Mailing List.\n\n\
                      \\\input{labels.sty}\n\n\
                      \\\begin{document}\n\n"     >>
	loop (paras (lines rawText))
	where loop [] = appendFile out "\n\\end{document}\n" >>
			putStr ("\nConversion completed; file " ++out++ " written.\n") >>
                        mainLoop fns
	      loop ps = writePage out ps loop

paras :: [Line] -> [Entry]
paras []  = []
paras lns = p : paras (dropWhile blankLine lns')
	    where (p,lns')  = break blankLine lns
		  blankLine = all (\c -> c==' ' || c=='\t')

writePage :: FileName -> [Entry] -> ([Entry]-> IO ()) -> IO ()
writePage out ps cont =
	appendFile out "\\lpage\n" >>
	writeBlock out ps long  9  >>= \ ps ->
	writeBlock out ps long  9  >>= \ ps ->
	writeBlock out ps long  9  >>= \ ps ->
	writeBlock out ps short 3  >>=
	cont

-- got to here (partain)

long  = "{\\lblock{\n"
short = "{\\sblock{\n"

writeBlock :: FileName -> [Entry] -> String -> Int -> IO [Entry]
writeBlock out ps kind size = 
	appendFile out kind >>
	loop ps 1
	where	loop (e:es) n = 
			writeEntry out e >>
			(if n==size then appendFile out "\n}}\n" >>
					 return es
				    else appendFile out "\n}{\n" >>
					 loop es (n+1) )
		loop [] n = loop (take (size-n+1) (repeat [])) n

writeEntry :: FileName -> Entry -> IO ()
writeEntry out entry = loop entry 1  where
  loop [] n =
	if n<5 then loop (take (5-n) (repeat "")) n
	       else return ()
  loop (ln:lns) n = 
	if n>4 
	then putStr
               "\nThis entry was truncated to 4 lines:\n" >>
	     print entry >>
	     putStr "\n" >>
	     return ()
	else appendFile out ln >>
	     appendFile out "\\\\ " >>
	     (if length ln>maxLineLength
	      then putStr "\nThis line may be too long:\n" >>
		   putStr ln >>
		   putStr "\nCheck LaTex output to be sure.\n" >>
		   loop lns (n+1)
	      else loop lns (n+1) )
