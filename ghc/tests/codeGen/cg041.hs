{- 
Date: Thu, 15 May 1997 14:20:29 +0100 (BST)
From: Alex Ferguson <abf@cs.ucc.ie>
The following erroneous fragment erroneously compiles.

And then promptly falls over in the assembler, of all places.
-}

data Token
     =  TokNewline
     |  TokLiteral
     |  TokCount
     |  TokCheck
     |  TokIs
     |  TokDeref
     |  TokFind
     |  TokLiteral		-- Duplicated!
     |  TokThe

      deriving Show

main = print TokCount
