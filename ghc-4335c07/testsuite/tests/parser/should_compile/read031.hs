-- !!! "--" can start a legal lexeme 

module ShouldCompile where

infix 2 --+, -->

ps  -->  True   = True

(--+) a b = a && b 

