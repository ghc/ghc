module Main (main) where

import System.Environment

import LrcPrelude
import Funcs_Parser_Lazy

import Data_Lazy
import Visfun_Lazy


-- runSemantics :: String -> [BibEntry]
runSemantics inp pw = lrcEval (runParser inp) pw


runEval fn pw
  = do s <- readFile fn
       let (code,errors,te,pp) = runSemantics s pw
       
       putStrLn "Pretty Printed Input:"    
       putStrLn pp
       putStrLn "MSP Generated Code:"
       putStrLn (showCode code)

       putStrLn "Detected Semantic Errors:"
       putStrLn (show errors)
       putStrLn (show te)     

--       putStrLn pp_code
       return ()

main :: IO ()
main = do args <- getArgs 
          putStrLn (show args)
          let fn = head args
          let pw = mytoint (head . tail $ args)
          runEval fn pw

mytoint :: String -> Integer
mytoint s = read s


showCode []     = "\n"
showCode (x:xs) = (showInstr x) ++ "\n" ++ (showCode xs)

showInstr (C_ALabel_1 n) = (show n) ++ ":"
showInstr C_Add_1        = "ADD"
showInstr C_And_1        = "AND"
showInstr (C_Call_1 n)   = "CALL " ++ (showName n)
showInstr C_Cod_1        = "CODIGO"
showInstr C_Data_1       = "MEMORIA DE DADOS"
showInstr C_Div_1        = "DIV"
showInstr C_Eq_1         = "EQ"
showInstr C_Gt_1         = "GT"
showInstr C_Halt_1       = "HALT"
showInstr C_IIn_1        = "IN"
showInstr C_IOut_1       = "OUT"
showInstr (C_Jump_1 n)   = "JMP " ++ (showName n)
showInstr (C_Jumpf_1 n)  = "JMPF " ++  (showName n)
showInstr C_Load_1       = "LOAD"
showInstr C_Lt_1         = "LT"
showInstr C_Minus_1      = "MIN"
showInstr C_Mul_1        = "MUL"
showInstr C_Neq_1        = "NEQ"
showInstr C_Not_1        = "NOT"
showInstr C_Or_1         = "OR"
showInstr (C_Pusha_1 n i) =  "PUSHa " ++ (showName n) ++ " " ++ (show i)
showInstr (C_Pushb_1 b)  = "PUSHb " ++ (show b)
showInstr (C_Pushi_1 i)  = "PUSHi " ++ (show i)
showInstr (C_Pushr_1 r)  = "PUSHr " ++ (show r)
showInstr C_Ret_1        = "RET"
showInstr C_Store_1      = "STORE"
showInstr C_Sub_1        = "SUB"
showInstr (C_Var_1 n i t) = (showName n) ++ "  TAM " ++ (show i)


showName (C_Ident_1 n) = show n
