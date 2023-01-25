module Syntax where

import Data.List (elemIndex)
import MonadicParsing

-- data type
data Term = TmVar Int
          | TmAbs String Term
          | TmApp Term Term
          deriving Show

data Binding = NameBind
             | EqTmBind Term
             | ColonEqTmBind Term
             deriving Show

type Context = [(String, Binding)] 

data Command = Eval Term 
             | Bind String Binding 
             deriving Show


-- Context management
isnamebound :: Context -> String -> Bool  
isnamebound ctx x = 
    case ctx of 
        [] -> False
        (y, _):rest -> if y == x then True else isnamebound rest x

pickfreshname :: Context -> String -> (Context, String)
pickfreshname ctx x =
    if isnamebound ctx x then pickfreshname ctx (x++"'") else (((x, NameBind):ctx), x)

index2name :: Context -> Int -> String
index2name ctx idx = 
    if (0 <= idx && idx < length ctx) then (map fst ctx)!!idx 
                else error  "Variable lookup failure :deBruijnIndex=" ++ (show idx)

addname :: Context -> String -> Context
addname ctx x = (x, NameBind):ctx 

deBruijnIndex :: String -> Context -> Parser Int
deBruijnIndex var ctx =
  case elemIndex var (map fst ctx) of
    Just i  -> return i
    Nothing -> error $ "The variable " ++ var ++ " has not been bound"

addbinding :: Context -> String -> Binding -> Context
addbinding ctx x bind = (x, bind):ctx


-- Printing
showTerm :: Context -> Term -> String
showTerm ctx (TmVar n)     = index2name ctx n
showTerm ctx (TmAbs x t1)  = let (ctx', x') = pickfreshname ctx x
                                  in  "(\\" ++ x' ++ "." ++ showTerm ctx' t1 ++ ")"
showTerm ctx (TmApp t1 t2) = "(" ++ showTerm ctx t1 ++ " " ++ showTerm ctx t2 ++ ")"

showBind :: Context -> Binding -> String
showBind ctx NameBind          = "/"
showBind ctx (EqTmBind t)      = " = " ++ (showTerm ctx t) 
showBind ctx (ColonEqTmBind t) = " := " ++ (showTerm ctx t)    
