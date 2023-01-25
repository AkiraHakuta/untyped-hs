module Parser where

import Control.Applicative ((<|>))
import MonadicParsing
import Syntax


command :: Context -> Parser Command
command ctx = (bindCommand ctx) <|> (evalCommand ctx)

bindCommand :: Context -> Parser Command
bindCommand ctx = 
    do x <- identifier
       bind <- (eqTermBinder ctx) <|> (colonEqTermBinder ctx) <|> (slashBinder ctx)
       return (Bind x bind)

eqTermBinder :: Context -> Parser Binding
eqTermBinder ctx = 
    do symbol "="
       t <- term ctx 
       return (EqTmBind (t))

colonEqTermBinder :: Context -> Parser Binding
colonEqTermBinder ctx = 
    do symbol ":="
       t <- term ctx 
       return (ColonEqTmBind (t))

slashBinder :: Context -> Parser Binding
slashBinder ctx = do symbol "/"
                     return NameBind

evalCommand :: Context -> Parser Command
evalCommand ctx = 
    do t <- term ctx
       return (Eval t)

term :: Context -> Parser Term
term ctx = (appTerm ctx) <|> (absTerm ctx) <|> (absUsTerm ctx)

absTerm :: Context -> Parser Term
absTerm ctx = do{symbol "\\"; x <- identifier;
                 symbol "."; t <- term (addname ctx x);
                 return (TmAbs x t)} 

absUsTerm :: Context -> Parser Term
absUsTerm ctx = do{symbol "\\"; symbol "_";
                 symbol "."; t <- term (addname ctx "_");
                 return (TmAbs "_" t)} 

appTerm :: Context -> Parser Term
appTerm ctx = chainl1 (aTerm ctx)  $ return (\t1 t2 -> TmApp t1 t2) 

aTerm :: Context -> Parser Term
aTerm ctx = do {symbol "("; t <- term ctx; symbol ")"; return t} <|> tmVar ctx        

tmVar :: Context -> Parser Term
tmVar ctx = do {x <- identifier; 
            idx <- deBruijnIndex x ctx;
            return(TmVar idx)}
