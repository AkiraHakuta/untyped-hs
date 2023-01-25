module Core where

import Syntax

--  Shifting
termShift :: Int -> Term -> Term
termShift d t  = walk 0 t
    where walk c (TmVar x)     = if  x >= c then TmVar (x+d) else  TmVar x
          walk c (TmAbs x t2)  = TmAbs x (walk (c+1) t2) 
          walk c (TmApp t1 t2) = TmApp (walk c t1) (walk c t2)

bindingshift :: Int -> Binding -> Binding
bindingshift _ NameBind = NameBind
bindingshift d (EqTmBind t) = EqTmBind (termShift d t) 
bindingshift d (ColonEqTmBind t) = ColonEqTmBind (termShift d t)   


-- Substitution
termSubst :: Int -> Term -> Term -> Term
termSubst j s t = walk 0 t
    where walk c (TmVar x)     = if  x == j + c then  termShift c s else TmVar x 
          walk c (TmAbs x t2)  = TmAbs x (walk (c+1) t2) 
          walk c (TmApp t1 t2)  = TmApp (walk c t1) (walk c t2)

termSubstTop :: Term -> Term -> Term
termSubstTop s t = termShift (-1) (termSubst 0 (termShift 1 s) t)

getbinding :: Context -> Int -> Maybe Binding
getbinding ctx i = if (0 <= i  && i < length ctx) 
                        then let (_, bind) = (ctx !! i) in return $ bindingshift (i+1) bind
                        else Nothing






