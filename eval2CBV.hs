module Eval2CBV where

import Syntax
import Core

--Call-by-Value Reduction
isVal :: Term -> Bool
isVal (TmAbs _ _) = True
isVal (TmVar _)   = True
isVal  _          = False 

eval1 :: Context -> Term -> Maybe Term 
eval1 ctx (TmVar n)                             =  case getbinding ctx n of 
                                                         Just (EqTmBind t)      -> return t
                                                         Just (ColonEqTmBind t) -> return t
                                                         _                      -> Nothing   
eval1 ctx (TmApp (TmAbs _ t12) t2) | isVal t2 = return$ termSubstTop t2 t12
eval1 ctx (TmApp t1 t2) |isVal t1             = do {t2' <- eval1 ctx t2; return (TmApp t1 t2')}
eval1 ctx (TmApp t1 t2)                       = do {t1' <- eval1 ctx t1; return (TmApp t1' t2)}
eval1 ctx _                                   = Nothing


eval :: Context -> Term -> Term
eval ctx t =
  case eval1 ctx t of
    Just t' -> eval ctx t'
    Nothing -> t
