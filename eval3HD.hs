module Eval3HD where

import Syntax
import Core

-- Head Reduction
eval1 :: Context -> Term -> Maybe Term
eval1 ctx (TmVar n)                 = case getbinding ctx n of 
                                                  Just (EqTmBind t)      -> return t
                                                  Just (ColonEqTmBind t) -> return t
                                                  _                      -> Nothing

eval1 ctx (TmAbs v1 t2)             = do {t2' <- eval1 (addname ctx v1) t2; return (TmAbs v1 t2')}      
eval1 ctx (TmApp (TmAbs _ t12) t2)  = return $ termSubstTop t2 t12     
eval1 ctx (TmApp t1 t2)             = do {t1' <- eval1 ctx t1; return (TmApp t1' t2)}


eval :: Context -> Term -> Term
eval ctx t =
  case eval1 ctx t of
    Just t' -> eval ctx t'
    Nothing -> t
