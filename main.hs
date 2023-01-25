
import Control.Arrow (first)
import System.IO
import System.Environment
import Control.Exception

import MonadicParsing
import Syntax
import Parser

{-- import only one Eval file for the Reduction Strategy you want to run ----}
import Eval1NOR -- Normal Order
--import Eval2CBV -- Call-by-value
--import Eval3HD -- Head 
--import Eval4CBN -- Call-by-name
--import Eval5APP -- Applicative Order


toplevel :: String -> Context -> Maybe ([Command], Context)
toplevel inp ctx =
    case runParser (command ctx) inp of 
        Just (Bind x b, y) -> case y of 
            (';':ys) -> let (cmd, ctx') = (Bind x b, addname ctx x) in (first ((:) cmd) <$> toplevel ys ctx')       
        Just (Eval t, y) -> case y of 
            (';':ys) -> let (cmd, ctx') = (Eval t, ctx ) in (first ((:) cmd) <$> toplevel ys ctx')
        Nothing ->  return ([], ctx)

execute :: Context -> [Command] -> IO()
execute _ [] = return ()
execute ctx (cmd:cs) = 
  let (ctx', str) = processCommand ctx cmd
    in do putStrLn str
          execute ctx' cs

processCommand :: Context -> Command -> (Context, String)
processCommand ctx (Eval t) =  (ctx, (showTerm ctx (eval ctx t)) ++ ";")     
processCommand ctx (Bind x b) =
        let b' = evalbinding ctx b
            prstr = x ++ (showBind ctx b') ++ ";" 
            ctx' = addbinding ctx x b'            
        in (ctx', prstr)

evalbinding :: Context -> Binding -> Binding
evalbinding ctx (EqTmBind t) = EqTmBind (eval ctx t) 
evalbinding ctx (ColonEqTmBind t) = ColonEqTmBind t
evalbinding ctx NameBind      = NameBind

onError :: String -> IOError -> IO String 
onError filename error_ = do 
    hPutStrLn stderr $ filename ++ " :: " ++ (show error_) 
    return [] 
 
main :: IO () 
main = do  
    args <- getArgs
    inp <- if null args
               then getContents 
               else catch (readFile $ head args) 
                      (onError $ head args) 
    
    case toplevel inp [] of 
        Just (comlist, _) -> execute [] comlist
        Nothing -> print "error"

{-
-- for testing
test :: String -> IO()
test inp = case toplevel inp [] of 
    Just (comlist, _) -> execute [] comlist
    Nothing -> print "error"

main :: IO()
main = do 
    print $ runParser (command [])"(\\x.x x)(\\y.y)"
    print $ runParser (command [])"a/;a;"
    print $ runParser (command [])"b := (\\x.x x)(\\y.y);b;"
    print $ runParser (command [])"c = (\\x.x x)(\\y.y);c;"
    test "(\\x.x)(\\x.x x);a/;a;b := (\\x.x)(\\x.x x);b;c = (\\x.x)(\\x.x x);c;"
-}