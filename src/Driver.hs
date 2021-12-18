module Driver where

import Algebra
import Model
import Interpreter
import Data.Maybe
import System.Exit

main = do
    print "Enter path space" 
    space <- readFile =<< getLine
    print "Enter Path program"
    program <- readFile =<< getLine
    print "Enter Starting Position"
    pos <- readLn
    print "Enter Starting Heading"
    head <- getHead <$> getLine
    print "Enter mode batch [b] / interactive [i] "
    mode <- getLine
    let s = run parseSpace space
    let e = toEnvironment program
    let a = ArrowState s pos head [Ident "start"]
    runProgram' mode e a

-- Exercise 11
interactive :: Environment -> ArrowState -> IO ()
interactive e a'@(ArrowState s' _ _ _) = print (printSpace s') *> runProgram (step e a') 
  where runProgram :: Step -> IO ()
        runProgram (Ok a@(ArrowState s p h cs)) = do 
                            input <- getConfrm 
                            print (printSpace s ++ " Pos: " ++ show p ++ " Head: "++ show h ) 
                            runProgram (step e a)
        runProgram (Done s p h) = print ( printSpace s) *> print p *> print h 
        runProgram (Fail s) = print s
        getConfrm =  print "Continue[c] / Stop [s]" *> getLine >>= confrm  
        confrm :: String -> IO ()
        confrm "c" = return ()
        confrm "s" = die "Terminate Program"
        confrm _ =  print "Invalid input" *> getLine >>= confrm 

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch e a' = runProgram (step e a')
    where runProgram (Ok a) = runProgram (step e a)
          runProgram (Fail s) = undefined -- there is no specification of what should be returned here in case the program fails
          runProgram (Done s p h) = (s, p, h)



runProgram' :: String -> Environment -> ArrowState -> IO()
runProgram' "b" e a = printBatch $ batch  e a
runProgram' "i" e a = interactive e a
runProgram' _ _ _ = print "invalid input"

printBatch :: (Space,Pos,Heading)  -> IO()
printBatch (s,p,h)= print (printSpace s ++ "Pos: "++ show p++ " Head: " ++ show h)


getHead :: String -> Heading
getHead "N" = N
getHead "S" = S
getHead "E" = E
getHead "W" = W