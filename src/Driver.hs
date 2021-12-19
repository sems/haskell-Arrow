module Driver where

import Algebra
import Model
import Interpreter
import Data.Maybe
import System.Exit

main = do -- interpeter asks for all needed information and then runs the program
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
        runProgram (Ok a@(ArrowState s p h cs)) = do  -- every step program ask for confirmation and when given prints the space and the arrows position and heading
                            getConfrm 
                            print (printSpace s ++ " Pos: " ++ show p ++ " Head: "++ show h ) 
                            runProgram (step e a)
        runProgram (Done s p h) = print ( printSpace s) *> print p *> print h -- if the program finishes print all the final state
        runProgram (Fail s) = print s --if program fails print the error mesage
        getConfrm =  print "Continue[c] / Stop [s]" *> getLine >>= confrm  
        confrm :: String -> IO ()
        confrm "c" = return () -- if 'c' is gives continue program
        confrm "s" = die "Terminate Program" -- if s is given stop program
        confrm _ =  print "Invalid input" *> getLine >>= confrm -- if any other input is given ask again

batch :: Environment -> ArrowState -> (Space, Pos, Heading)
batch e a' = runProgram (step e a') -- loops through program till it's done (or an erroe is given)
    where runProgram (Ok a) = runProgram (step e a)
          runProgram (Fail s) = undefined -- there is no specification of what should be returned here in case the program fails
          runProgram (Done s p h) = (s, p, h)

runProgram' :: String -> Environment -> ArrowState -> IO() -- calls the batch function or the interative function based on the given input
runProgram' "b" e a = printBatch $ batch  e a
runProgram' "i" e a = interactive e a
runProgram' _ _ _ = print "invalid input"

printBatch :: (Space,Pos,Heading)  -> IO() -- prints the contents of batch return
printBatch (s,p,h)= print (printSpace s ++ "Pos: "++ show p++ " Head: " ++ show h)

getHead :: String -> Heading -- converts the given string into it's matching heading 
getHead "N" = N
getHead "S" = S
getHead "E" = E
getHead "W" = W