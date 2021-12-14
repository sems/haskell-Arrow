module Algebra where

import Model
import Data.List

-- Exercise 5
type Algebra program rule cmds cmd dir alts alt contents = (
    [rule] -> program, -- Program
    String -> cmds -> rule, -- Rule
    [cmd] -> cmds, -- Cmds
    cmd, -- Go Cmd
    cmd, -- Take Cmd
    cmd, -- Mark Cmd
    cmd, -- Nothin Cmd
    dir -> cmd, -- Turn Cmd
    dir -> alts -> cmd, -- Case Cmd
    String -> cmd, -- Ident Cmd
    dir, -- Lef Dir
    dir, -- Righ Dir
    dir, -- Fron Dir
    [alt] -> alts, -- Alts
    contents -> cmds -> alt, -- Alt
    contents, -- Empty Contents
    contents, -- Lambda Contents
    contents, -- Debris Contents
    contents, -- Asteroid Contents
    contents, -- Boundary Contents
    contents -- Underscore Contents
  )

algCheck :: Algebra Bool (Bool,String,[String]) (Bool,[String]) (Bool,[String]) Dir (Bool,[String]) (Bool,Contents,[String]) Contents
algCheck = (fprogram ,frule, fcmds, (True,[]),(True,[]), (True,[]), (True, [] ), fturn, fcase, fident, Lef, Righ, Fron, falts, falt, Empty, Lambda, Debris, Asteroid, Boundary, Underscore )    

fprogram :: [(Bool,String,[String])] -> Bool
fprogram p = checkprog p [] []
  where checkprog ((False,_,_):_) _ _ = False
        checkprog [] rs ss | notElem "start" rs = False
                           | otherwise = checkStrings ss
          where checkStrings [] = True
                checkStrings (x:xs) | elem x rs = checkStrings xs 
                                    | otherwise = False
        checkprog ((_,rule,ss):xs) rs ts | elem rule rs = False
                                         | otherwise = checkprog xs (rule:rs) (ss++ts)

frule :: String ->  (Bool,[String])  -> (Bool,String,[String])
frule s (b,ss) = (b,s,ss)

fcmds :: [(Bool,[String])] -> (Bool,[String])
fcmds = checkCmds []
  where checkCmds _ ((False,_):_) = (False,[]) 
        checkCmds ss [] = (True,ss)
        checkCmds ts ((_,ss):xs) = checkCmds (ts++ss) xs

fturn :: Dir -> (Bool,[String])
fturn d = (True,[])

fcase :: Dir ->(Bool,[String]) -> (Bool,[String])
fcase _ x = x 

fident :: String -> (Bool,[String])
fident s = (True,[s])

falts :: [(Bool,Contents,[String])] -> (Bool,[String])
falts alts =  checkAlts alts [] []
  where checkAlts :: [(Bool,Contents,[String])] -> [Contents] -> [String] -> (Bool,[String])
        checkAlts [] cs ss = (checkContents,ss)
          where checkContents = elem Underscore cs || sort cs == [Empty,Lambda,Debris,Asteroid,Boundary]
        checkAlts ((False,_, _):_) _ _= (False,[])
        checkAlts ((_,c,ss):xs) cs ts = checkAlts xs (c:cs) (ss++ts)

falt :: Contents ->  (Bool,[String])-> (Bool,Contents,[String])
falt c (False,_) = (False,c,[])
falt c (b,ss) = (b,c,ss)

fold :: Algebra program rule cmds cmd dir alts alt contents -> Program -> program
fold (aProgram, aRule, aCmds, aGo, aTake, aMark, aNothing, aTurn, aCase, aIdent, aLeft, aRight, aFront, aAlts, aAlt, aEmpty, aLambda, aDebris, aAsteroid, aBoundary, aUnderscore) = fProg
  where
    fProg (Program rules) = aProgram (map fRule rules) -- Program
    fRule (Rule str cmds) = aRule str (fCmds cmds) -- Rule
    fCmds (Cmds cmds) = aCmds (map fCmd cmds) -- Cmds
    fCmd Go = aGo -- Go Cmd
    fCmd Take = aTake -- Take Cmd
    fCmd Mark = aMark -- Mark Cmd
    fCmd Nothin = aNothing -- Nothin Cmd
    fCmd (Turn dir) = aTurn (fDir dir) -- Turn Cmd
    fCmd (Case dir alts) = aCase (fDir dir) (fAlts alts) -- Case Cmd
    fCmd (Ident str) = aIdent str -- Ident Cmd
    fDir Lef = aLeft -- Lef Dir
    fDir Righ = aRight -- Righ Dir
    fDir Fron = aFront-- Fron Dir
    fAlts (Alts alts) = aAlts (map fAlt alts) -- Alts
    fAlt (Alt contents cmds) = aAlt (fCon contents) (fCmds cmds) -- Alt
    fCon Empty = aEmpty -- Empty Contents
    fCon Lambda = aLambda -- Lanbda Contents
    fCon Debris = aDebris -- Debris Contents
    fCon Asteroid = aAsteroid -- Asteroid Contents
    fCon Boundary = aBoundary -- Boundary Contents
    fCon Underscore = aUnderscore -- Underscore Contents

-- Exercise 6

checkProgram :: Program -> Bool
checkProgram  = fold algCheck 