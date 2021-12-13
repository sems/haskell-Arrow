module Algebra where

import Model

-- Exercise 5
type Algebra program rule cmds cmd dir alts alt contents = (
    [rule] -> program, -- Program
    String -> cmds -> rule, -- Rule
    [cmd] -> cmds, -- Cmds
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

fold :: Algebra program rule cmds cmd dir alts alt contents -> Program -> program
fold (aProgram, aRule, aCmds, aTake, aMark, aNothing, aTurn, aCase, aIdent, aLeft, aRight, aFront, aAlts, aAlt, aEmpty, aLambda, aDebris, aAsteroid, aBoundary, aUnderscore) = fProg
  where
    fProg (Program rules) = aProgram (map fRule rules) -- Program
    fRule (Rule str cmds) = aRule str (fCmds cmds) -- Rule
    fCmds (Cmds cmds) = aCmds (map fCmd cmds) -- Cmds
    fCmd Take = aTake
    fCmd Mark = aMark
    fCmd Nothin = aNothing
    fCmd (Turn dir) = aTurn (fDir dir)
    fCmd (Case dir alts) = aCase (fDir dir) (fAlts alts)
    fCmd (Ident str) = aIdent str
    fDir Lef = aLeft
    fDir Righ = aRight
    fDir Fron = aFront
    fAlts (Alts alts) = aAlts (map fAlt alts)
    fAlt (Alt contents cmds) = aAlt (fCon contents) (fCmds cmds)
    fCon Empty = aEmpty
    fCon Lambda = aLambda
    fCon Debris = aDebris
    fCon Asteroid = aAsteroid
    fCon Boundary = aBoundary
    fCon Underscore = aUnderscore

-- Exercise 6

checkProgram :: Program -> Bool
checkProgram = undefined