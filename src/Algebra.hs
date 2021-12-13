module Algebra where

import Model


-- Exercise 5
type Algebra rule cmds cmd dir alts alt contents program = (
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

fold :: Algebra rule cmds cmd dir alts alt contents program -> Program -> program
fold (aProgram, aRule, aCmds, aTake, aMark, aNothing, aTurn, aCase, aIdent, aLeft, aRight, aFront, aAlts, aAlt, aEmpty, aLambda, aDebris, aAsteroid, aBoundary, aUnderscore) p = f p
  where
    f (Program rules) = aProgram (map f rules) -- Program
    f (Rule str cmds) = aRule str (f cmds) -- Rule
    f (Cmds cmds) = aCmds (map f cmds) -- Cmds
    f (Take) = aTake
    f (Mark) = aMark
    f (Nothin) = aNothing
    f (Turn dir) = aTurn (f dir)
    f (Case dir alts) = aCase (f dir) (map f alts)
    f (Ident str) = aIdent str
    f (Lef) = aLef
    f (Righ) = aRight
    f (Fron) = aFront
    f (Alts alts) = aAlts (map f alts)
    f (Alt contents cmds) = aAlt (f contents) (f cmds)
    f (Empty) = aEmpty
    f (Lambda) = aLambda
    f (Debris) = aDebris
    f (Asteroid) = aAsteroid
    f (Boundary) = aBoundary
    f (Underscore) = aUnderscore
    



-- Exercise 6

checkProgram :: Program -> Bool
checkProgram = undefined