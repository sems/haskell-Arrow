module Interpreter where

import ParseLib.Abstract
import Prelude hiding ((<*), (<$))

import Data.Map (Map)
import qualified Data.Map as L
import Data.Char (isSpace)
import Control.Monad (replicateM)

import Lexer
import Parser
import Model
import Algebra
import Data.List (intercalate,sort)
import Data.Sequence (chunksOf)
import Data.Maybe (fromJust)

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents

run :: Parser a Space -> [a] -> Space -- copied from the Icalander assignment (Later adjusted)
run p input = getOutput (parse p input)
    where 
        getOutput [] = L.empty
        getOutput ((x,[]):_) =  x
        getOutput (_:ys) = getOutput ys

-- | Parses a space file that can be found in the examples folder.
parseSpace :: Parser Char Space
parseSpace = do
    (mr, mc) <- parenthesised ((,) <$> natural <* symbol ',' <*> natural) <* spaces
    -- read |mr + 1| rows of |mc + 1| characters
    css      <- replicateM (mr + 1) (replicateM (mc + 1) contents)
    -- convert from a list of lists to a finite map representation
    return $ L.fromList $ concat $
            zipWith (\r cs ->
            zipWith (\c d -> ((r, c), d)) [0..] cs) [0..] css
  where
    spaces :: Parser Char String
    spaces = greedy (satisfy isSpace)
    contents :: Parser Char Contents
    contents = choice (Prelude.map (\(f,c) -> f <$ symbol c) contentsTable) <* spaces

-- | Conversion table
contentsTable :: [ (Contents, Char)]
contentsTable =  [ (Empty   , '.' )
                 , (Lambda  , '\\')
                 , (Debris  , '%' )
                 , (Asteroid, 'O' )
                 , (Boundary, '#' )]

-- Testing space for testing the printSpace method
s1 :: Space
s1 = L.fromList [
    ((0,0), Empty),((1,0), Lambda),
    ((0,1), Lambda),((1,1), Empty),
    ((0,2), Debris),((1,2), Asteroid),
    ((0,3), Asteroid),((1,3), Debris),
    ((0,4), Boundary),((1,4), Boundary)
  ]

-- Exercise 7
printSpace :: Space -> String
printSpace s = printHeader ++ printRows 0
  where
    showContents :: Contents -> Char 
    showContents c = fromJust (lookup c contentsTable)
    -- PrintRow: first will filter the correct row (argument) to print. Then it will map to the snd in the tuple, which is of type Contents. Then it will map everything to the right string(s) and returns.
    printRow r = map (showContents . snd) (filter (\((_, y),_) -> y == r) (L.toList s)) ++ "\n" 
    highestInAxis :: Pos -- added typedef because it could be confusing
    highestInAxis = fst (L.findMax s)
    printHeader = show highestInAxis ++ "\n"
    printRows row | row <= snd highestInAxis = printRow row ++ printRows (row+1)
                  | otherwise                = ""

-- These three should be defined by you
type Ident = String
type Commands = [Cmd]
data Heading = N | E | S | W deriving (Eq, Enum, Ord, Show)

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment s  | checkProgram prog = getEnvr prog
                 | otherwise = L.empty
  where prog = parseTokens $ alexScanTokens s
        getEnvr (Program rules ) = L.fromList $ map f rules
        f (Rule s (Cmds cs)) = (s,reverse cs)

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step _ (ArrowState s p h []) = Done s p h
step e (ArrowState s p h (Go:cs)) | elem (L.lookup newpos s) [Just Empty, Just Lambda, Just Debris] = Ok (ArrowState s newpos h cs)
                                  | otherwise = Ok (ArrowState s p h cs)
  where newpos = move Fron h p
step e (ArrowState s p h (Take:cs)) = Ok (ArrowState (L.insert p Empty s) p h cs)
step e (ArrowState s p h (Mark:cs)) = Ok (ArrowState (L.insert p Lambda s) p h cs)
step e (ArrowState s p h (Nothin:cs)) = Ok (ArrowState s p h cs)
step e (ArrowState s p h (Turn d:cs)) = Ok (ArrowState s p (newHeading d h) cs)
step e (ArrowState s p h (Case d (Alts alts):cs)) = caseOf ( L.lookup newPos s) (reverse alts)
  where caseOf _ [] = Fail "non-Exhaustive patter in caseOf"
        caseOf Nothing ((Alt c (Cmds cmds)):xs) | c == Boundary =  Ok (ArrowState s p h (reverse cmds++cs))
                                                | c == Underscore =  Ok (ArrowState s p h (reverse cmds++cs))
                                                | otherwise = caseOf Nothing xs
        caseOf (Just cont) ((Alt c (Cmds cmds)):xs) | c == cont = Ok (ArrowState s p h (reverse cmds++cs))
                                                    | c == Underscore =  Ok (ArrowState s p h (reverse cmds++cs))
                                                    | otherwise = caseOf (Just cont) xs
        newPos = move d h p
step e (ArrowState s p h (Ident ident :cs)) = addRule (L.lookup ident e)
  where addRule Nothing = Fail ("Rule " ++ ident ++ " did not exist")
        addRule (Just cmds) = Ok (ArrowState s p h (cmds++cs))

move :: Dir -> Heading -> Pos -> Pos
move d h p = movePos p (newHeading d h)
  where
    movePos (x,y) N = (x,y-1)
    movePos (x,y) E = (x+1,y)
    movePos (x,y) S = (x,y+1)
    movePos (x,y) W = (x-1,y)

newHeading :: Dir -> Heading -> Heading
newHeading Lef N = W
newHeading Lef x = pred x
newHeading Fron x = x
newHeading Righ W = N
newHeading Righ x = succ x

