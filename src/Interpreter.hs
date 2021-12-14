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
import Data.List (intercalate)
import Data.Sequence (chunksOf)
import Data.Maybe (fromJust)

type Size      =  Int
type Pos       =  (Int, Int)
type Space     =  Map Pos Contents

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

s1 :: Space
s1 = L.fromList [ 
    ((0,0), Empty), 
    ((0,1), Lambda),
    ((0,2), Debris),
    ((0,3), Asteroid)
  ]

-- Exercise 7
printSpace :: Space -> String
printSpace s = printHeader
  where
    printHeader = show (maximum (L.keys s)) ++ "\n"
    printRow r = map (showContents . snd) (filter (\((_, y),_) -> y == r) (L.toList s)) ++ "\n"
    showContents c = fromJust (lookup c contentsTable)

-- These three should be defined by you
type Ident = ()
type Commands = ()
type Heading = ()

type Environment = Map Ident Commands

type Stack       =  Commands
data ArrowState  =  ArrowState Space Pos Heading Stack

data Step =  Done  Space Pos Heading
          |  Ok    ArrowState
          |  Fail  String

-- | Exercise 8
toEnvironment :: String -> Environment
toEnvironment = undefined

-- | Exercise 9
step :: Environment -> ArrowState -> Step
step = undefined


