module Model where

-- Exercise 1
-- Straight from the Lexer.x
data Token = TArrow | TDot | TComma | TGo | TTake | TMark | TNothing | TTurn | TCase | TOf | TEnd | 
  TLeft | TRight | TFront | TSemicolon | 
  TEmpty | TLambda | TDebris | TAsteroid | TBoundary | TUnderscore | 
  TIdent String deriving (Eq,Show)


-- Exercise 2
type Program =  [Rule] 
data Rule = Rule String Cmds deriving (Eq,Show)
type Cmds = [Cmd] 
data Cmd = Go 
  | Take 
  | Mark 
  | Nothin
  | Turn Dir 
  | Case Dir Alts 
  | Ident String deriving (Eq,Show)
data Dir = Lef
  | Righ 
  | Front deriving (Eq,Show)
type Alts =  [Alt] 
data Alt = Alt Contents Cmds deriving (Eq,Show)
data Contents = Empty 
  | Lambda 
  | Debris 
  | Asteroid 
  | Boundary 
  | Underscore deriving (Eq,Show)
