module Model where

-- Exercise 1
-- Straight from the Lexer.x
data Token = TArrow | TDot | TComma | TGo | TTake | TMark | TNothing | TTurn | TCase | TOf | TEnd | 
  TLeft | TRight | TFront | TSemicolon | 
  TEmpty | TLambda | TDebris | TAsteroid | TBoundary | TUnderscore | 
  TIdent String deriving (Eq,Show)

-- Exercise 2
data Program = Program [Rule] deriving (Eq,Show)
data Rule = Rule String Cmds deriving (Eq,Show)
data Cmds = Cmds [Cmd] deriving (Eq,Show,Ord)
data Cmd = Go 
  | Take 
  | Mark 
  | Nothin
  | Turn Dir 
  | Case Dir Alts 
  | Ident String deriving (Eq,Show,Ord)
data Dir = Lef
  | Righ 
  | Fron deriving (Eq,Show,Ord)
data Alts = Alts [Alt]  deriving (Eq,Show,Ord)

data Alt = Alt Contents Cmds deriving (Eq,Show,Ord)
data Contents = Empty 
  | Lambda 
  | Debris 
  | Asteroid 
  | Boundary 
  | Underscore deriving (Eq,Show,Ord)
