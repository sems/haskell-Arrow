{
module Lexer where

import Model
}

%wrapper "basic"
-- Some of the following tokens are copied from the official docs, from Alex.
$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-
  $white+                         ;
  "--".*                          ;
  "->"                            { const TArrow}
  \;                              { const TSemicolon}
  \,                              { const TComma}
  \.                              { const TDot}
  \_                              { const TUnderscore }
  go                              { const TGo}
  take                            { const TTake}
  mark                            { const TMark}
  nothing                         { const TNothing}
  turn                            { const TTurn}
  case                            { const TCase}
  of                              { const TOf}
  end                             { const TEnd}
  left                            { const TLeft}      
  right                           { const TRight}
  front                           { const TFront}
  Empty                           { const TEmpty}
  Lambda                          { const TLambda}
  Debris                          { const TDebris}
  Asteroid                        { const TAsteroid}
  Boundary                        { const TBoundary}
  [$alpha $digit \+ \-]+          { \s -> TIdent s }


{

main = do
  s <- getContents
  print (alexScanTokens s)
}