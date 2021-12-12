{
module Parser where

import Model
}

%name foo
%tokentype { Token }

%token
  "->" { TArrow }
  "." { TDot}
  "," { TComma}
  go { TGo}
  take { TTake}
  mark { TMark}
  nothing {TNothing}
  turn { TTurn}
  case { TCase}
  of {TOf}
  end {TEnd}
  left {TLeft}
  right {TRight}
  front { TFront}
  ";" {TSemicolon}
  empty {TEmpty }
  lambda {TLambda }
  debris {TDebris}
  asteroid {TAsteroid }
  boundary {TBoundary }
  "_" {TUnderscore}
  ident {TIdent $$} 

%%



Program :  Rule {  [$1] }
        |  Program Rule {  $2 : $1}
Rule : ident "->" Cmds "." {Rule $1 $3}
Cmds : {- empty -} { []}
     | Cmd  { [$1]}
     | Cmds "," Cmd { $3:$1}
Cmd : go {Go}
    | take {Take}
    | mark {Mark}
    | nothing {Nothin}
    | turn Dir {Turn $2}
    | case Dir of Alts end {Case $2 $4}
    | ident {Ident $1}
Dir : left {Lef}
    | right {Righ}
    | front {Front}
Alts :  {- empty -} {[]}
     | Alt { [$1]}
     | Alts ";" Alt { $3:$1}
Alt : Contents "->" Cmds {Alt $1 $3}
Contents : empty {Empty}
    | lambda {Lambda}
    | debris {Debris}
    | asteroid {Asteroid}
    | boundary {Boundary}
    | "_" {Underscore}

{

happyError _ = error "parse error"

}