{
module Parser where

import Model
}

%name foo
%tokentype { Token }

%token
  x { Token }

%%

Program : { Program }

{

happyError _ = error "parse error"

}