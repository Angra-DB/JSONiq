Definitions.

Constructors = to|\|\|

Arithmetic = \+|\-|\*|idiv|mod

Comparison = le|lt|ne|eq|gt|ge

Separators = [\:\,\{\}\[\]\?\|\)\(\.]

Logic = and|or|not

Rules.

[\s\t] : skip_token.

\n|\r\n|\r : skip_token.

-?[0-9]+ :
  {token,{digits, TokenLine, list_to_integer(TokenChars)}}.

null :
  {token,{null, TokenLine, list_to_atom(TokenChars)}}.

true|false :
  {token,{boolean, TokenLine, list_to_atom(TokenChars)}}.

\"(\\.|[^"\\])*\" :
  {token,{string, TokenLine, string:slice(TokenChars, 1, length(TokenChars)-2)}}.

it :
  {token,{list_to_atom(TokenChars), TokenLine}}.

{Constructors} : 
  {token,{list_to_atom(TokenChars), TokenLine}}.

{Arithmetic} :
  {token,{list_to_atom(TokenChars), TokenLine}}.

{Comparison} :
  {token,{list_to_atom(TokenChars), TokenLine}}.

{Logic} :
  {token,{list_to_atom(TokenChars), TokenLine}}.

{Separators} :
  {token,{list_to_atom(TokenChars), TokenLine}}.

Erlang code.