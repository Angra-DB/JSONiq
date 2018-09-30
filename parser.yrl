Header "%% Copyright (C)"
"%% @private"
"%% @Author John".

Nonterminals item literal numericLiteral stringLiteral booleanLiteral nullLiteral integerLiteral object pairs pairItem array arrayItem expr.

Terminals string digits boolean null ':' ',' '{' '}' '[' ']' '||' '+' '-' '*' 'mod' 'idiv' 'to' '?' '|' 'eq' 'ne' 'lt' 'le' 'gt' 'ge' 'and' 'or' 'not' '.'
    '(' ')' 'it'.

Rootsymbol item.

integerLiteral -> digits : value_of('$1').
booleanLiteral -> boolean : value_of('$1').
nullLiteral -> null : value_of('$1').
stringLiteral -> string : value_of('$1').
numericLiteral -> integerLiteral : '$1'.

literal -> numericLiteral : {numeric, '$1'}.
literal -> stringLiteral : {string, '$1'}.
literal -> booleanLiteral : {bool, '$1'}.
literal -> nullLiteral : {null, '$1'}.

object -> '{' pairs '}' : {object, '$2'}.
object -> '{' '}' : {object, {}}.
pairs -> pairItem : add_if_not_empty('$1', []).
pairs -> pairItem ',' pairs : ['$1' | '$3'].
pairItem -> expr ':' expr : {mk_pair, ['$1', '$3']}.
pairItem -> expr '?' ':' expr : {force_not_null, ['$1', '$4']}.

array -> '[' ']' : {array, []}.
array -> '[' arrayItem ']' : {array, '$2'}.
array -> '[' expr 'to' expr ']' : {gen_seq, ['$2', '$4']}.
arrayItem -> expr : ['$1'].
arrayItem -> expr ',' arrayItem : ['$1' | '$3'].

expr -> '(' expr ')' : '$2'.
expr -> expr '||' expr : {concat, ['$1', '$3']}.
expr -> expr '+' expr : {add, ['$1', '$3']}.
expr -> expr '-' expr : {sub, ['$1', '$3']}.
expr -> expr '*' expr : {mult, ['$1', '$3']}.
expr -> expr 'idiv' expr : {idiv, ['$1', '$3']}.
expr -> expr 'mod' expr : {mod, ['$1', '$3']}.
expr -> expr 'eq' expr : {eq, ['$1', '$3']}.
expr -> expr 'ne' expr : {ne, ['$1', '$3']}.
expr -> expr 'le' expr : {le, ['$1', '$3']}.
expr -> expr 'lt' expr : {lt, ['$1', '$3']}.
expr -> expr 'ge' expr : {ge, ['$1', '$3']}.
expr -> expr 'gt' expr : {gt, ['$1', '$3']}.
expr -> expr 'and' expr : {'and', ['$1', '$3']}.
expr -> expr 'or' expr : {'or', ['$1', '$3']}.
expr -> 'not' expr : {'not', ['$2']}.
expr -> expr '.' expr : {selector, ['$1', '$3']}.
expr -> expr '[''[' expr ']'']' : {indexer, ['$1', '$3']}.
expr -> literal : '$1'.
expr -> object : '$1'.
expr -> array : '$1'.

item -> expr : '$1'.

Erlang code.

value_of(Token) ->
    element(3, Token).