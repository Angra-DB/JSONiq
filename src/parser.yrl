Header "%% Copyright (C)"
"%% @private"
"%% @Author John".

Nonterminals item literal numericLiteral stringLiteral booleanLiteral nullLiteral integerLiteral object pairs pairItem array arrayItem expr variableExpr
    flowr whereClause returnClause collectionClause letClause keyExpr intervalClause.

Terminals variable string digits boolean null ':' ',' '{' '}' '[' ']' '||' '+' '-' '*' 'mod' 'idiv' 'to' '?' 'eq' 'ne' 'lt' 'le' 'gt' 'ge' 'and' 'or' 'not' '.'
    '(' ')' 'it' 'where' 'for' 'in' 'collection' 'return' 'let' '=' ';' 'if' 'then' 'else' 'save'.

Rootsymbol item.


Left 100 ';'.
Left 200 '+' '-'.
Left 300 '*' 'idiv' 'mod'.
Left 400 'or'.
Left 430 'and'.
Left 460 'not'.
Left 500 'eq' 'ne' 'le' 'lt' 'ge' 'gt'.
Unary 550 keyExpr.
Left 600 '.'.
Right 700 '='.

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
object -> '{' '}' : {object, []}.
pairs -> pairItem : ['$1'].
pairs -> pairItem ',' pairs : ['$1' | '$3'].
pairItem -> expr ':' expr : {mk_pair, ['$1', '$3']}.
pairItem -> expr '?' ':' expr : {force_not_null, ['$1', '$4']}.

array -> '[' ']' : {array, []}.
array -> '[' arrayItem ']' : {array, '$2'}.
array -> '[' expr 'to' expr ']' : {gen_seq, ['$2', '$4']}.
arrayItem -> expr : ['$1'].
arrayItem -> expr ',' arrayItem : ['$1' | '$3'].

collectionClause -> 'collection' '(' stringLiteral ')' : {collection,  '$3'}.

variableExpr -> variable : {variable, value_of('$1')}.

expr -> variableExpr : '$1'.
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
expr -> expr '[''[' expr ']'']' : {indexer, ['$1', '$4']}.
expr -> expr '[' expr ']' : {predicate, ['$1', '$3']}.
expr -> literal : '$1'.
expr -> object : '$1'.
expr -> array : '$1'.
expr -> 'it' : element(1, '$1').
expr -> flowr : '$1'.
expr -> letClause ';' expr : add_expr('$1', '$3').
expr -> 'if' expr 'then' expr 'else' expr : {if_clause, ['$2', '$4', '$6']}.
expr -> keyExpr : '$1'.
expr -> 'save' '(' expr ',' expr ')' : {save, ['$3', '$5']}.
    

keyExpr -> '*' expr : {get_key, '$2'}.
letClause -> 'let' variableExpr '=' expr : {let_clause, ['$2', '$4']}.
flowr -> 'for' variableExpr 'in' collectionClause whereClause returnClause : {flowr, ['$2', '$4', '$5', '$6']}. 
flowr -> 'for' variableExpr 'in' collectionClause intervalClause whereClause returnClause : {flowr, ['$2', '$4', '$5', '$6', '$7']}. 
whereClause -> 'where' expr : {where, '$2'}.
returnClause -> 'return' expr : {return, '$2'}.
intervalClause -> '[' expr ',' expr ']' : {interval, {'$2', '$4'}}.

item -> expr : '$1'.

Erlang code.

value_of(Token) ->
    element(3, Token).

add_expr({let_clause, Args}, Expr) ->
    {let_clause, Args++[Expr]}.