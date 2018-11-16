-module(jsoniq).
-compile(export_all).

run_query_from_file(File, AdbSock) ->
    {ok, String} = file:read_file(File),
    run_query(binary_to_list(String), AdbSock).

run_query(Query, AdbSock) ->
    case parse(Query) of
        {ok, ParsingTree} ->
            lager:info("Parsing Tree = ~p", [ParsingTree]),
            format_json(execute(ParsingTree, AdbSock, []));
        Err ->
            Err
    end.

parse(Query) ->
    case lex:string(Query) of
        {ok, Tokens, _} -> 
            parser:parse(Tokens);
        Err ->
            Err
    end.

execute(T = {Type, _}, _, _) when Type =:= numeric; Type =:= bool; Type =:= string; Type =:= null; Type =:= pair ->
    T;

execute({variable, Var}, _, Context) ->
    case proplists:get_value(Var, Context, not_found) of
        not_found ->
            variable_not_found;
        Val ->
            Val
    end;

execute({let_clause, [Var, Attr, RestExpr]}, AdbSock, Context) ->
    execute(RestExpr, AdbSock, bind_variable(Var, execute(Attr, AdbSock, Context), Context));

execute({if_clause, [ConditionExpr, ThenExpr, ElseExpr]}, AdbSock, Context) ->
    case execute(ConditionExpr, AdbSock, Context) of
        {bool, true} ->
            execute(ThenExpr, AdbSock, Context);
        {bool, false} ->
            execute(ElseExpr, AdbSock, Context);
        _ ->
            invalid_condition_expression
    end;

execute({array, ExpL}, AdbSock, Context) ->
    {array, lists:map(fun(X) -> execute(X, AdbSock, Context) end, ExpL)};

execute({object, ExpL}, AdbSock, Context) ->
    {object, lists:filtermap(fun(X) -> 
            case (V = execute(X, AdbSock, Context)) =:= {} of
                false ->
                    {true, V};
                _ ->
                    false         
            end 
        end, ExpL)};

execute({mk_pair, [KExp, VExp]}, AdbSock, Context) ->
    {K, V} = {execute(KExp, AdbSock, Context), execute(VExp, AdbSock, Context)},
    make_pair(K, V);
    

execute({force_not_null, [KExp, VExp]}, AdbSock, Context) ->
    {K, V} = {execute(KExp, AdbSock, Context), execute(VExp, AdbSock, Context)},
    case V of
        {null, _} ->
            {};
        _ ->
            make_pair(K, V)
    end;

execute({db_object, [Obj, K ={object_key, _}]}, AdbSock, Context) ->
    {db_object, [execute(Obj, AdbSock, Context), K]};

execute({get_key, Expr}, AdbSock, Context) ->
    case execute(Expr, AdbSock, Context) of
        {db_object, [_, K]} ->
            K;
        {object, _} ->
            {null, null};
        _ ->
            not_a_object
    end;


execute({flowr, [Var, Collection, WhereClause, {return, ReturnExpr}]}, AdbSock, Context) ->
    {Keys, {ok, Jsons}} = filter_collection(Var, WhereClause, Collection, AdbSock, Context),
    ObjList = format_string("[~s]", [string:join(Jsons, ",")]),
    case parse(ObjList) of
        {ok, {array, Objs}} ->
            WithKeys = lists:zip(Objs, Keys),
            {array, lists:map(fun ({O, K}) -> execute(ReturnExpr, AdbSock, bind_variable(Var, execute({db_object, [O, {object_key, K}]}, AdbSock, Context), Context)) end, WithKeys)};
        _ ->
            invalid_collection
    end;
    

execute({concat, Args}, AdbSock, Context) ->
    {string, execute_op(Args, [string, string], fun(A, B) -> A ++ B end, AdbSock, Context)};

execute({add, Args}, AdbSock, Context) ->
    {numeric, execute_op(Args, [numeric, numeric], fun(A, B) -> A + B end, AdbSock, Context)};

execute({sub, Args}, AdbSock, Context) ->
    {numeric, execute_op(Args, [numeric, numeric], fun(A, B) -> A - B end, AdbSock, Context)};

execute({mult, Args}, AdbSock, Context) ->
    {numeric, execute_op(Args, [numeric, numeric], fun(A, B) -> A * B end, AdbSock, Context)};

execute({idiv, Args}, AdbSock, Context) ->
    {numeric, execute_op(Args, [numeric, numeric], fun(A, B) -> A div B end, AdbSock, Context)};

execute({mod, Args}, AdbSock, Context) ->
    {numeric, execute_op(Args, [numeric, numeric], fun(A, B) -> A rem B end, AdbSock, Context)};

execute({eq, Args}, AdbSock, Context) ->
    {bool, execute_op(Args, [[numeric, string, null], [numeric, string, null]], fun(A, B) -> A =:= B end, AdbSock, Context)};

execute({ne, Args}, AdbSock, Context) ->
    {bool, execute_op(Args, [any, any], fun(A, B) -> A =/= B end, AdbSock, Context)};
    
execute({le, Args}, AdbSock, Context) ->
    {bool, execute_op(Args, [[numeric, null, string], [numeric, null, string]], fun le/2, AdbSock, Context)};
    
execute({lt, Args}, AdbSock, Context) ->
    {bool, execute_op(Args, [[numeric, null, string], [numeric, null, string]], fun lt/2, AdbSock, Context)};

execute({ge, Args}, AdbSock, Context) ->
    {bool, execute_op(Args, [[numeric, null, string], [numeric, null, string]], fun ge/2, AdbSock, Context)};
    
execute({gt, Args}, AdbSock, Context) ->
    {bool, execute_op(Args, [[numeric, null, string], [numeric, null, string]], fun gt/2, AdbSock, Context)};

execute({'and', Args}, AdbSock, Context) ->
    {bool, execute_op(Args, [bool, bool], fun(A, B) -> A and B end, AdbSock, Context)};
    
execute({'or', Args}, AdbSock, Context) ->
    {bool, execute_op(Args, [bool, bool], fun(A, B) -> A or B end, AdbSock, Context)};

execute({'not', [Expr]}, AdbSock, Context) ->
    ExprValue = execute(Expr, AdbSock, Context),
    case ExprValue of
        {bool, Value} ->
            {bool, not Value};
        false ->
            invalid_argument    
    end;

execute({gen_seq, Args}, AdbSock, Context) ->
    ExprValues = [A, B] = execute_args(Args, AdbSock, Context),
    case type_guard(ExprValues, [numeric, numeric]) of
        true ->
            {array, lists:map(fun(X) -> {numeric, X} end, lists:seq(get_value(A), get_value(B)))};
        false ->
            invalid_arguments
    end;

execute({indexer, Args}, AdbSock, Context) ->
    ExprValues = [Array, Index] = execute_args(Args, AdbSock, Context),
    case type_guard(ExprValues, [array, numeric]) of
        true ->
            access_array(get_value(Array), get_value(Index));
        false ->
            invalid_arguments    
    end;

execute({selector, Args}, AdbSock, Context) ->
    ExprValues = [Obj, Selector] = execute_args(Args, AdbSock, Context),
    case type_guard(ExprValues, [[object, array, db_object], string]) of
        true ->
            access_field(Obj, Selector);
        false ->
            invalid_arguments    
    end;

execute({predicate, [ArrayExpr, PredExpr]}, AdbSock, Context) ->
    {array, Array} = execute(ArrayExpr, AdbSock, Context),
    execute({array, lists:filter(fun(V) -> execute_pred(V, PredExpr, AdbSock, Context) end, Array)}, AdbSock, Context);

execute(T, _, _) ->
    lager:info("Invalid Parsing Tree Node ~p", [T]),
    invalid_parse.
% Misc

idiv_numbers(N1, N2) when is_integer(N1), is_integer(N2) ->
    N1 div N2;
idiv_numbers(_, _) ->
    not_integers.

mod_numbers(N1, N2) when is_integer(N1), is_integer(N2) ->
    N1 rem N2;
mod_numbers(_, _) ->
    not_integers.

make_pair(K, V) ->
    case is_of_type({K, string}) of
        true ->
            {pair, {K, V}};
        _ ->
            invalid_key
    end.

le(null, _) ->
    true;
le(_, null) ->
    false;
le(A, B) ->
    A =< B.

lt(_, null) ->
    false;
lt(null, _) ->
    true;
lt(A, B) ->
    A < B.

ge(_, null) ->
    true;
ge(null, _) ->
    false;
ge(A, B) ->
    A >= B.

gt(null, _) ->
    false;
gt(_, null) ->
    true;
gt(A, B) ->
    A > B.

access_field({db_object, [Obj, _]}, Selector) ->
    access_field(Obj, Selector);

access_field({array, ObjList}, Selector) ->
    io:format("List ~p~nSelector ~p~n", [ObjList, Selector]),
    {array, lists:map(fun(Obj) -> access_field(Obj, Selector) end, ObjList)};

access_field({object, Obj}, Selector) -> 
    PropList = lists:map(fun({pair, KV}) -> KV end, Obj),
    proplists:get_value(Selector, PropList, {null, null}).

access_array(Array, Index) ->
    case Index+1 > length(Array) of
        true ->
            invalid_index;
        _ ->
            lists:nth(Index+1, Array)
    end.

% Evaluates if the type of Value is equal to the Type element of the tuple
is_of_type(Arg) -> 
    case Arg of
        {_, any} ->
            true;
        {{Type, _}, Type} ->
            true;
        {{Type, _}, L} when is_list(L) ->
            lists:any(fun(X) -> X =:= Type end, L);
        _ ->
            false
    end.
    

type_guard(Args, Types) when length(Args) =:= length(Types) ->
    lists:all(fun is_of_type/1, lists:zip(Args, Types)).

execute_args(Args, AdbSock, Context) -> 
    lists:map(fun(X) -> execute(X, AdbSock, Context) end, Args).

execute_op(Exprs, Types, OP, AdbSock, Context) ->
    ExprValues = [A, B] = execute_args(Exprs, AdbSock, Context),
    case type_guard(ExprValues, Types) of
        true ->
            OP(get_value(A), get_value(B));
        false ->
            invalid_arguments
    end.

get_value({Type, V}) when Type =:= numeric; Type =:= bool; Type =:= string; Type =:= null; Type =:= array; Type =:= object ->
    V;

get_value({db_object, [Obj, _]}) ->
    get_value(Obj).

get_type({Type, _}) ->
    Type.

execute_pred(V, PredExpr, AdbSock, Context) ->
    PredResult = execute(replace_context(V, PredExpr, it), AdbSock, Context),
    case is_of_type({PredResult, bool}) of
        true ->
            get_value(PredResult);
        _ ->
            invalid_predicate
    end.

replace_context(V, Var, Var) ->
    V;

replace_context(_, {Type, Value}, _) when Type =:= numeric; Type =:= bool; Type =:= string; Type =:= null ->
    {Type, Value};

replace_context(V, {Type, Args}, Var) ->
    {Type, lists:map(fun(X) -> replace_context(V, X, Var) end, Args)}.

filter_collection(Variable, {where, WhereClause}, {collection, CollectionName}, AdbSock, Context) ->
    send_command(AdbSock, format_string("connect ~s", [CollectionName])),
    AngraQuery = traverse_where_clause(Variable, WhereClause, AdbSock, Context),
    QueryResult = parse_erlang_term(send_command(AdbSock, format_string("query ~s", [AngraQuery]))),
    DocumentKeys = lists:map(fun(X) -> element(2, X) end, QueryResult),
    Documents = parse_erlang_term(send_command(AdbSock, format_string("bulk_lookup ~s", [string:join(DocumentKeys, " ")]))),
    {DocumentKeys, Documents}.


traverse_where_clause(Variable, {BoolOp, [Lhs, Rhs]}, AdbSock, Context) when BoolOp =:= 'or'; BoolOp =:= 'and' ->
    LhsExpr = traverse_where_clause(Variable, Lhs, AdbSock, Context),
    RhsExpr = traverse_where_clause(Variable, Rhs, AdbSock, Context),
    format_string("~s~s/~s", [RhsExpr, BoolOp, LhsExpr]);

traverse_where_clause(Variable, {eq, [Lhs, Rhs]}, AdbSock, Context) ->
    {{string, FilterString}, Selectors} = case {get_field_selectors(Variable, Lhs, AdbSock, Context), get_field_selectors(Variable, Rhs, AdbSock, Context)} of
        {normal_expr, normal_expr} ->
            invalid_where_clause;
        {normal_expr, FieldNames} ->
            {execute(Lhs, AdbSock, Context), FieldNames};
        {FieldNames, normal_expr} ->
            {execute(Rhs, AdbSock, Context), FieldNames}
    end,
    format_string("filter_field ~s/filter ~s/", [Selectors, FilterString]).

% This function should return the list of field selectors if the operand is accessing a for context variable or normal_expr otherwise
get_field_selectors(Variable, {selector, [Variable, Expr]}, AdbSock, Context) ->
    case execute(Expr, AdbSock, Context) of
        {string, FieldName} ->
            FieldName;
        _ ->
            invalid_where_clause
    end;

get_field_selectors(Variable, {selector, [Obj = {selector, _}, Selector]}, AdbSock, Context) ->
    case get_field_selectors(Variable, Obj, AdbSock, Context) of
        normal_expr ->
            normal_expr;
        OuterFieldNames ->
            {string, FieldName} = execute(Selector, AdbSock, Context),
            format_string("~p ~p", [OuterFieldNames, FieldName])
    end;

get_field_selectors(_, _, _, _) ->
    normal_expr.

bind_variable({variable, Var}, Val, []) ->
    [{Var, Val}];

bind_variable(Variable = {variable, Var}, Val, [V | Context]) ->
    case V of
        {Var, _} ->
            [{Var, Val} | Context];
        _ ->
            [V | bind_variable(Variable, Val, Context)]
    end.

% Formatting functions

format_json({array, List}) ->
    lists:map(fun (X) -> format_json(X) end, List);

format_json({object, PropList}) ->
    list_to_tuple(lists:map(fun ({pair, {Name, Value}}) -> {format_json(Name), format_json(Value)} end, PropList));

format_json({db_object, [Obj, _]}) ->
    format_json(Obj);

format_json({_, V}) ->
    V;

format_json(Error) ->
    Error.

format_string(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

send_command(Socket, Command) ->
    gen_tcp:send(Socket, Command),
    case gen_tcp:recv(Socket, 0, 500) of
        {ok, Packet} -> 
            L = binary_to_list(Packet),
            {Size, Rest} = split(L),
            read_result(Size, Rest, Socket);
        {error, Reason} -> 
            throw(Reason)
    end.

read_result(Size, Data, Socket) ->
    case Size > length(Data) of
        true ->
            case gen_tcp:recv(Socket, 0, 500) of
                {ok, Packet} -> 
                    read_result(Size, Data++binary_to_list(Packet), Socket);
                {error, Reason} -> 
                    throw(Reason)
            end;
        _ ->
            Data
    end.

split(Str) ->
    Stripped = string:strip(Str),
    Pred = fun(A) -> A =/= $  end,
    {Size, Args} = lists:splitwith(Pred, Stripped),
    {list_to_integer(Size), string:strip(Args)}.

parse_erlang_term(ErlString) ->
    {ok,Tokens,_} = erl_scan:string(ErlString++"."),
    {ok,Term} = erl_parse:parse_term(Tokens),
    Term.