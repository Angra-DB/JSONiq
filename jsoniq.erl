-module(jsoniq).
-compile(export_all).

run_query(File) ->
    {ok, String} = file:read_file(File),
    case lex:string(binary_to_list(String)) of
        {ok, Tokens, _} -> 
            case parser:parse(Tokens) of
                {ok, ParsingTree} ->
                    % io:format("Parsing Tree: ~p~n", [ParsingTree]),
                    execute(ParsingTree);
                Err ->
                    Err
            end;
        Err ->
            Err
    end.

execute(T = {Type, _}) when Type =:= numeric; Type =:= bool; Type =:= string; Type =:= null ->
    T;

execute({array, ExpL}) ->
    {array, lists:map(fun(X) -> execute(X) end, ExpL)};

execute({object, ExpL}) ->
    {object, lists:filtermap(fun(X) -> 
            case (V = execute(X)) =:= {} of
                false ->
                    {true, V};
                _ ->
                    false         
            end 
        end, ExpL)};

execute({mk_pair, [KExp, VExp]}) ->
    {K, V} = {execute(KExp), execute(VExp)},
    make_pair(K, V);
    

execute({force_not_null, [KExp, VExp]}) ->
    {K, V} = {execute(KExp), execute(VExp)},
    case V of
        {null, _} ->
            {};
        _ ->
            make_pair(K, V)
    end;

execute({concat, Args}) ->
    {string, execute_op(Args, [string, string], fun(A, B) -> A ++ B end)};

execute({add, Args}) ->
    {numeric, execute_op(Args, [numeric, numeric], fun(A, B) -> A + B end)};

execute({sub, Args}) ->
    {numeric, execute_op(Args, [numeric, numeric], fun(A, B) -> A - B end)};

execute({mult, Args}) ->
    {numeric, execute_op(Args, [numeric, numeric], fun(A, B) -> A * B end)};

execute({idiv, Args}) ->
    {numeric, execute_op(Args, [numeric, numeric], fun(A, B) -> A div B end)};

execute({mod, Args}) ->
    {numeric, execute_op(Args, [numeric, numeric], fun(A, B) -> A rem B end)};

execute({eq, Args}) ->
    {bool, execute_op(Args, [[numeric, string, null], [numeric, string, null]], fun(A, B) -> A =:= B end)};

execute({ne, Args}) ->
    {bool, execute_op(Args, [any, any], fun(A, B) -> A =/= B end)};
    
execute({le, Args}) ->
    {bool, execute_op(Args, [[numeric, null, string], [numeric, null, string]], fun le/2)};
    
execute({lt, Args}) ->
    {bool, execute_op(Args, [[numeric, null, string], [numeric, null, string]], fun lt/2)};

execute({ge, Args}) ->
    {bool, execute_op(Args, [[numeric, null, string], [numeric, null, string]], fun ge/2)};
    
execute({gt, Args}) ->
    {bool, execute_op(Args, [[numeric, null, string], [numeric, null, string]], fun gt/2)};

execute({'and', Args}) ->
    {bool, execute_op(Args, [bool, bool], fun(A, B) -> A and B end)};
    
execute({'or', Args}) ->
    {bool, execute_op(Args, [bool, bool], fun(A, B) -> A or B end)};

execute({'not', [Expr]}) ->
    ExprValue = execute(Expr),
    case ExprValue of
        {bool, Value} ->
            {bool, not Value};
        false ->
            invalid_argument    
    end;

execute({gen_seq, Args}) ->
    ExprValues = [A, B] = execute_args(Args),
    case type_guard(ExprValues, [numeric, numeric]) of
        true ->
            {array, lists:map(fun(X) -> {numeric, X} end, lists:seq(get_value(A), get_value(B)))};
        false ->
            invalid_arguments
    end;

execute({indexer, Args}) ->
    ExprValues = [Array, Index] = execute_args(Args),
    case type_guard(ExprValues, [array, numeric]) of
        true ->
            access_array(get_value(Array), get_value(Index));
        false ->
            invalid_arguments    
    end;

execute({selector, Args}) ->
    ExprValues = [Obj, Selector] = execute_args(Args),
    case type_guard(ExprValues, [[object, array], string]) of
        true ->
            access_field(Obj, Selector);
        false ->
            invalid_arguments    
    end;

execute({predicate, [ArrayExpr, PredExpr]}) ->
    execute({array, lists:filter(fun(V) -> execute_pred(V, PredExpr) end, get_value(ArrayExpr))}).

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
            {K, V};
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

access_field({array, ObjList}, Selector) ->
    io:format("List ~p~nSelector ~p~n", [ObjList, Selector]),
    lists:map(fun(Obj) -> access_field(Obj, Selector) end, ObjList);

access_field({object, Obj}, Selector) -> 
    proplists:get_value(Selector, Obj, null).

access_array(Array, Index) ->
    lists:nth(Index+1, Array).

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

execute_args(Args) -> 
    lists:map(fun(X) -> execute(X) end, Args).

execute_op(Exprs, Types, OP) ->
    ExprValues = [A, B] = execute_args(Exprs),
    case type_guard(ExprValues, Types) of
        true ->
            OP(get_value(A), get_value(B));
        false ->
            invalid_arguments
    end.

get_value({Type, V}) when Type =:= numeric; Type =:= bool; Type =:= string; Type =:= null; Type =:= array; Type =:= object ->
    V.

get_type({Type, _}) ->
    Type.

execute_pred(V, PredExpr) ->
    PredResult = execute(replace_context(V, PredExpr)),
    case is_of_type({PredResult, bool}) of
        true ->
            get_value(PredResult);
        _ ->
            invalid_predicate
    end.

replace_context(V, it) ->
    V;

replace_context(_, {Type, Value}) when Type =:= numeric; Type =:= bool; Type =:= string; Type =:= null ->
    {Type, Value};

replace_context(V, {Type, Args}) ->
    {Type, lists:map(fun(X) -> replace_context(V, X) end, Args)}.
