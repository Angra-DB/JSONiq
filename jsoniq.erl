-module(jsoniq).
-compile(export_all).

execute(T = {Type, _}) when Type =:= numeric; Type =:= bool; Type =:= string; Type =:= null ->
    T;

execute({array, ExpL}) ->
    {array, lists:map(fun(X) -> execute(X) end, ExpL)};

execute({object, ExpL}) ->
    {object, list_to_tuple(lists:filtermap(fun(X) -> 
            case V = execute(X) =:= {} of
                false ->
                    {true, V};
                _ ->
                    false         
            end 
        end, ExpL))};

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

execute({gen_seq, Args}) ->
    ExprValues = [A, B] = execute_args(Args),
    case type_guard(ExprValues, [numeric, numeric]) of
        true ->
            {array, lists:seq(A, B)};
        false ->
            invalid_arguments
    end;

execute({concat, Args}) ->
    execute_op(Args, [string, string], fun(A, B) -> A ++ B end);

execute({add, Args}) ->
    execute_op(Args, [numeric, numeric], fun(A, B) -> A + B end);

execute({sub, Args}) ->
    execute_op(Args, [numeric, numeric], fun(A, B) -> A - B end);

execute({mult, Args}) ->
    execute_op(Args, [numeric, numeric], fun(A, B) -> A * B end);

execute({idiv, Args}) ->
    execute_op(Args, [numeric, numeric], fun(A, B) -> A div B end);

execute({mod, Args}) ->
    execute_op(Args, [numeric, numeric], fun(A, B) -> A rem B end);

execute({eq, Args}) ->
    execute_op(Args, [[numeric, string, null], [numeric, string, null]], fun(A, B) -> A =:= B end);

execute({ne, Args}) ->
    execute_op(Args, [any, any], fun(A, B) -> A =/= B end);
    
execute({le, Args}) ->
    execute_op(Args, [[numeric, null, string], [numeric, null, string]], fun le/2);
    
execute({lt, Args}) ->
    execute_op(Args, [[numeric, null, string], [numeric, null, string]], fun lt/2);

execute({ge, Args}) ->
    execute_op(Args, [[numeric, null, string], [numeric, null, string]], fun ge/2);
    
execute({gt, Args}) ->
    execute_op(Args, [[numeric, null, string], [numeric, null, string]], fun gt/2);

execute({'and', Args}) ->
    execute_op(Args, [bool, bool], fun(A, B) -> A and B end);
    
execute({'or', Args}) ->
    execute_op(Args, [bool, bool], fun(A, B) -> A or B end);

execute({'not', [Expr]}) ->
    ExprValue = execute(Expr),
    case is_of_type({ExprValue, bool}) of
        true ->
            not ExprValue;
        false ->
            invalid_argument    
    end.

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

is_comparable({_}) ->
    true;

is_comparable(V) -> 
    is_integer(V) orelse (V =:= null) orelse V orelse not V.

compare(A, B, Fun) ->
    case is_comparable(A) and is_comparable(B) of
        true ->
            Fun(A, B);
        _ ->
            not_atomic
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

access_field(ObjList, {Selector}) when is_list(Selector), is_list(ObjList) ->
    lists:map(fun(Obj) -> access_field(Obj, {Selector}) end, ObjList);

access_field(Obj, {Selector}) when is_list(Selector) -> 
    Proplist = 
        case Obj of
            {T} when is_list(T) ->
                not_object;
            _ ->
                tuple_to_list(Obj)
        end,
    proplists:get_value(Selector, Proplist, null).

access_array(Array, Index) when is_list(Array), is_integer(Index) ->
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
            {Type, ValueA} = A,
            {Type, ValueB} = B,
            {Type, OP(ValueA, ValueB)};
        false ->
            invalid_arguments
    end.