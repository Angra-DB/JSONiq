% ------------------------------------------------
% @author Rodrigo Bonifacio <rbonifacio@unb.br>
%
% @doc a first attempt to build the Angra-DB server
%
% @end
% ------------------------------------------------



-module(jsoniq_server).
-include_lib("eunit/include/eunit.hrl").

-behavior(gen_server).

%
% API functions
%

-export([ start_link/1
	, get_count/0   % we can understand both get_count and stop as adm operations
	, stop/0]).

% gen_server callbacks
-export([ init/1
	, handle_call/3
	, handle_cast/2
	, handle_info/2
	, terminate/2
	, code_change/3]).

-define(SERVER, ?MODULE).      % declares a SERVER macro constant (?MODULE is the module's name)

-record(state, {lsock, parent, adb_socket}). % a record for keeping the server state

%%%======================================================
%%% API
%%%
%%% Each one of the functions that appear in the
%%% API section, calls one of the gen_server library
%%% functions (start_link/4, call/2, cast/2)... This
%%% is a bit trick.
%%%======================================================

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

get_count() ->
    gen_server:call(?SERVER, get_count).

stop() ->
    gen_server:cast(?SERVER, stop).

%%%===========================================
%%% gen_server callbacks
%%%===========================================

init([LSock]) ->
    lager:info("starting new jsoniq server"),
    {ok, #state{lsock = LSock, adb_socket = connect()}, 0}.

handle_call(Msg, _From, State) ->
    {reply, {ok, Msg}, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info({tcp, Socket, RawData}, State = #state{adb_socket = AdbSock}) ->
    Result = jsoniq:run_query(preprocess(RawData), AdbSock),
    Flatten = lists:flatten(io_lib:fwrite("~p~n", [Result])),
    gen_tcp:send(Socket, io_lib:fwrite("~p ~s", [length(Flatten), Flatten])),
    {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info(timeout, #state{lsock = LSock} = State) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    jsoniq_sup:start_child(),
    {noreply, State#state{ lsock = Sock }}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


% Util functions

connect() ->
    case gen_tcp:connect({127,0,0,1}, 1234, [binary, {active, false}]) of
        {ok, Socket} -> 
            Socket;			 
        _ -> 
            throw(could_not_connect_to_the_server)
    end.

preprocess(RawData) -> 
    _reverse = lists:reverse(RawData),
    Pred = fun(C) -> (C == $\n) or (C == $\r) end,
    lists:reverse(lists:dropwhile(Pred, _reverse)).