-module(epgpb_worker).
-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-record(state, {conn}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    Hostname = proplists:get_value(hostname, Args),
    Database = proplists:get_value(database, Args),
    Username = proplists:get_value(username, Args),
    Password = proplists:get_value(password, Args),
    {ok, Conn} = pgsql:connect(Hostname, Username, Password, [
        {database, Database}
    ]),

    %% user-specific setup
    SetupFun = proplists:get_value(setup, Args),
    case SetupFun of
	undefined -> ok;
	{Mod, Fun} -> erlang:apply(Mod, Fun, [Conn, Args])
    end,

    {ok, #state{conn=Conn}}.



handle_call({get_parameter, Name}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:get_parameter(Conn, Name), State};

handle_call({squery, Sql}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:squery(Conn, Sql), State};

handle_call({equery, Sql}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:equery(Conn, Sql), State};

handle_call({equery, Sql, Parameters}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:equery(Conn, Sql, Parameters), State};

handle_call({parse, Sql}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:parse(Conn, Sql), State};

handle_call({parse, Sql, Types}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:parse(Conn, Sql, Types), State};

handle_call({parse, Name, Sql, Types}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:parse(Conn, Name, Sql, Types), State};

handle_call({bind, Statement, Parameters}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:bind(Conn, Statement, Parameters), State};

handle_call({bind, Statement, PortalName, Parameters}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:bind(Conn, Statement, PortalName, Parameters), State};

handle_call({execute, S}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:execute(Conn, S), State};

handle_call({execute, S, N}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:execute(Conn, S, N), State};

handle_call({execute, S, PortalName, N}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:execute(Conn, S, PortalName, N), State};

handle_call({describe, X0}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:describe(Conn, X0), State};

handle_call({describe, Type, Name}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:describe(Conn, Type, Name), State};

handle_call({sync}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:sync(Conn), State};

handle_call({with_transaction, F}, _From, #state{conn=Conn}=State) ->
    {reply, pgsql:with_transaction(Conn, F), State};


handle_call(_Other, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{conn=Conn}) ->
    ok = pgsql:close(Conn),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
