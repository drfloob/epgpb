-module(epgpb_example_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}, 1000}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Here be the magic
handle_info(timeout, State) ->
    {ok, _, [{CountBin}]} = epgpb:squery(pool1, "select count(*) from test"),
    Count = list_to_integer(binary_to_list(CountBin)),
    error_logger:info_msg("Before insertion: ~p~n", [Count]),

    {ok, _Count} = epgpb:equery(pool1, "insert into test values($1)", [Count+1]),

    {ok, _, [{CountBin2}]} = epgpb:squery(pool1, "select count(*) from test"),
    Count2 = list_to_integer(binary_to_list(CountBin2)),
    error_logger:info_msg("After insertion: ~p~n", [Count2]),

    {stop, normal, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
