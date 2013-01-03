-module(epgpb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_link(Pools) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Pools).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Pools} = application:get_env(epgpb, pools),
    init(Pools);
init(Pools) ->
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
				  PoolArgs = [{name, {local, Name}},
					      {worker_module, epgpb_worker}] ++ SizeArgs,
				  poolboy:child_spec(Name, PoolArgs, WorkerArgs)
			  end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.
    


