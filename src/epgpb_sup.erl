-module(epgpb_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, Pools} = application:get_env(epgpb, pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
				  PoolArgs = [{name, {local, Name}},
					      {worker_module, epgpb_worker}] ++ SizeArgs,
				  poolboy:child_spec(Name, PoolArgs, WorkerArgs)
			  end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

