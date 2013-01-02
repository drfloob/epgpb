
-module(epgpb_example_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    application:set_env(epgpb, pools, 
			[{pool1, [{size, 2}, {max_overflow, 0}]
			  , [{hostname, "127.0.0.1"},
			     {database, "db1"},
			     {username, "db1"},
			     {password, "abc123"}
			    ]}
			]),
    application:start(epgpb),
    {ok, { {one_for_one, 5, 10}, [?CHILD(epgpb_example_server, worker)]} }.

