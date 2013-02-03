-module(epgpb_example_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    application:set_env(epgpb, pools, 
			[{pool1, [{size, 2}, {max_overflow, 0}]
			  , [{hostname, "127.0.0.1"},
			     {database, "db1"},
			     {username, "db1"},
			     {password, "abc123"},
			     {setup, {epgpb_example_server, setup}}
			    ]}
			]),
    application:start(epgpb),

    epgpb_example_sup:start_link().

stop(_State) ->
    ok.
