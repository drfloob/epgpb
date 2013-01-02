-module(epgpb).
-export([get_parameter/2, squery/2, equery/2, equery/3, parse/2, parse/3, parse/4, describe/2, describe/3, bind/3, bind/4, execute/2, execute/3, execute/4, sync/1, with_transaction/2]).


get_parameter(PoolName, Name) ->
    poolboy:transaction(PoolName, 
		 	fun(W) -> 
		 		gen_server:call(W, {get_parameter, Name}) 
		 	end).

squery(PoolName, Sql) ->
    poolboy:transaction(PoolName, 
		 	fun(W) -> 
		 		gen_server:call(W, {squery, Sql}) 
		 	end).

equery(PoolName, Sql) ->
    poolboy:transaction(PoolName, 
		 	fun(W) -> 
		 		gen_server:call(W, {equery, Sql}) 
		 	end).

equery(PoolName, Sql, Parameters) ->
    poolboy:transaction(PoolName, 
		 	fun(W) -> 
		 		gen_server:call(W, {equery, Sql, Parameters}) 
		 	end).

parse(PoolName, Sql) ->
    poolboy:transaction(PoolName, 
		 	fun(W) -> 
		 		gen_server:call(W, {parse, Sql}) 
		 	end).

parse(PoolName, Sql, Types) ->
    poolboy:transaction(PoolName, 
		 	fun(W) -> 
		 		gen_server:call(W, {parse, Sql, Types}) 
		 	end).

parse(PoolName, Name, Sql, Types) ->
    poolboy:transaction(PoolName, 
		 	fun(W) -> 
		 		gen_server:call(W, {parse, Name, Sql, Types}) 
		 	end).

bind(PoolName, Statement, Parameters) ->
    poolboy:transaction(PoolName, 
		 	fun(W) -> 
		 		gen_server:call(W, {bind, Statement, Parameters}) 
		 	end).

bind(PoolName, Statement, PortalName, Parameters) ->
    poolboy:transaction(PoolName, 
		 	fun(W) -> 
		 		gen_server:call(W, {bind, Statement, PortalName, Parameters}) 
		 	end).

execute(PoolName, S) ->
    poolboy:transaction(PoolName, 
		 	fun(W) -> 
		 		gen_server:call(W, {execute, S}) 
		 	end).

execute(PoolName, S, N) ->
    poolboy:transaction(PoolName, 
		 	fun(W) -> 
		 		gen_server:call(W, {execute, S, N}) 
		 	end).

execute(PoolName, S, PortalName, N) ->
    poolboy:transaction(PoolName, 
		 	fun(W) -> 
		 		gen_server:call(W, {execute, S, PortalName, N}) 
		 	end).

describe(PoolName, X0) ->
    poolboy:transaction(PoolName, 
		 	fun(W) -> 
		 		gen_server:call(W, {describe, X0}) 
		 	end).

describe(PoolName, Type, Name) ->
    poolboy:transaction(PoolName, 
		 	fun(W) -> 
		 		gen_server:call(W, {describe, Type, Name}) 
		 	end).

sync(PoolName) ->
    poolboy:transaction(PoolName, 
		 	fun(W) -> 
		 		gen_server:call(W, {sync}) 
		 	end).

with_transaction(PoolName, F) ->
    poolboy:transaction(PoolName, 
		 	fun(W) -> 
		 		gen_server:call(W, {with_transaction, F}) 
		 	end).