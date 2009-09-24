%% Author: jldupont
%% Created: 2009-09-24
%% Description: erlnotify_server
-module(erlnotify_server).

-define(SERVER,  erlnotify).
-define(TIMEOUT, 1000).

%%
%% Exported Functions
%%
-export([
		 start_link/0
		,call/2
		
		,server/0
		 ]).

%% ----------------------      ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% API  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------      ------------------------------
start_link() ->
	Pid = spawn(?MODULE, server, []),
	register(?SERVER, Pid),
	{ok, Pid}.



%% @doc Calls from a Client end-up here
%%
%%
call(FromNode, Q) ->
	%%io:format("call: from[~p] Q[~p]~n", [FromNode, Q]),
	%%?TOOLS:msg("rpc: From[~p] Message[~p]", [FromNode, Q]),
	try
		?SERVER ! {self(), {FromNode, Q}},
		receive
			{reply, ServerPid, Reply} ->
				{ServerPid, Reply};
	
			Other ->
				%% Internal error... bug!
				{error, {internal.error, call.msg, Other}}
	
		after ?TIMEOUT ->
			{error, rpc_timeout}
		end
	catch
		_X:_Y ->
			{error, server.down}
	end.



%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% LOCALS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------

server() ->
	receive
		ok ->
			ok
	end,
	server().


