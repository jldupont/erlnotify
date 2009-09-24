%% Author: jldupont
%% Created: 2009-09-24
%% Description: erlnotify_server
-module(erlnotify_server).

-define(SERVER,  erlnotify).
-define(TIMEOUT, 1000).
-define(API,     erlnotify_api).

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
		?SERVER ! {call, self(), FromNode, Q},
		
		%% WAIT FOR A RESPONSE  or  TIMEOUT
		%% ================================
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


%% rpc reply mechanism
%% @private
reply(To, Message) ->
	try
		To ! {reply, self(), Message}
	catch
		
		% if we get here, something else is going to
		% break anyway... we'll catch it upstream.
		_:_ ->
			noop
	end.


%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% LOCALS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------


%% @doc Client request server
%%		Requests are dispatched through 'handle_call'
%%
server() ->
	receive
		{call, Fpid, _Fnode, Q} ->
			handle_call(Fpid, Q);
		
		_ ->
			ok
	end,
	server().


%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%   CALL   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%% HANDLERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------


handle_call(_Fpid=Client, {notif, Params}) ->
	?API:notif(Client, Params);
	


handle_call(Fpid, Q) ->
	reply(Fpid, {error, {invalid.method, Q}}).



