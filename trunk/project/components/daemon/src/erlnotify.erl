%% Author: jldupont
%% Created: 2009-09-24
%% Description: erlnotify main entry point 
%%
-module(erlnotify).

-define(SERVER,  erlnotify).
-define(TIMEOUT, 1000). % in ms
-define(TOOLS,  erlnotify_tools).

%%
%% Exported Functions
%%
-export([
		 api/1
		 ]).

%% ----------------------      ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% API  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------      ------------------------------

api({notif, Timeout, Title, Msg}) ->
	rpc({notif, Timeout, Title, Msg});
	

api(Other) ->
	{error, {invalid_method_call, Other}}.


%% ----------------------               ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% REMOTE ACCESS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------               ------------------------------

rpc(Message) ->
	Fnode=?TOOLS:make_node(node()),
	dorpc(Fnode, Message).


%% The Client must be running in a register node() context
%% or else the RPC does not work.
%%
dorpc(nohost, _) ->
	{error, node.required};


%% @private
dorpc(Fnode, Message) ->
	FromNode=node(),
	RemoteNode=?TOOLS:make_node(?SERVER),
	
	io:format("dorpc: Fnode[~p] Message[~p]~n", [Fnode, Message]),
	
	%% If the daemon is down, this call will fail first and thus
	%% {error, node_down} will be received by the caller
	case rpc:call(RemoteNode, erlnotify_server, call, [FromNode, Message], ?TIMEOUT) of
		{badrpc, Reason} ->
			io:format("~p: dorpc: badrpc: ~p~n", [?MODULE, Reason]),
			{error, node_down};
		
		Other ->
			Other
	end.
