%% Author: jldupont
%% Created: 2009-09-24
%% Description: erlnotify_api
-module(erlnotify_api).
-compile(export_all).

-define(API_SERVER, erlnotify_api_server).


%%
%% Exported Functions
%%
-export([
		 notif/2
	   
	   ,start_link/0
		 ]).




%% ----------------------      ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% API  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------      ------------------------------

notif(Client, {Timeout, Title, Msg}) ->
	doreq(Client, {notif, {Timeout, Title, Msg}});
	

notif(Client, _) ->
	reply(Client, {error, invalid.parameters}).




%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% SERVER  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------

start_link() ->
	Pid = spawn(?MODULE, server, []),
	register(?API_SERVER, Pid),
	Pid ! start,
	{ok, Pid}.


server() ->
	receive
		start ->
			ok;
		
		{req, Client, R} ->
			handle_request(Client, R);
		
		_ -> noop
	
	end,
	server().



%% ----------------------          ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% HANDLERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------          ------------------------------

handle_request(Client, R) ->
	ok.




%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% HELPERS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------

doreq(Client, R) ->
	try
		?API_SERVER ! {req, Client, R}
	catch
		_:_ ->
			reply(Client, {error, api.server.down})
	end.


%% rpc reply mechanism
%% @private
reply(To, Message) ->
	try
		To ! {reply, self(), Message}
	catch
		% if we get here, something else is going to
		% break anyway... we'll catch it upstream.
		_:_ ->	noop
	end.

