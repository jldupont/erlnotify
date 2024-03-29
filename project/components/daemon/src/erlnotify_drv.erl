%% Author: jldupont
%% Created: 2009-09-24
%% Description:  erlnotify_drv
-module(erlnotify_drv).
-compile(export_all).

-define(DRV_BASE_PATH, "/usr/lib/erlnotify").
-define(DRV_DEFAULT,   "liberlnotify.so").
-define(DRV_BASE,      "liberlnotify").
-define(MIN_VERSION,   "0.1").  % Minimum version required

-define(SERVER,        drv_server).
-define(TOOLS,         erlnotify_tools).
-define(TIMEOUT,       30*1000).


%% ----------------------      ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% API  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------      ------------------------------
start_link() ->
	Pid=spawn_link(?MODULE, server, []),
	register(?SERVER, Pid),
	Pid ! start,
	{ok, Pid}.


%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% LOCALS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------


%% @doc Client request server
%%		Requests are dispatched through 'handle_call'
%%
server() ->
	receive
		start ->
			start_drv();

		{_Port, {exit_status, _}} ->
			schedule_restart();

		
		{_Port, {data, Data}} ->
			handle_rx_drv(Data);
		
		_ -> noop
	end,
	server().


%% ----------------------           ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% HANDLERS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------           ------------------------------

handle_rx_drv(Data) ->
	ok.




%% ----------------------         ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% LOCALS  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------         ------------------------------

schedule_restart() ->
	timer:send_after(?TIMEOUT, start).


start_drv() ->
	Port=get(driver.port),
	maybe_start_drv(Port).



maybe_start_drv(undefined) ->
    process_flag(trap_exit, true),
	DriverPath=find_drv(),
	try_start_drv(DriverPath);

maybe_start_drv(_Port) ->
	put(driver.state, already_started),
	already_started. % hopefully ;-)




try_start_drv({ok,Path}) ->
	?TOOLS:addvar(driver.start), % stats
    Port = open_port({spawn, Path}, [{packet, 2}, binary, exit_status]),
	put(driver.state, started),
	put(driver.port, Port);

try_start_drv(_) ->
	put(driver.state, error),
	error.


%% @doc Locates the most appropriate driver
%%		
find_drv() ->
	?TOOLS:find_file(?DRV_BASE_PATH, ?DRV_BASE, ?DRV_DEFAULT, ?MIN_VERSION).

