%% Author: jldupont
%% Created: 2009-09-24
%% Description:  erlnotify_drv
-module(erlnotify_drv).
-compile(export_all).

-define(DRV_BASE_PATH, "/usr/lib/erlnotify").
-define(DRV_DEFAULT,   "liberlnotify.so").
-define(DRV_BASE,      "liberlnotify").
-define(MIN_VERSION,   "0.1").  % Minimum version required

-define(TOOLS,         erlnotify_tools).


%% ----------------------      ------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%% API  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ----------------------      ------------------------------

start_drv() ->
    process_flag(trap_exit, true),
    Port = open_port({spawn, ExtPrg}, [{packet, 2}, binary, exit_status]),
	
	ok.


%% @doc Locates the most appropriate driver
%%		
find_drv() ->
	Ret=file:list_dir(?DRV_BASE_PATH),
	maybe_find_drv(Ret).


maybe_find_drv({ok, FileList}) ->
	FilteredList=?TOOLS:pattern_filter(?DRV_BASE, FileList),
	
	case lists:member(?DRV_DEFAULT, FilteredList) of
		true -> {ok, ?DRV_DEFAULT};
		_    ->	estimate_drv(FilteredList)
	end;

maybe_find_drv(_) ->
	{error, 'driver.not.found'}.



estimate_drv([]) ->
	{error, 'driver.not.found'};

%% @doc Finds the highest version number
%%		that is *at least* greater than the minimum required
%%
estimate_drv(FileList) ->
	
