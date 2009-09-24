%% Author: jldupont
%% Created: 2009-09-24
%% Description:  erlnotify_tools
-module(erlnotify_tools).
-compile(export_all).


%% @doc Makes a "short name" node from a Name
%%		e.g. Name@Node
%%
%% @spec make_node(Name) -> string()
%% where
%%	Name=atom()
%%
make_node(Name) ->
	make_node(Name, node()).

%% @private
make_node(Name, Node) when is_atom(Name) ->
	make_node(erlang:atom_to_list(Name), Node);

make_node(Name , Node) when is_list(Name) ->
	Host=extract_host(Node),
	PartialName=string:concat(Name, "@"),
	CompleteName=string:concat(PartialName, Host),
	erlang:list_to_atom(CompleteName).


%% @doc Extracts the "host" part of the running node
%% 		i.e.  Host@Node
%%
%% @spec extract_host() -> string()
%%
extract_host() ->
	extract_host(node()).

%% @doc Extracts the "host" part of a "short-name" node name
%%		e.g. Host@Node
%%
%% @spec extract_host(Node) -> string()
%% where
%%	Node = atom()
%%
extract_host(Node) when is_atom(Node) ->
	extract_host(atom_to_list(Node));

extract_host(Node) when is_list(Node) ->
	Tokens = string:tokens(Node, "@"),
	lists:last(Tokens).



%% @doc Very permissive pattern based filter
%%
pattern_filter(Pattern, List) ->
	pattern_filter(Pattern, List, []).

pattern_filter(_Pattern, [], Acc) ->
	Acc;

pattern_filter(Pattern, [H|T], Acc) when is_list(Pattern) ->
	
	case string:str(H, Pattern) of
		0 -> pattern_filter(Pattern, T, Acc);
		_ -> pattern_filter(Pattern, T, Acc++[H])
	end;
	
pattern_filter(_, _, Acc) ->
	Acc.
	



extract_version(File) when is_list(File) ->
	Tokens=string:tokens(File, "-"),
	maybe_extract_version(Tokens).


maybe_extract_version([_FileName, Version]) ->
	Version;
	
maybe_extract_version(_) ->
	undefined.
	


%% @doc Compare version information
%%
compare_version(Min, Version) ->
	MinF=tofloat(Min),
	VersionF=tofloat(Version),
	cv(MinF, VersionF).


cv(error, _) -> error;
cv(_, error) -> error;

cv(MinF, VersionF) ->
	VersionF > MinF.
	

tofloat(N) when is_list(N) ->
	case string:to_float(N) of
		{error, _} -> 
			case string:to_integer(N) of
				{error, _} -> error;
				{Int, _}   -> 1.0*Int
			end;
		{FN, _}    -> FN
	end;
tofloat(N) when is_float(N)   -> N;
tofloat(N) when is_integer(N) -> 1.0*N;
tofloat(_) -> error.



%% @doc Finds a file of BaseName in the directory BasePath 
%%		having a version number of at least MinVersion.
%%		If the file Default is found, return it regardeless.
%% 
%% @spec find_file(BasePath, BaseName, Default, MinVersion) -> {ok, FilePath} | {error, Reason}
%% where
%%	BasePath = string()
%%	BaseName = string()
%%	Default  = string()
%%	MinVersion = string() | float() | integer()
%%	Reason = term()
%%
find_file(BasePath, BaseName, Default, MinVersion) ->
	Ret=file:list_dir(BasePath),
	maybe_find_file(BasePath, BaseName, Default, MinVersion, Ret).


maybe_find_file(BasePath, BaseName, Default, MinVersion, {ok, FileList}) ->
	FilteredList=pattern_filter(BaseName, FileList),
	
	case lists:member(Default, FilteredList) of
		true -> {ok, BasePath++"/"++Default};
		_    ->	estimate_file(MinVersion, FilteredList)
	end;

maybe_find_file(_, _, _, _, _) ->
	{error, 'file.not.found'}.



estimate_file(_, []) ->
	{error, 'file.not.found'};

%% @doc Finds the highest version number
%%		that is *at least* greater than the minimum required
%%
estimate_file(MinVersion, [File|Rest]=_FileList) ->
	Version=extract_version(File),
	case compare_version(MinVersion, Version) of
		true -> {ok, File};
		false-> estimate_file(MinVersion, Rest)
	end;

estimate_file(_, _) ->
	{error, 'file.not.found'}.



%% @doc Adds 1 to the current value of Var
%%
addvar(Var) when is_atom(Var) -> addvar(Var, 1);
addvar(_) -> error.


%% @doc Adds Count to the current value of Var
%%
addvar(Var, Count) when is_atom(Var) and is_integer(Count) ->
	addvar(Var, get(Var), Count);

addvar(_,_) ->
	error.


addvar(Var, undefined, Count) -> put(Var, Count);
addvar(Var, Value, Count)     -> put(Var, Value+Count).
