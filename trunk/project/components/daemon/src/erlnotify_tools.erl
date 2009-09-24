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
	
	
	
	
	