%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc Normal rules implementation module for 'cgolam' app.


-module(cgolam_rules_normal).


-behaviour(cgolam_rules).


-export([new/1, calc/4, init/4]).


-record(cgolam_rules_normal, {
	field_mod :: module()
}).


-type cgolam_rules_normal() :: cgolam_rules:rules() .


-export_type([cgolam_rules_normal/0]).


%% @doc Create a new ruler.

-spec new
	(RulesModCfg :: list()) ->
		cgolam_rules_normal() .

new(RulesModCfg) ->
	FieldMod = case lists:keysearch(field, 1, RulesModCfg) of
		{value, {field, M}} -> M;
		{value, {field, M, _C}} -> M
		end,
	#cgolam_rules_normal{
		field_mod = FieldMod
	}
.


%% @doc Calculate a new cell state.

-spec calc
	(Rules :: cgolam_rules_normal(), Field :: cgolam_field:field(), X :: integer(), Y :: integer()) ->
		CellState :: term() . 

calc(#cgolam_rules_normal{field_mod=FieldMod}, Field, X, Y) ->
	AliveCount = lists:foldl(
		fun ({Xdiff, Ydiff}, Count) ->
			case FieldMod:get(Field, X + Xdiff, Y + Ydiff) of
				false -> Count;
				_Alive -> Count + 1
				end
			end,
		0,
		[{-1, -1}, {0, -1}, {1, -1},
			{-1, 0},         {1, 0},
			{-1, 1}, {0, 1}, {1, 1}
			]
	),
	if AliveCount > 3 -> false;
		AliveCount < 2 -> false;
		AliveCount == 3 -> true;
		AliveCount == 2 -> FieldMod:get(Field, X, Y)
		end
.


%% @doc Initialise a field.

-spec init
	(Rules :: cgolam_rules_normal(), Field0 :: cgolam_field:field(), Type :: atom(), InitCfg :: list()) ->
		Field1 :: cgolam_field:field() .

init(#cgolam_rules_normal{field_mod=FieldMod}, Field0, default, InitCfg) ->
	Width = FieldMod:width(Field0),
	Height = FieldMod:height(Field0),
	Clusters = case lists:keysearch(clusters, 1, InitCfg) of
		{value, {clusters, I}} -> I;
		false -> 4
		end,
	ClusterSizeCfg = case lists:keysearch(cluster_size, 1, InitCfg) of
		{value, {cluster_size, CSC}} -> CSC / 100;
		false -> 1
		end,
	ClusterDensityCfg = case lists:keysearch(cluster_density, 1, InitCfg) of
		{value, {cluster_density, CDC}} -> CDC / 100;
		false -> 1
		end,
	ClusterSize = trunc(math:sqrt(Width * Height) / 2 * ClusterSizeCfg),
	ClusterDensity = trunc(ClusterSize * ClusterSize / 20 * ClusterDensityCfg),
	lists:foldl(
		fun(_, Field01Acc) ->
			ClusterX = trunc(rand:uniform(Width)),
			ClusterY = trunc(rand:uniform(Height)),
			lists:foldl(
				fun(_, Field02Acc) ->
					CellX = trunc((rand:uniform()-0.5)*(rand:uniform()-0.5) * ClusterSize) + ClusterX,
					CellY = trunc((rand:uniform()-0.5)*(rand:uniform()-0.5) * ClusterSize) + ClusterY,
					FieldMod:set(Field02Acc, CellX, CellY, true)
					end,
				Field01Acc,
				lists:seq(1, ClusterDensity)
			)
			end,
		Field0,
		lists:seq(1, Clusters)
	)
.
