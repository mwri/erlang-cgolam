%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc 'Coloured' rules implementation module for 'cgolam' app.


-module(cgolam_rules_coloured).


-behaviour(cgolam_rules).


-export([new/1, calc/4, init/4]).


-record(cgolam_rules_coloured, {
	field_mod :: module()
}).


-type cgolam_rules_coloured() :: cgolam_rules:rules() .


-export_type([cgolam_rules_coloured/0]).


%% @private

-spec new
	(RulesModCfg :: list()) ->
		cgolam_rules_coloured() .

new(RulesModCfg) ->
	FieldMod = case lists:keysearch(field, 1, RulesModCfg) of
		{value, {field, M}} -> M;
		{value, {field, M, _C}} -> M
		end,
	#cgolam_rules_coloured{
		field_mod = FieldMod
	}
.


%% @private

-spec calc
	(Rules :: cgolam_rules_coloured(), Field :: cgolam_field:field(), X :: integer(), Y :: integer()) ->
		CellState :: term() .

calc(#cgolam_rules_coloured{field_mod=FieldMod}, Field, X, Y) ->
	AliveCellStates = lists:foldl(
		fun ({Xdiff, Ydiff}, AliveCellStatesAcc) ->
			case FieldMod:get(Field, X + Xdiff, Y + Ydiff) of
				false -> AliveCellStatesAcc;
				Alive -> [Alive | AliveCellStatesAcc]
				end
			end,
		[],
		[{-1, -1}, {0, -1}, {1, -1},
			{-1, 0},         {1, 0},
			{-1, 1}, {0, 1}, {1, 1}
			]
	),
	AliveCount = length(AliveCellStates),
	if AliveCount > 3 -> false;
		AliveCount < 2 -> false;
		AliveCount == 3 -> raise_brightness(merge_cellstates(AliveCellStates));
		AliveCount == 2 -> FieldMod:get(Field, X, Y)
		end
.


%% @private

merge_cellstates([{col, {R, G, B}} | T]) ->
	merge_cellstates(R, G, B, T, 1)
.

merge_cellstates(Rs, Gs, Bs, [{col, {R, G, B}} | T], N) ->
	merge_cellstates(Rs + R, Gs + G, Bs + B, T, N + 1)
;
merge_cellstates(Rs, Gs, Bs, [], N) ->
	{col, {trunc(Rs/N), trunc(Gs/N), trunc(Bs/N)}}
.


raise_brightness({col, {R, G, B}}) when (R >= G) and (R >= B) ->
	raise_brightness({col, {R, G, B}}, 255 / R)
;
raise_brightness({col, {R, G, B}}) when (G >= R) and (G >= B) ->
	raise_brightness({col, {R, G, B}}, 255 / G)
;
raise_brightness({col, {R, G, B}}) when (B >= R) and (B >= G) ->
	raise_brightness({col, {R, G, B}}, 255 / B)
.

raise_brightness({col, {R, G, B}}, F) ->
	{col, {trunc(R*F), trunc(G*F), trunc(B*F)}}
.


%% @private

-spec init
	(Rules :: cgolam_rules_coloured(), Field0 :: cgolam_field:field(), Type :: atom(), InitCfg :: list()) ->
		Field1 :: cgolam_field:field() .

init(#cgolam_rules_coloured{field_mod=FieldMod}, Field0, default, InitCfg) ->
	Width = FieldMod:width(Field0),
	Height = FieldMod:height(Field0),
	Clusters = case lists:keysearch(clusters, 1, InitCfg) of
		{value, {clusters, I}} -> I;
		false -> 3
		end,
	ClusterSizeCfg = case lists:keysearch(cluster_size, 1, InitCfg) of
		{value, {cluster_size, CSC}} -> CSC / 100;
		false -> 1
		end,
	ClusterDensityCfg = case lists:keysearch(cluster_density, 1, InitCfg) of
		{value, {cluster_density, CDC}} -> CDC / 100;
		false -> 1
		end,
	ClusterCols = [{255,0,0}, {0,255,0}, {0,0,255}, {255,255,0}, {255,0,255}, {0,255,255}],
	ClusterSize = trunc(math:sqrt(Width * Height) / 2 * ClusterSizeCfg),
	ClusterDensity = trunc(ClusterSize * ClusterSize / 20 * ClusterDensityCfg),
	{Field1, _RemainingCols} = lists:foldl(
		fun (_, {Field01Acc, [Col | ClusterCols01AccT]}) ->
				% per cluster, accumulator is field and depleting colour selection
				ClusterX = trunc(rand:uniform(Width)),
				ClusterY = trunc(rand:uniform(Height)),
				Field01Acc2 = lists:foldl(
					fun (_, Field02Acc) ->
						% per cell in cluster, 
						CellX = trunc((rand:uniform()-0.5)*(rand:uniform()-0.5) * ClusterSize) + ClusterX,
						CellY = trunc((rand:uniform()-0.5)*(rand:uniform()-0.5) * ClusterSize) + ClusterY,
						FieldMod:set(Field02Acc, CellX, CellY, {col, Col})
						end,
					Field01Acc,
					lists:seq(1, ClusterDensity)
				),
				{Field01Acc2, ClusterCols01AccT};
		    (_, {Field01Acc, []}) ->
				% cycle round colours if run out
				{Field01Acc, ClusterCols}
				end,
		{Field0, ClusterCols},
		lists:seq(1, Clusters)
	),
	Field1
;

init(#cgolam_rules_coloured{field_mod=FieldMod}, Field0, term, InitCfg) ->
	{value, {set, InitTerm}} = lists:keysearch(set, 1, InitCfg),
	lists:foldl(
		fun ({{X, Y}, Col = {col, _RGB}}, Field0Acc) ->
			FieldMod:set(Field0Acc, X, Y, Col)
			end,
		Field0,
		InitTerm
	)
.
