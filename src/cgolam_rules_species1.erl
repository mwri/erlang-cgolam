%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%%
%% @doc 'Species1' rules implementation module for 'cgolam' app.
%%
%% This is SIMILAR to the coloured version of Conway's Game of
%% Life, but here the different colours are relabelled species
%% as they are broadly UNcooperative.
%%
%% The calculation of a cells new 'alive' state is done as follows...
%%
%% 1. The surrounding eight cells, and the mid cell, are examined
%% and all the unique colours (up to nine) are determined. What is
%% considered unique depends on a tolerance, based on a bitmap, so
%% a number of lower order bits may be discarded for the purpose of
%% this comparison.
%%
%% 2. For each unique colour, CGoL (Conway's Game of life) rules
%% are applied, counting only those surrounding cells which are
%% the same colour (given the above mentioned tolerance).
%%
%% 3. If the CGoL rules return dead for all colours, the cell
%% becomes dead.
%%
%% 4. If the CGoL rules return alive for one or more colours and
%% one of the colours is the same (given the above mentioned
%% tolerance) as the mid cell, then the cell remains unchanged.
%%
%% 5. If the CGoL rules return alive for one or more colours and
%% the mid cell is dead or none of the colours is the same (given
%% the above mentionedtolerance) as the mid cell, then the new
%% cell colour is calculated by averaging the colours of the
%% surrounding cells (for which the CGoL rules return alive)
%% (these cells are chosen using the tolerance mentioned above
%% but the actual unchanged cell colours are used in the average).
%% The brightness level of the resultant calculated average
%% colour will be adjusted (all RGB components multipled by a
%% common factor) so that at least one component is 255.
%%
%% With this algorithm, any lone pure colour should behave
%% entirely like the standard CGoL, but when colours collide it
%% gets more interesting.


-module(cgolam_rules_species1).


-behaviour(cgolam_rules).


-export([new/1, calc/4, init/4]).


-record(cgolam_rules_species1, {
	field_mod   :: module(),
	colmatch_bm :: integer()
}).


-type cgolam_rules_species1() :: cgolam_rules:rules() .


-export_type([cgolam_rules_species1/0]).


%% @private

-spec new
	(RulesModCfg :: list()) ->
		cgolam_rules_species1() .

new(RulesModCfg) ->
	FieldMod = case lists:keysearch(field, 1, RulesModCfg) of
		{value, {field, M}} -> M;
		{value, {field, M, _C}} -> M
		end,
	ColMatchBitmask = case lists:keysearch(colmatch_bitmask, 1, RulesModCfg) of
		{value, {colmatch_bitmask, BM}} -> BM;
		false -> 16#80
		end,
	#cgolam_rules_species1{
		field_mod   = FieldMod,
		colmatch_bm = ColMatchBitmask
	}
.


%% @private

-spec calc
	(Rules :: cgolam_rules_species1(), Field :: cgolam_field:field(), X :: integer(), Y :: integer()) ->
		CellState :: term() .

calc(#cgolam_rules_species1{
		field_mod   = FieldMod,
		colmatch_bm = ColMatchBitmask
		}, Field, X, Y) ->
	SurroundingRGBs = lists:foldl(
		fun ({Xdiff, Ydiff}, SurroundingRGBsAcc) ->
			case FieldMod:get(Field, X + Xdiff, Y + Ydiff) of
				false -> SurroundingRGBsAcc;
				{col, RGB} -> [RGB | SurroundingRGBsAcc]
				end
			end,
		[],
		[{-1, -1}, {0, -1}, {1, -1},
			{-1, 0},         {1, 0},
			{-1, 1}, {0, 1}, {1, 1}]
	),
	MidCol = FieldMod:get(Field, X, Y),
	MidRGB = case MidCol of
		false -> false;
		{col, RGB} -> RGB
		end,
	UniqueRGBs = lists:foldl(
		fun (RGB, UniqueRGBsAcc) ->
			Repeat = lists:any(
				fun (UniqueRGB) -> tolerated_same(RGB, UniqueRGB, ColMatchBitmask) /= false end,
				UniqueRGBsAcc
			),
			if Repeat -> UniqueRGBsAcc; true -> [RGB | UniqueRGBsAcc] end
			end,
		[],
		case MidCol of
			{col, MidRGB} -> [MidRGB | SurroundingRGBs];
			false -> SurroundingRGBs
			end
	),
	{ContendingRGBs, MidRGBContending} = lists:foldl(
		fun (RGB, {ContendingRGBsAcc, MidCellRGBContending}) ->
			SurroundingSameRGBs = [
				SurroundingRGB ||
					SurroundingRGB <- SurroundingRGBs,
					tolerated_same(SurroundingRGB, RGB, ColMatchBitmask) /= false
				],
			case cgol_rule(length(SurroundingSameRGBs)) of
				true ->
					{[{RGB, SurroundingSameRGBs} | ContendingRGBsAcc], MidCellRGBContending};
				false ->
					{ContendingRGBsAcc, MidCellRGBContending};
				unchanged when MidRGB == false ->
					{ContendingRGBsAcc, MidCellRGBContending};
				unchanged ->
					case tolerated_same(MidRGB, RGB, ColMatchBitmask) of
						false -> {ContendingRGBsAcc, MidCellRGBContending};
						_Alive -> {[{RGB, SurroundingSameRGBs} | ContendingRGBsAcc], true}
						end
				end
			end,
		{[], false},
		UniqueRGBs
	),
	if ContendingRGBs == [] ->
			false;
		MidRGBContending ->
			MidCol;
		true ->
			{col, adjust_brightness(
				merge_cellstates(
					lists:flatten(
						[Contribs || {_RGB, Contribs} <- ContendingRGBs]
					)
				)
			)}
		end
.


%% @private

tolerated_same({Ra, Ga, Ba}, {Rb, Gb, Bb}, ColMatchBitmask)
		when ((Ra band ColMatchBitmask) == (Rb band ColMatchBitmask))
			and ((Ga band ColMatchBitmask) == (Gb band ColMatchBitmask))
			and ((Ba band ColMatchBitmask) == (Bb band ColMatchBitmask)) ->
	{Ra band ColMatchBitmask, Ga band ColMatchBitmask, Ba band ColMatchBitmask}
;

tolerated_same(_A, _B, _ColMatchBitmask) ->
	false
.


%% @private

cgol_rule(SurroundingCells) ->
	if SurroundingCells > 3 -> false;
		SurroundingCells < 2 -> false;
		SurroundingCells == 3 -> true;
		SurroundingCells == 2 -> unchanged
		end
.


%% @private

merge_cellstates([{R, G, B} | T]) ->
	merge_cellstates(R, G, B, T, 1)
.

merge_cellstates(Rs, Gs, Bs, [{R, G, B} | T], N) ->
	merge_cellstates(Rs + R, Gs + G, Bs + B, T, N + 1)
;
merge_cellstates(Rs, Gs, Bs, [], N) ->
	{trunc(Rs/N), trunc(Gs/N), trunc(Bs/N)}
.


adjust_brightness({R, G, B}) when (R >= G) and (R >= B) and (R /= 0) ->
	adjust_brightness({R, G, B}, 255 / R)
;
adjust_brightness({R, G, B}) when (G >= R) and (G >= B) and (G /= 0) ->
	adjust_brightness({R, G, B}, 255 / G)
;
adjust_brightness({R, G, B}) when (B >= R) and (B >= G) and (B /= 0) ->
	adjust_brightness({R, G, B}, 255 / B)
;
adjust_brightness({0, 0, 0}) ->
	{0, 0, 0}
.

adjust_brightness({R, G, B}, F) ->
	{trunc(R*F), trunc(G*F), trunc(B*F)}
.


%% @private

-spec init
	(Rules :: cgolam_rules_species1(), Field0 :: cgolam_field:field(), Type :: atom(), InitCfg :: list()) ->
		Field1 :: cgolam_field:field() .

init(#cgolam_rules_species1{field_mod=FieldMod}, Field0, default, InitCfg) ->
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

init(#cgolam_rules_species1{field_mod=FieldMod}, Field0, term, InitCfg) ->
	{value, {set, InitTerm}} = lists:keysearch(set, 1, InitCfg),
	lists:foldl(
		fun ({{X, Y}, Col = {col, _RGB}}, Field0Acc) ->
			FieldMod:set(Field0Acc, X, Y, Col)
			end,
		Field0,
		InitTerm
	)
.
