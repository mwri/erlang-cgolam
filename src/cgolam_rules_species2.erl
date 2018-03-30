%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc 'Species2' rules implementation module for 'cgolam' app.
%%
%% This is based on 'Species1', but v1, whilst it did work, it
%% generally only yielded alternative colour static structures, and
%% only occasionally at that! I did ONCE see an active mix colour
%% group establish but this is obviously a very infrequent event.
%% The different species ignored each other very effectively, and
%% in practise it met all the intended design goals, except being
%% remotely interesting! Depressingly it's probably relatively
%% realistic, I mean inter species development is improbable...
%% But if it's not interesting, what's the point?
%%
%% The five stage algorithm of v1 is therefore implemented in
%% v2, but with the one modification; the presence of other
%% colours/species is taken into account for over crowding
%% considerations.
%%
%% The result is an even more surpassingly dull simulation as
%% species now hate each other too much to get close enough to
%% mate. Even with the extra over crowd limit set to 5 for the
%% extra multi species over crowd check, it's just not a good
%% algorithm.


-module(cgolam_rules_species2).


-behaviour(cgolam_rules).


-export([new/1, calc/4, init/4]).


-record(cgolam_rules_species2, {
	field_mod   :: module(),
	colmatch_bm :: integer()
}).


-type cgolam_rules_species2() :: cgolam_rules:rules() .


-export_type([cgolam_rules_species2/0]).


%% @private

-spec new
	(RulesModCfg :: list()) ->
		cgolam_rules_species2() .

new(RulesModCfg) ->
	FieldMod = case lists:keysearch(field, 1, RulesModCfg) of
		{value, {field, M}} -> M;
		{value, {field, M, _C}} -> M
		end,
	ColMatchBitmask = case lists:keysearch(colmatch_bitmask, 1, RulesModCfg) of
		{value, {colmatch_bitmask, BM}} -> BM;
		false -> 16#80
		end,
	#cgolam_rules_species2{
		field_mod   = FieldMod,
		colmatch_bm = ColMatchBitmask
	}
.


%% @private

-spec calc
	(Rules :: cgolam_rules_species2(), Field :: cgolam_field:field(), X :: integer(), Y :: integer()) ->
		CellState :: term() .

calc(#cgolam_rules_species2{
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
			case cgol_modified_rule(length(SurroundingRGBs), 5) of
				overcrowded ->
					{ContendingRGBsAcc, MidCellRGBContending};
				_NotOvercrowded ->
					case cgol_modified_rule(length(SurroundingSameRGBs), 3) of
						goldilocks ->
							{[{RGB, SurroundingSameRGBs} | ContendingRGBsAcc], MidCellRGBContending};
						overcrowded ->
							{ContendingRGBsAcc, MidCellRGBContending};
						lonely ->
							{ContendingRGBsAcc, MidCellRGBContending};
						tolerable when MidRGB == false ->
							{ContendingRGBsAcc, MidCellRGBContending};
						tolerable ->
							case tolerated_same(MidRGB, RGB, ColMatchBitmask) of
								false -> {ContendingRGBsAcc, MidCellRGBContending};
								_Alive -> {[{RGB, SurroundingSameRGBs} | ContendingRGBsAcc], true}
								end
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

cgol_modified_rule(SurroundingCells, OvercrowdLimit) when SurroundingCells > OvercrowdLimit -> overcrowded;
cgol_modified_rule(SurroundingCells, _OvercrowdLimit) when SurroundingCells < 2 -> lonely;
cgol_modified_rule(3, _OvercrowdLimit) -> goldilocks;
cgol_modified_rule(_Tolerable, _OvercrowdLimit) -> tolerable.


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
	(Rules :: cgolam_rules_species2(), Field0 :: cgolam_field:field(), Type :: atom(), InitCfg :: list()) ->
		Field1 :: cgolam_field:field() .

init(#cgolam_rules_species2{field_mod=FieldMod}, Field0, default, InitCfg) ->
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

init(#cgolam_rules_species2{field_mod=FieldMod}, Field0, term, InitCfg) ->
	{value, {set, InitTerm}} = lists:keysearch(set, 1, InitCfg),
	lists:foldl(
		fun ({{X, Y}, Col = {col, _RGB}}, Field0Acc) ->
			FieldMod:set(Field0Acc, X, Y, Col)
			end,
		Field0,
		InitTerm
	)
.
