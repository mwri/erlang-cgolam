%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc 'Species3' rules implementation module for 'cgolam' app.
%%
%% This is based on 'Species1', following the failure of 'Species2'.
%% Here the strategy is simply to make colours/species more compatible
%% by changing the colour matching algorithm.
%%
%% Multiple colour match algorithms are implemented, and may be
%% changed by way of the colmatch_algorithm config, which may be set
%% to one of:
%%
%% <pre>
%%     common_duo     - the default, will match colours based on
%%                      there being only two common colour components
%%                      instead of three, though three is matched
%%                      first if possible.
%%
%%     common_highbit - two colours are considered the same if
%%                      any of the top four MSBs are common
%%                      for R, G and B colour components.
%% </pre>
%%
%% This does at last yield a result that is interesting, due to the
%% inter species interaction being sufficiently favourable whilst
%% still fundamentally maintaining the integrity of Conway's Game
%% of Life rules.
%%
%% A single colour will still operate exactly according to the original
%% CGoL rules, but the colours, when interacting are not purely
%% cooperative... though I'm not sure if you would call them
%% competitive as such, a bit like CGoL generally, it's a bit of
%% a curiosity and a fascinating demonstration of emergent behaviour.


-module(cgolam_rules_species3).


-behaviour(cgolam_rules).


-export([new/1, calc/4, init/4]).


-record(cgolam_rules_species3, {
	field_mod    :: module(),
	colmatch_alg :: atom()
}).


-type cgolam_rules_species3() :: cgolam_rules:rules() .


-export_type([cgolam_rules_species3/0]).


%% @private

-spec new
	(RulesModCfg :: list()) ->
		cgolam_rules_species3() .

new(RulesModCfg) ->
	FieldMod = case lists:keysearch(field, 1, RulesModCfg) of
		{value, {field, M}} -> M;
		{value, {field, M, _C}} -> M
		end,
	ColMatchAlg = case lists:keysearch(colmatch_algorithm, 1, RulesModCfg) of
		{value, {colmatch_algorithm, CMA}} -> CMA;
		false -> common_highbit
		end,
	#cgolam_rules_species3{
		field_mod    = FieldMod,
		colmatch_alg = ColMatchAlg
	}
.


%% @private

-spec calc
	(Rules :: cgolam_rules_species3(), Field :: cgolam_field:field(), X :: integer(), Y :: integer()) ->
		CellState :: term() .

calc(#cgolam_rules_species3{
		field_mod    = FieldMod,
		colmatch_alg = ColMatchAlg
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
				fun (UniqueRGB) -> tolerated_same(RGB, UniqueRGB, ColMatchAlg) /= false end,
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
					tolerated_same(SurroundingRGB, RGB, ColMatchAlg) /= false
				],
			case cgol_rule(length(SurroundingSameRGBs)) of
				true ->
					{[{RGB, SurroundingSameRGBs} | ContendingRGBsAcc], MidCellRGBContending};
				false ->
					{ContendingRGBsAcc, MidCellRGBContending};
				unchanged when MidRGB == false ->
					{ContendingRGBsAcc, MidCellRGBContending};
				unchanged ->
					case tolerated_same(MidRGB, RGB, ColMatchAlg) of
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

tolerated_same({Ra, Ga, Ba}, {Rb, Gb, Bb}, common_duo)
		when ((Ra band 16#80) == (Rb band 16#80))
			and ((Ga band 16#80) == (Gb band 16#80))
			and ((Ba band 16#80) == (Bb band 16#80)) ->
	{Ra band 16#80, Ga band 16#80, Ba band 16#80}
;

tolerated_same({Ra, Ga, Ba}, {Rb, Gb, Bb}, common_duo)
		when ((Ra band 16#80) == (Rb band 16#80))
			and ((Ga band 16#80) == (Gb band 16#80)) ->
	{Ra band 16#80, Ga band 16#80, Ba band Bb}
;

tolerated_same({Ra, Ga, Ba}, {Rb, Gb, Bb}, common_duo)
		when ((Ra band 16#80) == (Rb band 16#80))
			and ((Ba band 16#80) == (Bb band 16#80)) ->
	{Ra band 16#80, Ga band Gb, Ba band 16#80}
;

tolerated_same({Ra, Ga, Ba}, {Rb, Gb, Bb}, common_duo)
		when ((Ga band 16#80) == (Gb band 16#80))
			and ((Ba band 16#80) == (Bb band 16#80)) ->
	{Ra band Rb, Ga band 16#80, Ba band 16#80}
;

tolerated_same(_A, _B, common_duo) ->
	false
;


tolerated_same(A, B, intolerant_duo) when (A == B) ->
	A
;

tolerated_same({Ra, Ga, Ba}, {Rb, Gb, Bb}, intolerant_duo) when (Ra == Rb) and (Ga == Gb) ->
	{Ra band 16#80, Ga band 16#80, Ba band Bb}
;

tolerated_same({Ra, Ga, Ba}, {Rb, Gb, Bb}, intolerant_duo) when (Ra == Rb) and (Ba == Bb) ->
	{Ra band 16#80, Ga band Gb, Ba band 16#80}
;

tolerated_same({Ra, Ga, Ba}, {Rb, Gb, Bb}, intolerant_duo) when (Ga == Gb) and (Ba == Bb) ->
	{Ra band Rb, Ga band 16#80, Ba band 16#80}
;

tolerated_same(_A, _B, intolerant_duo) ->
	false
;


tolerated_same({Ra, Ga, Ba}, {Rb, Gb, Bb}, common_highbit)
		when ((Ra band 16#80) == (Rb band 16#80))
			and ((Ga band 16#80) == (Gb band 16#80))
			and ((Ba band 16#80) == (Bb band 16#80)) ->
	{Ra band 16#80, Ga band 16#80, Ba band 16#80}
;

tolerated_same({Ra, Ga, Ba}, {Rb, Gb, Bb}, common_highbit)
		when ((Ra band 16#40) == (Rb band 16#40))
			and ((Ga band 16#40) == (Gb band 16#40))
			and ((Ba band 16#40) == (Bb band 16#40)) ->
	{Ra band 16#40, Ga band 16#40, Ba band 16#40}
;

tolerated_same({Ra, Ga, Ba}, {Rb, Gb, Bb}, common_highbit)
		when ((Ra band 16#20) == (Rb band 16#20))
			and ((Ga band 16#20) == (Gb band 16#20))
			and ((Ba band 16#20) == (Bb band 16#20)) ->
	{Ra band 16#20, Ga band 16#20, Ba band 16#20}
;

tolerated_same({Ra, Ga, Ba}, {Rb, Gb, Bb}, common_highbit)
		when ((Ra band 16#10) == (Rb band 16#10))
			and ((Ga band 16#10) == (Gb band 16#10))
			and ((Ba band 16#10) == (Bb band 16#10)) ->
	{Ra band 16#10, Ga band 16#10, Ba band 16#10}
;

tolerated_same(_A, _B, common_highbit) ->
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
	(Rules :: cgolam_rules_species3(), Field0 :: cgolam_field:field(), Type :: atom(), InitCfg :: list()) ->
		Field1 :: cgolam_field:field() .

init(#cgolam_rules_species3{field_mod=FieldMod}, Field0, default, InitCfg) ->
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

init(#cgolam_rules_species3{field_mod=FieldMod}, Field0, term, InitCfg) ->
	{value, {set, InitTerm}} = lists:keysearch(set, 1, InitCfg),
	lists:foldl(
		fun ({{X, Y}, Col = {col, _RGB}}, Field0Acc) ->
			FieldMod:set(Field0Acc, X, Y, Col)
			end,
		Field0,
		InitTerm
	)
.
