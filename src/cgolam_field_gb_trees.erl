%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc GB tree based field implementation module for 'cgolam' app.


-module(cgolam_field_gb_trees).


-behaviour(cgolam_field).


-export([new/1, width/1, height/1, get/3, set/4, all/1]).


-record(cgolam_field_gb_trees, {
	width  :: integer(),
	height :: integer(),
	tree   :: gb_trees:tree()
	}).


-type cgolam_field_gb_trees() :: cgolam_field:field() .


-export_type([cgolam_field_gb_trees/0]).


%% @private

-spec new
	(FieldModCfg :: list()) ->
		cgolam_field_gb_trees() .

new(FieldModCfg) ->
	{value, {width, Width}} = lists:keysearch(width, 1, FieldModCfg),
	{value, {height, Height}} = lists:keysearch(height, 1, FieldModCfg),
	Tree = lists:foldl(
		fun (Coords, TreeAcc) ->
			gb_trees:insert(Coords, false, TreeAcc)
			end,
		gb_trees:empty(),
		[{X, Y} || X <- lists:seq(0, Width-1), Y <- lists:seq(0, Height-1)]
		),
	#cgolam_field_gb_trees{
		width  = Width,
		height = Height,
		tree   = Tree
		}
.


%% @private

-spec width
	(Field :: cgolam_field_gb_trees()) ->
		Width :: integer() .

width(#cgolam_field_gb_trees{width=Width}) ->
	Width
.


%% @private

-spec height
	(Field :: cgolam_field_gb_trees()) ->
		Height :: integer() .

height(#cgolam_field_gb_trees{height=Height}) ->
	Height
.


%% @private

-spec get
	(Field :: cgolam_field_gb_trees(), X :: integer(), Y :: integer()) ->
		CellState :: term() . 

get(State = #cgolam_field_gb_trees{width=Width}, X, Y) when X < 0 ->
	get(State, X+Width, Y)
;

get(State = #cgolam_field_gb_trees{width=Width}, X, Y) when X >= Width ->
	get(State, X-Width, Y)
;

get(State = #cgolam_field_gb_trees{height=Height}, X, Y) when Y < 0 ->
	get(State, X, Y+Height)
;

get(State = #cgolam_field_gb_trees{height=Height}, X, Y) when Y >= Height ->
	get(State, X, Y-Height)
;

get(#cgolam_field_gb_trees{tree=Tree}, X, Y) ->
	{value, CellState} = gb_trees:lookup({X, Y}, Tree),
	CellState
.


%% @private

-spec set
	(Field0 :: cgolam_field_gb_trees(), X :: integer(), Y :: integer(), CellState :: term()) ->
		Field1 :: cgolam_field_gb_trees() .

set(State = #cgolam_field_gb_trees{width=Width}, X, Y, NewCellState) when X < 0 ->
	set(State, X+Width, Y, NewCellState)
;

set(State = #cgolam_field_gb_trees{width=Width}, X, Y, NewCellState) when X >= Width ->
	set(State, X-Width, Y, NewCellState)
;

set(State = #cgolam_field_gb_trees{height=Height}, X, Y, NewCellState) when Y < 0 ->
	set(State, X, Y+Height, NewCellState)
;

set(State = #cgolam_field_gb_trees{height=Height}, X, Y, NewCellState) when Y >= Height ->
	set(State, X, Y-Height, NewCellState)
;

set(State = #cgolam_field_gb_trees{tree=Tree}, X, Y, NewCellState) ->
	State#cgolam_field_gb_trees{
			tree = gb_trees:enter({X, Y}, NewCellState, Tree)
		}
.


%% @private

-spec all
	(Field :: cgolam_field_gb_trees()) ->
		[CellState :: term()] . 

all(#cgolam_field_gb_trees{tree=Tree}) ->
	gb_trees:to_list(Tree)
.

