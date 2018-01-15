%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc ETS based field implementation module for 'cgolam' app.


-module(cgolam_field_ets).


-behaviour(cgolam_field).


-export([new/1, width/1, height/1, get/3, set/4, all/1]).


-record(cgolam_field_ets, {
	width  :: integer(),
	height :: integer(),
	table  :: ets:tab()
	}).


-type cgolam_field_ets() :: cgolam_field:field() .


-export_type([cgolam_field_ets/0]).


%% @doc Create a new field.

-spec new
	(FieldModCfg :: list()) ->
		cgolam_field_ets() .

new(FieldModCfg) ->
	{value, {width, Width}} = lists:keysearch(width, 1, FieldModCfg),
	{value, {height, Height}} = lists:keysearch(height, 1, FieldModCfg),
	Table = ets:new(field_data, [set]),
	ets:insert(Table, [{{X, Y}, false} || X <- lists:seq(0, Width-1), Y <- lists:seq(0, Height-1)]),
	#cgolam_field_ets{
		width  = Width,
		height = Height,
		table  = Table
		}
.


%% @doc Return the field's width.

-spec width
	(Field :: cgolam_field_ets()) ->
		Width :: integer() .

width(#cgolam_field_ets{width=Width}) ->
	Width
.


%% @doc Return the field's height.

-spec height
	(Field :: cgolam_field_ets()) ->
		Height :: integer() .

height(#cgolam_field_ets{height=Height}) ->
	Height
.


%% @doc Get a field cell state.

-spec get
	(Field :: cgolam_field_ets(), X :: integer(), Y :: integer()) ->
		CellState :: term() . 


get(State = #cgolam_field_ets{width=Width}, X, Y) when X < 0 ->
	get(State, X+Width, Y)
;

get(State = #cgolam_field_ets{width=Width}, X, Y) when X >= Width ->
	get(State, X-Width, Y)
;

get(State = #cgolam_field_ets{height=Height}, X, Y) when Y < 0 ->
	get(State, X, Y+Height)
;

get(State = #cgolam_field_ets{height=Height}, X, Y) when Y >= Height ->
	get(State, X, Y-Height)
;

get(#cgolam_field_ets{table=Table}, X, Y) ->
	[{{X, Y}, CellState}] = ets:lookup(Table, {X, Y}),
	CellState
.


%% @doc Set a field cell state.

-spec set
	(Field0 :: cgolam_field_ets(), X :: integer(), Y :: integer(), CellState :: term()) ->
		Field1 :: cgolam_field_ets() .

set(State = #cgolam_field_ets{width=Width}, X, Y, NewCellState) when X < 0 ->
	set(State, X+Width, Y, NewCellState)
;

set(State = #cgolam_field_ets{width=Width}, X, Y, NewCellState) when X >= Width ->
	set(State, X-Width, Y, NewCellState)
;

set(State = #cgolam_field_ets{height=Height}, X, Y, NewCellState) when Y < 0 ->
	set(State, X, Y+Height, NewCellState)
;

set(State = #cgolam_field_ets{height=Height}, X, Y, NewCellState) when Y >= Height ->
	set(State, X, Y-Height, NewCellState)
;

set(State = #cgolam_field_ets{table=Table}, X, Y, NewCellState) ->
	true = ets:insert(Table, {{X, Y}, NewCellState}),
	State
.


%% @doc Return ALL the cells (coordinates and states together).

-spec all
	(Field :: cgolam_field_ets()) ->
		[CellState :: term()] . 

all(#cgolam_field_ets{table=Table}) ->
	ets:tab2list(Table)
.

