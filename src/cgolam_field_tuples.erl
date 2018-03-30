%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc Tuple field implementation module for 'cgolam' app.


-module(cgolam_field_tuples).


-behaviour(cgolam_field).


-export([new/1, width/1, height/1, get/3, set/4, all/1]).


-record(cgolam_field_tuples, {
	width  :: integer(),
	height :: integer(),
	data   :: tuple()
	}).


-type cgolam_field_tuples() :: cgolam_field:field() .


-export_type([cgolam_field_tuples/0]).


%% @private

-spec new
	(FieldModCfg :: list()) ->
		cgolam_field_tuples() .

new(FieldModCfg) ->
	{value, {width, Width}} = lists:keysearch(width, 1, FieldModCfg),
	{value, {height, Height}} = lists:keysearch(height, 1, FieldModCfg),
	Data = erlang:list_to_tuple(
		[erlang:list_to_tuple(
				[false || _X <- lists:seq(0, Width-1)]
			) || _Y <- lists:seq(0, Height-1)]
		),
	#cgolam_field_tuples{
		width  = Width,
		height = Height,
		data   = Data
		}
.


%% @private

-spec width
	(Field :: cgolam_field_tuples()) ->
		Width :: integer() .

width(#cgolam_field_tuples{width=Width}) ->
	Width
.


%% @private

-spec height
	(Field :: cgolam_field_tuples()) ->
		Height :: integer() .

height(#cgolam_field_tuples{height=Height}) ->
	Height
.


%% @private

-spec get
	(Field :: cgolam_field_tuples(), X :: integer(), Y :: integer()) ->
		CellState :: term() . 

get(State = #cgolam_field_tuples{width=Width}, X, Y) when X < 0 ->
	get(State, X+Width, Y)
;

get(State = #cgolam_field_tuples{width=Width}, X, Y) when X >= Width ->
	get(State, X-Width, Y)
;

get(State = #cgolam_field_tuples{height=Height}, X, Y) when Y < 0 ->
	get(State, X, Y+Height)
;

get(State = #cgolam_field_tuples{height=Height}, X, Y) when Y >= Height ->
	get(State, X, Y-Height)
;

get(#cgolam_field_tuples{data=Data}, X, Y) ->
	erlang:element(X + 1, erlang:element(Y + 1, Data))
.


%% @private

-spec set
	(Field0 :: cgolam_field_tuples(), X :: integer(), Y :: integer(), CellState :: term()) ->
		Field1 :: cgolam_field_tuples() .

set(State = #cgolam_field_tuples{width=Width}, X, Y, NewCellState) when X < 0 ->
	set(State, X+Width, Y, NewCellState)
;

set(State = #cgolam_field_tuples{width=Width}, X, Y, NewCellState) when X >= Width ->
	set(State, X-Width, Y, NewCellState)
;

set(State = #cgolam_field_tuples{height=Height}, X, Y, NewCellState) when Y < 0 ->
	set(State, X, Y+Height, NewCellState)
;

set(State = #cgolam_field_tuples{height=Height}, X, Y, NewCellState) when Y >= Height ->
	set(State, X, Y-Height, NewCellState)
;

set(State = #cgolam_field_tuples{data=Data}, X, Y, NewCellState) ->
	State#cgolam_field_tuples{
			data = erlang:setelement(
					Y + 1,
					Data,
					erlang:setelement(
						X + 1,
						erlang:element(Y + 1, Data),
						NewCellState
					)
				)
		}
.

%% @private

-spec all
	(Field :: cgolam_field_tuples()) ->
		[CellState :: term()] . 

all(#cgolam_field_tuples{
		width  = Width,
		height = Height,
		data   = Data
		}) ->
	[{{Xp1-1, Yp1-1}, erlang:element(Xp1, erlang:element(Yp1, Data))}
		|| Yp1 <- lists:seq(1, Height), Xp1 <- lists:seq(1, Width)]
.
