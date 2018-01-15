%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc Field behaviour specification module for 'cgolam' app.


-module(cgolam_field).


-type field() :: term() .


-export_type([field/0]).


-callback new
	(FieldModCfg :: list()) ->
		Field :: field() .

-callback width
	(Field :: field()) ->
		Width :: integer() .

-callback height
	(Field :: field()) ->
		Height :: integer() .

-callback get
	(Field :: field(), X :: integer(), Y :: integer()) ->
		CellState :: term() .

-callback set
	(Field0 :: field(), X :: integer(), Y :: integer(), CellState :: term()) ->
		Field1 :: field() .

-callback all
	(Field :: field()) ->
		[CellState :: term()] .
