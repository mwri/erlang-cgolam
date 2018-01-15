%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc Display behaviour specification module for 'cgolam' app.


-module(cgolam_display).


-type display() :: term() .
-type update() :: {{X :: integer(), Y :: integer()}, CellState :: term()} .
-type updates() :: update() | [update()] .


-export_type([display/0, updates/0, update/0]).


-callback new
	(DisplayModCfg :: list()) ->
		Display :: display() .

-callback stop
	(Display :: display()) ->
		ok .

-callback update
	(Display0 :: display(), FieldUpdate :: updates()) ->
		Display1 :: term() .

-callback sync
	(Display0 :: display()) ->
		Display1 :: display() .
