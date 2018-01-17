%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc Dummy display implementation module for 'cgolam' app.


-module(cgolam_display_dummy).


-behaviour(cgolam_display).
-behaviour(gen_server).


-export([new/1, stop/1, update/2, sync/1]).


-type cgolam_display_dummy() :: cgolam_display:display() .


-export_type([cgolam_display_dummy/0]).


%% @doc Create a new display.

-spec new
	(DisplayModCfg :: list()) ->
		cgolam_display_dummy() .

new(DisplayModCfg) ->
	undefined
.


%% @doc Update the display.

-spec update
	(Display0 :: cgolam_display_dummy(), FieldUpdate :: cgolam_display:updates()) ->
		Display1 :: term() .

update(undefined, _FieldUpdate) ->
	undefined
.


%% @doc Sync (update display).

-spec sync
	(Display0 :: cgolam_display_dummy()) ->
		Display1 :: cgolam_display_dummy() .

sync(undefined) ->
	undefined
.


%% @doc Stop / close the display.

-spec stop
	(Display :: cgolam_display_dummy()) ->
		ok .

stop(undefined) ->
	ok
.
