%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc App callback module for 'cgolam' app.


-module(cgolam_app).


-behaviour(application).


-export([start/2, stop/1]).


%% @private

start(_StartType, _StartArgs) ->
	{ok, TopSup} = cgolam_sup:start_link(),
	lists:foreach(
		fun (GameCfg) ->
			cgolam:start(GameCfg)
			end,
		application:get_env(cgolam, games, [])
		),
	{ok, TopSup}
.


%% @private

stop(_State) ->
	ok
.


