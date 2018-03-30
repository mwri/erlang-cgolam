%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%%
%% @doc Game supervisor module for 'cgolam' app.


-module(cgolam_game_sup).


-behaviour(supervisor).


-export([start_link/0]).

-export([init/1]).

-export([start_game/1, stop_game/1, list_games/0]).


%% @private

-spec start_link
	() ->
		{ok, Pid :: pid()} .

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [])
.


%% @private

init([]) ->
	RestartSpec = {simple_one_for_one, 3, 60},
	ChildrenSpec = [
			{cgolam_game, {cgolam_game, start_link, []},
				temporary, 2000, worker,
					[cgolam_game]
				}
		],
	{ok, {RestartSpec, ChildrenSpec}}
.


%% @private

-spec start_game
	(GameCfg :: list()) ->
		{ok, Pid :: pid()} .

start_game(GameCfg) ->
	supervisor:start_child(?MODULE, [GameCfg])
.


%% @private

-spec stop_game
	(Pid :: pid()) ->
		ok .

stop_game(Pid) ->
	supervisor:terminate_child(?MODULE, Pid)
.


%% @private

-spec list_games
	() ->
		[Pid :: pid()] .

list_games() ->
	[Pid || {_, Pid, _, [cgolam_game]} <- supervisor:which_children(?MODULE)]
.
