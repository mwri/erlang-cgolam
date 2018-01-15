%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc Top supervisor module for 'cgolam' app.


-module(cgolam_sup).


-behaviour(supervisor).


-export([start_link/0]).

-export([init/1]).


%% @private

-spec start_link
	() ->
		{ok, Pid :: pid()} .

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [])
.


%% @private

init([]) ->
	RestartSpec = {one_for_all, 3, 300},
	ChildrenSpec = [
		{cgolam_game_sup,
			{cgolam_game_sup, start_link, []},
				permanent, 30, worker, [cgolam_game_sup]}
		],
	{ok, {RestartSpec, ChildrenSpec}}
.
