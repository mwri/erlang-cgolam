%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc Main API module for 'cgolam' app.


-module(cgolam).


-export([start/1, stop/1, list/0]).


%% @doc Start a game.
%%
%% Start a game. Width and height properties must be provided, like this:
%%
%% <pre>
%%     1> ok = application:start(cgolam).
%%     ok
%%     2> cgolam:start([{width, 100}, {height, 100}]).
%%     {ok,<0.83.0>}
%% </pre>
%%
%% Other properties that may be given are as follows:
%%
%% <pre>
%%     {rules, RulesModule, RulesConfig}
%%     {rules, RulesModule}
%% </pre>
%%
%% The default is {cgolam_rules_normal, []} but other modules, which implement
%% the cgolam_rules behaviour, may be used to provide a different set of rules
%% to Conway's Game of Life.
%%
%% Another rules implementation is cgolam_rules_coloured, which also implements
%% Conway's Game of Life, but uses different colours for the cells, with new
%% cells (becoming alive from dead) assuming a colour mix of the surrounding
%% cells.
%%
%% <pre>
%%     {field, FieldModule, FieldConfig}
%%     {field, FieldModule}
%% </pre>
%%
%% The 'field' module is the back end implementation containing the data representing
%% the game state.
%%
%% The default is {cgolam_rules_ets, []}, which uses ETS tables, but cgolam_rules_gb_trees
%% and cgolam_rules_tuples also exist.
%%
%% <pre>
%%     {init, InitModule, InitType, InitConfig}
%%     {init, InitModule, InitType}
%% </pre>
%%
%% The 'init' module is used to initialise the field with contents. The default is to use
%% RulesModule, which initialises the field with data that makes sense to the rules. The
%% default InitType is default.
%%
%% InitType is an atom, by default it is default but can be other values if supported
%% by the module. InitConfig is a list of properties, and the properties supported depends
%% entirely on the module, but cgolam_rules_normal and cgolam_rules_coloured both support
%% cluster_size and cluster_density, which increase or decrease the size and densities of
%% the clusters of cells (the default is 100, it is a percentage, so 50 halves the density
%% (sort of) and 200 doubles it). Also both support clusters, which determine the number
%% of clusters.
%%
%% <pre>
%%     {display, DisplayModule, DisplayConfig}
%%     {display, DisplayModule}
%% </pre>
%%
%% The 'display' module is used to display the game state. Currently this can only be
%% cgolam_display_wx, which uses WX Widgets / X Windows. A property of sqsize may be
%% given in DisplayConfig to change the size of the cells in pixels.
%%
%% For example, this, with the width and height added is the default game (note that
%% the 'init' config is redundant here):
%%
%% <pre>
%%     cgolam:start([
%%         {rules, cgolam_rules_normal, []},
%%         {field, cgolam_field_ets, []},
%%         {width, 100}, {height, 100},
%%         {display, cgolam_display_wx, [{sqsize, 1}]},
%%         {init, cgolam_rules_normal, default, []}
%%     ]).
%% </pre>
%%
%% Here's a more interesting one which shows you more features:
%%
%% <pre>
%%     cgolam:start([
%%         {title, "Coloured Conway's Game of Life"},
%%         {interval, 20},
%%         {rules, cgolam_rules_coloured, []},
%%         {field, cgolam_field_ets, []},
%%         {width, 100}, {height, 100},
%%         {display, cgolam_display_wx, [{sqsize, 5}]},
%%         {init, cgolam_rules_coloured, default, [{cluster_size, 200}]}
%%     ]).
%% </pre>

-spec start
	(GameCfg :: list()) ->
		{ok, Pid :: pid()} .

start(GameCfg) ->
	cgolam_game_sup:start_game(GameCfg)
.


%% @doc Stop a game.

-spec stop
	(Pid :: pid()) ->
		ok .

stop(Pid) ->
	cgolam_game_sup:stop_game(Pid)
.


%% @doc List running games.

-spec list
	() ->
		[Pid :: pid()] .

list() ->
	cgolam_game_sup:list_games()
.
