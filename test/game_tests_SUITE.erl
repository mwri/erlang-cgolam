-module(game_tests_SUITE).


-compile(export_all).


-include_lib("common_test/include/ct.hrl").


-define(cgolam_games_env, [[
	{title, "Coloured Conway's Game of Life"},
	{interval, 1000},
	{rules, cgolam_rules_coloured, [{ignored_unknown, naff}]},
	{field, cgolam_field_ets, [{not_found, 404}]},
	{width, 100}, {height, 100},
	{display, cgolam_display_dummy, [{sqsize, 5}, {garbage, bar}]},
	{init, cgolam_rules_coloured, default, [{
		{cluster_size, 200},
		{cluster_density, 150},
		{clusters, 3},
		{rubbish, foo}
	}]},
	{nonsense, nonsense}
]]).


all() -> [
	test_voluntary_stop,
	test_crash_on_unexpected_info,
	test_crash_on_unexpected_cast,
	test_crash_on_unexpected_call
].


suite() ->
	[{timetrap, {seconds,30}}, {logdir, "logs"}]
.


init_per_suite(Config) ->
	Config
.

end_per_suite(_Config) ->
	ok
.


init_per_group(start_app, Config) ->
	Config
;
init_per_group(_Group, Config) ->
	{ok, _AppsStarted} = application:ensure_all_started(cgolam),
	ok = application:stop(cgolam),
	Config
.


end_per_group(_Test, _Config) ->
	ok
.


init_per_testcase(_Test, Config) ->
	application:load(cgolam),
	ok = application:set_env(cgolam, games, ?cgolam_games_env),
	{ok, _AppsStarted} = application:ensure_all_started(cgolam),
	Config
.

end_per_testcase(_Test, _Config) ->
	ok = application:stop(cgolam)
.


test_voluntary_stop(_Config) ->
	{ok, NewGamePid} = cgolam:start([{width, 5}, {height, 5}, {display, cgolam_display_dummy}]),
	% check it was started
	[Pid1, Pid2] = cgolam:list(),
	true = is_pid(Pid1),
	true = is_pid(Pid2),
	true = (NewGamePid == Pid1) or (NewGamePid == Pid2),
	% stop both
	cgolam_game:stop(Pid1),
	cgolam_game:stop(Pid2),
	% check they stopped
	timer:sleep(1000),
	[] = cgolam:list()
.


test_crash_on_unexpected_info(_Config) ->
	[Pid] = cgolam:list(),
	Pid ! load_of_rubbish,
	% check it is dead
	timer:sleep(1000),
	[] = cgolam:list()
.


test_crash_on_unexpected_cast(_Config) ->
	[Pid] = cgolam:list(),
	gen_server:cast(Pid, load_of_rubbish),
	% check it is dead
	timer:sleep(1000),
	[] = cgolam:list()
.


test_crash_on_unexpected_call(_Config) ->
	[Pid] = cgolam:list(),
	{error, {unexpected_call, load_of_rubbish}} = gen_server:call(Pid, load_of_rubbish),
	% check it is dead
	timer:sleep(1000),
	[] = cgolam:list()
.
