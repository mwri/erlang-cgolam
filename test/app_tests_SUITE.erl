-module(app_tests_SUITE).


-compile(export_all).


-include_lib("common_test/include/ct.hrl").


-define(cgolam_games_env, [[
	{field, cgolam_field_ets, []},
	{width, 40}, {height, 40},
	{rules, cgolam_rules_normal, []},
	{display, cgolam_display_wx, [
		{sqsize, 4},
		{title, "Good old CGoL"}
	]}
]]).


all() -> [
	{group, start_app},
	{group, start_game}
].


groups() -> [
		{start_app, [], [
				test_app_start_stop,
				test_default_game_start
			]},
		{start_game, [{start_apps_per_test, [cgolam]}], [
				test_game_start,
				test_game_start_stop
			]}
	]
.


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
	application:load(cgolam),
	ok = application:set_env(cgolam, games, ?cgolam_games_env),
	{ok, _AppsStarted} = application:ensure_all_started(cgolam),
	ok = application:stop(cgolam),
	Config
.


end_per_group(_Test, _Config) ->
	ok
.


init_per_testcase(_Test, Config) ->
	{tc_group_properties, GrpProps} = lists:keyfind(tc_group_properties, 1, Config),
	case lists:keyfind(start_apps_per_test, 1, GrpProps) of
		{start_apps_per_test, Apps} ->
			lists:foreach(
				fun (App) ->
					application:load(App),
					ok = application:set_env(cgolam, games, ?cgolam_games_env),
					ok = application:start(App)
					end,
				Apps
			);
		false ->
			ok
		end,
	Config
.

end_per_testcase(_Test, Config) ->
	{tc_group_properties, GrpProps} = lists:keyfind(tc_group_properties, 1, Config),
	case lists:keyfind(start_apps_per_test, 1, GrpProps) of
		{start_apps_per_test, Apps} ->
			lists:foreach(
				fun (App) ->
					ok = application:stop(App)
					end,
				Apps
			);
		false ->
			ok
		end,
	ok
.


test_app_start_stop(_Config) ->
	application:load(cgolam),
	ok = application:set_env(cgolam, games, ?cgolam_games_env),
	{ok, AppsStarted} = application:ensure_all_started(cgolam),
	[cgolam] = [cgolam || cgolam <- AppsStarted],
	[cgolam] = [cgolam || {cgolam, _, _} <- application:which_applications()],
	ok = application:stop(cgolam)
.


test_default_game_start(_Config) ->
	application:load(cgolam),
	ok = application:set_env(cgolam, games, ?cgolam_games_env),
	{ok, _AppsStarted} = application:ensure_all_started(cgolam),
	[OneRunner] = cgolam:list(),
	true = is_pid(OneRunner),
	ok = application:stop(cgolam)
.


test_game_start(_Config) ->
	{ok, NewGamePid} = cgolam:start([{width, 5}, {height, 5}]),
	% check it was started
	[Pid1, Pid2] = cgolam:list(),
	true = is_pid(Pid1),
	true = is_pid(Pid2),
	true = (NewGamePid == Pid1) or (NewGamePid == Pid2)
.


test_game_start_stop(_Config) ->
	{ok, NewGamePid} = cgolam:start([{width, 5}, {height, 5}]),
	% check it was started
	[Pid1, Pid2] = cgolam:list(),
	true = is_pid(Pid1),
	true = is_pid(Pid2),
	true = (NewGamePid == Pid1) or (NewGamePid == Pid2),
	% stop both
	cgolam:stop(Pid1),
	cgolam:stop(Pid2),
	% check they stopped
	[] = cgolam:list()
.
