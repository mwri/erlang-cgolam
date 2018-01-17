-module(display_tests_SUITE).


-compile(export_all).


-include_lib("common_test/include/ct.hrl").


-define(display_modules, [
	{cgolam_display_dummy, [{title, "CGoL"}, {width, 5}, {height, 5}]}
]).

-define(display_tests, [
	test_new_stop,
	test_update,
	test_update_sync_update
]).


all() ->
	[{group, Mod} || {Mod, _ModCfg} <- ?display_modules]
.


groups() ->
	[{Mod, [parallel, {display, Mod, ModCfg}], ?display_tests} || {Mod, ModCfg} <- ?display_modules]
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


init_per_group(_Group, Config) ->
	Config
.

end_per_group(_Group, _Config) ->
	ok
.


test_new_stop(Config) ->
	{tc_group_properties, GrpProps} = lists:keyfind(tc_group_properties, 1, Config),
	{display, DisplayMod, DisplayModCfg} = lists:keyfind(display, 1, GrpProps),
	Display = DisplayMod:new(DisplayModCfg),
	ok = DisplayMod:stop(Display)
.


test_update(Config) ->
	{tc_group_properties, GrpProps} = lists:keyfind(tc_group_properties, 1, Config),
	{display, DisplayMod, DisplayModCfg} = lists:keyfind(display, 1, GrpProps),
	Display1 = DisplayMod:new(DisplayModCfg),
	Display2 = DisplayMod:update(Display1, {{1, 1}, true}),
	Display3 = DisplayMod:update(Display2, {{0, 0}, true}),
	Display4 = DisplayMod:update(Display3, [{{2, 2}, true}, {{3, 2}, true}, {{4, 2}, true}]),
	Display5 = DisplayMod:update(Display4, [{{1, 1}, false}]),
	ok = DisplayMod:stop(Display5)
.


test_update_sync_update(Config) ->
	{tc_group_properties, GrpProps} = lists:keyfind(tc_group_properties, 1, Config),
	{display, DisplayMod, DisplayModCfg} = lists:keyfind(display, 1, GrpProps),
	Display1 = DisplayMod:new(DisplayModCfg),
	Display2 = DisplayMod:update(Display1, {{1, 1}, true}),
	Display3 = DisplayMod:update(Display2, {{0, 0}, true}),
	Display4 = DisplayMod:update(Display3, [{{2, 2}, true}, {{3, 2}, true}, {{4, 2}, true}]),
	Display5 = DisplayMod:sync(Display4),
	Display6 = DisplayMod:update(Display5, [{{1, 1}, false}]),
	Display6 = DisplayMod:update(Display5, {{4, 4}, true}),
	ok = DisplayMod:stop(Display6)
.
