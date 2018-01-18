-module(display_tests_SUITE).


-compile(export_all).


-include_lib("common_test/include/ct.hrl").


-define(display_modules, [
	{cgolam_display_dummy, [{title, "CGoL"}, {width, 5}, {height, 5}]},
	{cgolam_display_wx, [{title, "CGoL"}, {width, 5}, {height, 5}]}
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
	[{Mod, [{display, Mod, ModCfg}], ?display_tests} || {Mod, ModCfg} <- ?display_modules]
.


suite() ->
	[{timetrap, {seconds,30}}, {logdir, "logs"}]
.


init_per_suite(Config) ->
	{ok, _StartedApps} = application:ensure_all_started(meck),
	Config
.


end_per_suite(_Config) ->
	ok = application:stop(meck),
	ok
.


init_per_group(cgolam_display_wx, Config) ->
	Config
;
init_per_group(_Group, Config) ->
	Config
.

end_per_group(cgolam_display_wx, _Config) ->
	ok
;
end_per_group(_Group, _Config) ->
	ok
.


init_per_testcase(_Test, Config) ->
	{tc_group_properties, GrpProps} = lists:keyfind(tc_group_properties, 1, Config),
	case lists:keyfind(display, 1, GrpProps) of
		{display, cgolam_display_wx, _ModCfg} ->
			ok = meck:new(wx, []),
			ok = meck:new(wxFrame, []),
			ok = meck:new(wxMenuBar, []),
			ok = meck:new(wxWindow, []),
			ok = meck:new(wxMenu, []),
			ok = meck:new(wxMenuItem, []),
			ok = meck:new(wxPaintDC, []),
			ok = meck:new(wxBrush, []),
			ok = meck:new(wxDC, []),
			ok = meck:expect(wx, new, fun() -> undefined end),
			ok = meck:expect(wxFrame, new, fun(undefined, _, _) -> undefined end),
			ok = meck:expect(wxFrame, createStatusBar, fun(undefined) -> undefined end),
			ok = meck:expect(wxFrame, setStatusText, fun(undefined, _) -> undefined end),
			ok = meck:expect(wxFrame, setMenuBar, fun(undefined, undefined) -> undefined end),
			ok = meck:expect(wxFrame, show, fun(undefined) -> true end),
			ok = meck:expect(wxFrame, connect, fun(undefined, _) -> undefined end),
			ok = meck:expect(wxMenuBar, new, fun() -> undefined end),
			ok = meck:expect(wxMenuBar, append, fun(undefined, undefined, _) -> undefined end),
			ok = meck:expect(wxWindow, getSize, fun(undefined) -> {50, 50} end),
			ok = meck:expect(wxWindow, setSize, fun(undefined, _) -> undefined end),
			ok = meck:expect(wxMenu, new, fun() -> undefined end),
			ok = meck:expect(wxMenu, append, fun(undefined, _) -> undefined end),
			ok = meck:expect(wxMenuItem, new, fun(_) -> undefined end),
			ok = meck:expect(wxPaintDC, new, fun(undefined) -> undefined end),
			ok = meck:expect(wxPaintDC, destroy, fun(undefined) -> undefined end),
			ok = meck:expect(wxBrush, new, fun(_) -> undefined end),
			ok = meck:expect(wxDC, setBrush, fun(undefined, undefined) -> undefined end),
			ok = meck:expect(wxDC, drawRectangle, fun(undefined, _) -> undefined end);
		_ ->
			ok
		end,
	Config
.


end_per_testcase(_Test, Config) ->
	{tc_group_properties, GrpProps} = lists:keyfind(tc_group_properties, 1, Config),
	case lists:keyfind(display, 1, GrpProps) of
		{display, cgolam_display_wx, _ModCfg} ->
			ok = meck:unload(wx),
			ok = meck:unload(wxFrame),
			ok = meck:unload(wxMenuBar),
			ok = meck:unload(wxWindow),
			ok = meck:unload(wxMenu),
			ok = meck:unload(wxMenuItem),
			ok = meck:unload(wxPaintDC),
			ok = meck:unload(wxBrush),
			ok = meck:unload(wxDC);
		_ ->
			ok
		end,
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
