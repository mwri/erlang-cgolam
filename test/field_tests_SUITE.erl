-module(field_tests_SUITE).


-compile(export_all).


-include_lib("common_test/include/ct.hrl").


-define(field_modules, [
	cgolam_field_ets,
	cgolam_field_gb_trees,
	cgolam_field_tuples
]).

-define(field_tests, [
	test_new,
	test_width,
	test_height,
	test_get,
	test_set_get,
	test_set_get_set_overlap,
	test_all,
	test_all_set_get_all
]).


all() ->
	[{group, M} || M <- ?field_modules]
.


groups() ->
	[{M, [parallel, {field_mod, M}], ?field_tests} || M <- ?field_modules]
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


test_new(Config) ->
	{tc_group_properties, GrpProps} = lists:keyfind(tc_group_properties, 1, Config),
	{field_mod, FieldMod} = lists:keyfind(field_mod, 1, GrpProps),
	FieldCfgList = [
		[{width, 10}, {height, 10}],
		[{width, 100}, {height, 100}],
		[{width, 1000}, {height, 1000}],
		[{width, 10}, {height, 200}],
		[{width, 200}, {height, 20}]
	],
	lists:foreach(
		fun (FieldCfg) ->
			FieldMod:new(FieldCfg)
			end,
		FieldCfgList
	)
.


test_width(Config) ->
	{tc_group_properties, GrpProps} = lists:keyfind(tc_group_properties, 1, Config),
	{field_mod, FieldMod} = lists:keyfind(field_mod, 1, GrpProps),
	FieldCfgList = [
		[{width, 10}, {height, 10}],
		[{width, 100}, {height, 100}],
		[{width, 1000}, {height, 1000}],
		[{width, 10}, {height, 200}],
		[{width, 200}, {height, 20}]
	],
	lists:foreach(
		fun (FieldCfg) ->
			Field = FieldMod:new(FieldCfg),
			{width, Width} = lists:keyfind(width, 1, FieldCfg),
			Width = FieldMod:width(Field)
			end,
		FieldCfgList
	)
.


test_height(Config) ->
	{tc_group_properties, GrpProps} = lists:keyfind(tc_group_properties, 1, Config),
	{field_mod, FieldMod} = lists:keyfind(field_mod, 1, GrpProps),
	FieldCfgList = [
		[{width, 10}, {height, 10}],
		[{width, 100}, {height, 100}],
		[{width, 1000}, {height, 1000}],
		[{width, 10}, {height, 200}],
		[{width, 200}, {height, 20}]
	],
	lists:foreach(
		fun (FieldCfg) ->
			Field = FieldMod:new(FieldCfg),
			{height, Height} = lists:keyfind(height, 1, FieldCfg),
			Height = FieldMod:height(Field)
			end,
		FieldCfgList
	)
.


test_get(Config) ->
	{tc_group_properties, GrpProps} = lists:keyfind(tc_group_properties, 1, Config),
	{field_mod, FieldMod} = lists:keyfind(field_mod, 1, GrpProps),
	Field = FieldMod:new([{width, 100}, {height, 100}]),
	FieldCoords = [
		{10, 10}, {20, 20}, {30, 30}, {0, 99}, {99, 0}, {0, 0}, {99, 99},
		{-1, 10}, {-10, 10}, {-1, -10}, {1, -10}, {-1, -1000}
	],
	lists:foreach(
		fun ({X, Y}) ->
			% check initially unset
			false = FieldMod:get(Field, X, Y)
			end,
		FieldCoords
		),
	lists:foreach(
		fun ({X, Y}) ->
			% check still unset
			false = FieldMod:get(Field, X, Y)
			end,
		FieldCoords
		),
	ok
.


test_set_get(Config) ->
	{tc_group_properties, GrpProps} = lists:keyfind(tc_group_properties, 1, Config),
	{field_mod, FieldMod} = lists:keyfind(field_mod, 1, GrpProps),
	Field1 = FieldMod:new([{width, 100}, {height, 100}]),
	FieldCoords = [
		{10, 10}, {30, 30}, {0, 99}, {99, 0}, {0, 0}, {99, 99},
		{-2, 10}, {-10, 10}, {-2, -10}, {2, -10}, {-2, -1010}
	],
	lists:foreach(
		fun ({X, Y}) ->
			% check initially unset (verify start conditions)
			false = FieldMod:get(Field1, X, Y)
			end,
		FieldCoords
		),
	Field2 = lists:foldl(
		fun ({X, Y}, FieldAcc) ->
			% set cell status for field coords
			FieldMod:set(FieldAcc, X, Y, true)
			end,
		Field1,
		FieldCoords
		),
	lists:foreach(
		fun ({X, Y}) ->
			% check get yields true for coords, including duplicate coords
			true = FieldMod:get(Field2, X, Y)
			end,
		FieldCoords
		),
	ok
.


test_set_get_set_overlap(Config) ->
	{tc_group_properties, GrpProps} = lists:keyfind(tc_group_properties, 1, Config),
	{field_mod, FieldMod} = lists:keyfind(field_mod, 1, GrpProps),
	Field1 = FieldMod:new([{width, 100}, {height, 100}]),
	FieldCoords1 = [
		{10, 10}, {30, 30}, {0, 99}, {99, 0}, {0, 0}, {99, 99}
	],
	FieldCoords2 = [
		{-2, 10}, {-10, 10}, {-2, -10}, {2, -10}, {-2, -1010}
	],
	DupFieldCoords1 = [
		{-90, -90}, {-70, -70}, {-100, -1}, {-1, 100}, {200, -200}, {-1, -1}
	],
	lists:foreach(
		fun ({X, Y}) ->
			% check initially unset (verify start conditions)
			false = FieldMod:get(Field1, X, Y)
			end,
		FieldCoords1++FieldCoords2++DupFieldCoords1
		),
	Field2 = lists:foldl(
		fun ({X, Y}, FieldAcc) ->
			% set cell status for field coords
			FieldMod:set(FieldAcc, X, Y, true)
			end,
		Field1,
		FieldCoords1++FieldCoords2
		),
	lists:foreach(
		fun ({X, Y}) ->
			% check get yields true for coords, including duplicate coords
			true = FieldMod:get(Field2, X, Y)
			end,
		FieldCoords1++FieldCoords2++DupFieldCoords1
		),
	Field3 = lists:foldl(
		fun ({X, Y}, FieldAcc) ->
			% set cell status to something particular for field coords
			FieldMod:set(FieldAcc, X, Y, {foo, bar, X, Y})
			end,
		Field2,
		FieldCoords1
		),
	lists:foreach(
		fun ({{Xa, Ya}, {Xb, Yb}}) ->
			% check get yields particular values for coords
			{foo, bar, Xa, Ya} = FieldMod:get(Field3, Xa, Ya),
			{foo, bar, Xa, Ya} = FieldMod:get(Field3, Xb, Yb)
			end,
		lists:zip(FieldCoords1, DupFieldCoords1)
		),
	Field4 = lists:foldl(
		fun ({X, Y}, FieldAcc) ->
			% set cell status to something particular for field coords
			FieldMod:set(FieldAcc, X, Y, {foo, bar, X, Y})
			end,
		Field3,
		DupFieldCoords1
		),
	lists:foreach(
		fun ({{Xa, Ya}, {Xb, Yb}}) ->
			% check get yields particular values for coords
			{foo, bar, Xb, Yb} = FieldMod:get(Field4, Xa, Ya),
			{foo, bar, Xb, Yb} = FieldMod:get(Field4, Xb, Yb)
			end,
		lists:zip(FieldCoords1, DupFieldCoords1)
		),
	ok
.


test_all(Config) ->
	{tc_group_properties, GrpProps} = lists:keyfind(tc_group_properties, 1, Config),
	{field_mod, FieldMod} = lists:keyfind(field_mod, 1, GrpProps),
	Field = FieldMod:new([{width, 100}, {height, 100}]),
	Expected = lists:sort([{{X, Y}, false} || X <- lists:seq(0,99), Y <- lists:seq(0,99)]),
	Actual = lists:sort(FieldMod:all(Field)),
	Expected = Actual,
	ok
.


test_all_set_get_all(Config) ->
	{tc_group_properties, GrpProps} = lists:keyfind(tc_group_properties, 1, Config),
	{field_mod, FieldMod} = lists:keyfind(field_mod, 1, GrpProps),
	Field1 = FieldMod:new([{width, 100}, {height, 100}]),
	Expected1 = lists:sort([{{X, Y}, false} || X <- lists:seq(0,99), Y <- lists:seq(0,99)]),
	Actual1 = lists:sort(FieldMod:all(Field1)),
	Expected1 = Actual1,
	FieldCoords = [
		{10, 10}, {30, 30}, {0, 99}, {99, 0}, {0, 0}, {99, 99},
		{-2, 10}, {-10, 10}, {-2, -10}, {2, -10}, {-2, -1010}
	],
	Field2 = lists:foldl(
		fun ({X, Y}, FieldAcc) ->
			% set cell status to something particular for field coords
			FieldMod:set(FieldAcc, X, Y, {foo, bar, X, Y})
			end,
		Field1,
		FieldCoords
		),
	Expected2 = lists:sort([{{X, Y}, FieldMod:get(Field2, X, Y)} || X <- lists:seq(0,99), Y <- lists:seq(0,99)]),
	Actual2 = lists:sort(FieldMod:all(Field2)),
	Expected2 = Actual2,
	ok
.
