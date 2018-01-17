-module(rules_tests_SUITE).


-compile(export_all).


-include_lib("common_test/include/ct.hrl").


-define(rules_modules, [
	{cgolam_rules_normal, [{field, cgolam_field_tuples, [{width, 20}, {height, 20}]}]},
	{cgolam_rules_coloured, [{field, cgolam_field_tuples, [{width, 20}, {height, 20}]}]},
	{cgolam_rules_species1, [{field, cgolam_field_tuples, [{width, 20}, {height, 20}]}]},
	{cgolam_rules_species2, [{field, cgolam_field_tuples, [{width, 20}, {height, 20}]}]},
	{cgolam_rules_species3, [{field, cgolam_field_tuples, [{width, 20}, {height, 20}]}]}
]).

-define(rules_tests, [
	test_new,
	test_calc,
	test_init
]).


all() ->
	[{group, Mod} || {Mod, _ModCfg} <- ?rules_modules]
.


groups() ->
	[{Mod, [parallel, {rules, Mod, ModCfg}], ?rules_tests} || {Mod, ModCfg} <- ?rules_modules]
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
	{rules, RulesMod, RulesCfg} = lists:keyfind(rules, 1, GrpProps),
	_Rules = RulesMod:new(RulesCfg),
	ok
.


test_calc(Config) ->
	{tc_group_properties, GrpProps} = lists:keyfind(tc_group_properties, 1, Config),
	{rules, RulesMod, RulesCfg} = lists:keyfind(rules, 1, GrpProps),
	{field, FieldMod, FieldCfg} = lists:keyfind(field, 1, RulesCfg),
	Rules = RulesMod:new(RulesCfg),
	Results = lists:map(
		fun (Bitmap) ->
			Field1 = FieldMod:new(FieldCfg),
			Field2 = write_bitmap_to_field(RulesMod, cgolam_field_tuples, Field1, Bitmap, {1, 1}),
			RulesMod:calc(Rules, Field2, 1, 1)
			end,
		lists:seq(0, 511)
		),
	FalseCount = length(lists:filter(fun(R) -> R == false end, Results)),
	AliveCount = length(lists:filter(fun(R) -> R /= false end, Results)),
	if (FalseCount < 56) or (FalseCount > 456) ->
			throw({false_count_unfeasible, FalseCount});
		true ->
			ok
		end,
	if (AliveCount < 56) or (AliveCount > 456) ->
			throw({alive_count_unfeasible, AliveCount});
		true ->
			ok
		end,
	ok
.


write_bitmap_to_field(RulesMod, FieldMod, Field, Bitmap, {Xoffset, Yoffset}) ->
	lists:foldl(
		fun ({Bit, {X, Y}}, FieldAcc) ->
			CellState = if
				(Bitmap band (1 bsl Bit)) /= 0 -> cellstate_initvalue(RulesMod);
				true -> false
				end,
			FieldMod:set(FieldAcc, X + Xoffset, Y + Yoffset, CellState)
			end,
		Field,
		lists:zip(
			lists:seq(0, 8),
			[{-1, -1}, {0, -1}, {1, -1},
				{-1, 0}, {0, 0}, {1, 0},
				{-1, 1}, {0, 1}, {1, 1}
				]
		)
	)
.


cellstate_initvalue(cgolam_rules_normal) ->
	true
;
cellstate_initvalue(cgolam_rules_coloured) ->
	{col, {
		trunc(rand:uniform() * 256) band 16#f8,
		trunc(rand:uniform() * 256) band 16#f8,
		trunc(rand:uniform() * 256) band 16#f8
	}}
;
cellstate_initvalue(cgolam_rules_species1) ->
	{col, {40, 200, 100}}
;
cellstate_initvalue(cgolam_rules_species2) ->
	{col, {40, 200, 100}}
;
cellstate_initvalue(cgolam_rules_species3) ->
	{col, {40, 200, 100}}
.


test_init(Config) ->
	{tc_group_properties, GrpProps} = lists:keyfind(tc_group_properties, 1, Config),
	{rules, RulesMod, RulesCfg} = lists:keyfind(rules, 1, GrpProps),
	{field, FieldMod, FieldCfg} = lists:keyfind(field, 1, RulesCfg),
	Rules = RulesMod:new(RulesCfg),
	Field1 = FieldMod:new(FieldCfg),
	Field2 = RulesMod:init(Rules, Field1, default, []),
	AllCells = FieldMod:all(Field2),
	AliveCount = lists:foldl(
		fun ({_Coords, false}, Acc) -> Acc;
			({_Coords, _Alive}, Acc) -> Acc + 1
			end,
		0,
		AllCells
	),
	TotalCells = length(AllCells),
	FalseCount = TotalCells - AliveCount,
	AlivePercent = AliveCount / TotalCells * 100,
	FalsePercent = FalseCount / TotalCells * 100,
	if (FalsePercent < 1) or (FalsePercent > 99) ->
			throw({false_count_unfeasible, FalsePercent});
		true ->
			ok
		end,
	if (AlivePercent < 1) or (AlivePercent > 99) ->
			throw({alive_count_unfeasible, AlivePercent});
		true ->
			ok
		end,
	AnotherField1 = FieldMod:new(FieldCfg),
	AnotherField2 = RulesMod:init(
		Rules,
		AnotherField1,
		term,
		[{set, [
			{{1, 1}, cellstate_initvalue(RulesMod)},
			{{1, 2}, cellstate_initvalue(RulesMod)},
			{{0, 4}, cellstate_initvalue(RulesMod)}
			]}]
		),
	AnotherAllCells = FieldMod:all(AnotherField2),
	AnotherAliveCount = lists:foldl(
		fun ({_Coords, false}, Acc) -> Acc;
			({_Coords, _Alive}, Acc) -> Acc + 1
			end,
		0,
		AnotherAllCells
	),
	3 = AnotherAliveCount,
	ok
.
