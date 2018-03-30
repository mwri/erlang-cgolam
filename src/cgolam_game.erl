%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc Game server module for 'cgolam' app.


-module(cgolam_game).


-behaviour(gen_server).


-export([start_link/1, stop/1]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).


-define(default_interval, 50).


-record(cgolam_game, {
	interval   :: integer(),
	fieldmod   :: module(),
	field      :: cgolam_field:field(),
	rulesmod   :: module(),
	rules      :: cgolam_rules:rules(),
	displaymod :: module(),
	display    :: cgolam_display:display()
}).


%% @private

-spec start_link
	(list()) ->
		{ok, pid()} .

start_link(GameCfg) ->
	gen_server:start_link(?MODULE, GameCfg, [])
.


%% @private

-spec stop
	(Pid :: pid()) ->
		ok .

stop(Pid) ->
	gen_server:cast(Pid, stop),
	ok
.



%% @private

init(GameCfg) ->
	process_flag(trap_exit, true),
	{value, {width, Width}} = lists:keysearch(width, 1, GameCfg),
	{value, {height, Height}} = lists:keysearch(height, 1, GameCfg),
	{RulesMod, RulesCfg} = case lists:keysearch(rules, 1, GameCfg) of
		{value, {rules, Mr}} -> {Mr, []};
		{value, {rules, Mr, Cr}} -> {Mr, Cr};
		false -> {cgolam_rules_normal, []}
		end,
	{FieldMod, FieldCfg} = case lists:keysearch(field, 1, GameCfg) of
		{value, {field, Mf}} -> {Mf, []};
		{value, {field, Mf, Cf}} -> {Mf, Cf};
		false -> {cgolam_field_ets, []}
		end,
	{InitMod, InitType, InitCfg} = case lists:keysearch(init, 1, GameCfg) of
		{value, {init, Mi, Ti}} -> {Mi, Ti, []};
		{value, {init, Mi, Ti, Ci}} -> {Mi, Ti, Ci};
		false -> {RulesMod, default, []}
		end,
	{DisplayMod, DisplayCfg} = case lists:keysearch(display, 1, GameCfg) of
		{value, {display, Md}} -> {Md, []};
		{value, {display, Md, Cd}} -> {Md, Cd};
		false -> {cgolam_display_wx, []}
		end,
	Title = case lists:keysearch(title, 1, GameCfg) of
		{value, {title, S}} -> S;
		false when RulesMod == cgolam_rules_normal -> "Conway's Game of Life";
		false -> "Unknown"
		end,
	Interval = case lists:keysearch(interval, 1, GameCfg) of
		{value, {interval, I}} -> I;
		false -> ?default_interval
		end,
	FullRulesCfg = [{field, FieldMod, FieldCfg} | RulesCfg],
	FullFieldCfg = [{width, Width} | [{height, Height} | FieldCfg]],
	FullDisplayCfg1 = case lists:keysearch(title, 1, DisplayCfg) of
		{value, {title, _Title}} -> DisplayCfg;
		false -> [{title, Title} | DisplayCfg]
		end,
	FullDisplayCfg2 = [{width, Width} | [{height, Height} | FullDisplayCfg1]],
	Rules = RulesMod:new(FullRulesCfg),
	Field1 = FieldMod:new(FullFieldCfg),
	Field2 = InitMod:init(Rules, Field1, InitType, InitCfg),
	Display1 = DisplayMod:new(FullDisplayCfg2),
	Display2 = DisplayMod:update(Display1, FieldMod:all(Field2)),
	Display3 = DisplayMod:sync(Display2),
	self() ! interval,
	{ok, #cgolam_game{
		interval   = Interval,
		fieldmod   = FieldMod,
		field      = Field2,
		rulesmod   = RulesMod,
		rules      = Rules,
		displaymod = DisplayMod,
		display    = Display3
	}}
.


%% @private

code_change(_OldVsn, State, _Extra) ->
	{ok, State}
.


%% @private

handle_info(interval, State = #cgolam_game{
		interval   = Interval,
		fieldmod   = FieldMod,
		field      = Field0,
		rulesmod   = RulesMod,
		rules      = Rules,
		displaymod = DisplayMod,
		display    = Display0
		}) ->
	erlang:send_after(Interval, self(), interval),
	Updates = lists:foldl(
		fun ({Coords = {X, Y}, CurrentCellState}, UpdatesAcc) ->
			NewCellState = RulesMod:calc(Rules, Field0, X, Y),
			if CurrentCellState /= NewCellState ->
					[{Coords, RulesMod:calc(Rules, Field0, X, Y)} | UpdatesAcc];
				true ->
					UpdatesAcc
				end
			end,
		[],
		FieldMod:all(Field0)
		),
	Field1 = lists:foldl(
		fun ({{X, Y}, CellState}, FieldAcc) ->
			FieldMod:set(FieldAcc, X, Y, CellState)
			end,
		Field0,
		Updates
	),
	Display1 = DisplayMod:update(Display0, Updates),
	Display2 = DisplayMod:sync(Display1),
	{noreply, State#cgolam_game{
		field   = Field1,
		display = Display2
	}}
;

handle_info({'EXIT', _Pid, normal}, State) ->
	{stop, normal, State}
;

handle_info({'EXIT', _Pid, Error}, State) ->
	{stop, {child_process_exited, Error}, State}
;

handle_info(Msg, State) ->
	{stop, {error, {unexpected_info, Msg}}, State}
.


%% @private

handle_cast(stop, State) ->
	{stop, normal, State}
;

handle_cast(Msg, State) ->
	{stop, {error, {unexpected_cast, Msg}}, State}
.



%% @private

handle_call(Msg, _From, State) ->
	{reply, {error, {unexpected_call, Msg}}, {error, {unexpected_cast, Msg}}, State}
.


%% @private

terminate(_Reason, _State) ->
	undefined
.
