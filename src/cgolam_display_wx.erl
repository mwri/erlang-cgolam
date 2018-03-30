%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc WX widgets display implementation module for 'cgolam' app.


-module(cgolam_display_wx).


-behaviour(cgolam_display).
-behaviour(gen_server).


-export([new/1, stop/1, update/2, sync/1]).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).


-define(frame_file_exit, 400).


-include_lib("wx/include/wx.hrl").


-record(cgolam_display_wx_svr, {
	width           :: integer(),
	height          :: integer(),
	sqsize          :: integer(),
	data            :: ets:tab(),
	wx              :: wx:wx_object(),
	frame           :: wxFrame:wxFrame(),
	wxdc            :: wxDC:wxDC(),
	white_brush     :: wxBrush:wxBrush(),
	black_brush     :: wxBrush:wxBrush(),
	brushes         :: ets:tid(),
	gen         = 0 :: integer()
}).


-type cgolam_display_wx() :: cgolam_display:display() .


-export_type([cgolam_display_wx/0]).


%% @private

-spec new
	(DisplayModCfg :: list()) ->
		cgolam_display_wx() .

new(DisplayModCfg) ->
	{ok, Pid} = start_link(DisplayModCfg),
	%erlang:monitor(process, Pid),
	Pid
.


%% @private

-spec update
	(Display0 :: cgolam_display_wx(), FieldUpdate :: cgolam_display:updates()) ->
		Display1 :: term() .

update(Pid, FieldUpdate) ->
	Pid ! {update, FieldUpdate},
	Pid
.


%% @private

-spec sync
	(Display0 :: cgolam_display_wx()) ->
		Display1 :: cgolam_display_wx() .

sync(Pid) ->
	Pid ! sync,
	Pid
.


%% @private

-spec stop
	(Display :: cgolam_display_wx()) ->
		ok .

stop(Pid) ->
	gen_server:cast(Pid, stop),
	ok
.


%% @private

-spec start_link
	(list()) ->
		{ok, pid()} .

start_link(GameCfg) ->
	gen_server:start_link(?MODULE, GameCfg, [])
.


%% @private

init(DisplayModCfg) ->
	{value, {title, Title}} = lists:keysearch(title, 1, DisplayModCfg),
	{value, {width, Width}} = lists:keysearch(width, 1, DisplayModCfg),
	{value, {height, Height}} = lists:keysearch(height, 1, DisplayModCfg),
	SqSize = case lists:keysearch(sqsize, 1, DisplayModCfg) of
		{value, {sqsize, I}} -> I;
		false -> 4
		end,
	Wx = wx:new(),
	Frame = wxFrame:new(Wx, ?wxID_ANY, Title),
	StatusBar = wxFrame:createStatusBar(Frame),
	wxFrame:setStatusText(Frame, io_lib:format("Width ~b height ~b generation 1", [Width, Height])),
	MenuBar = wxMenuBar:new(),
	{_MbWidth, MbHeight} = wxWindow:getSize(MenuBar),
	{_SbWidth, SbHeight} = wxWindow:getSize(StatusBar),
	% ugh, how do I get the title bar height?
	TbHeight = 37,
	FileMenu = wxMenu:new(),
	ExitMenuOption = wxMenuItem:new([{id, ?frame_file_exit}, {text, "&Exit"}]),
	wxMenu:append(FileMenu, ExitMenuOption),
	wxMenuBar:append(MenuBar, FileMenu, "&File"),
	wxFrame:setMenuBar(Frame, MenuBar),
	ExtWidth = Width * SqSize,
	% ugh, where does this 8 come from, do the TB, MB, SB and
	% frame use 2 pixels extra each, not sure, need to research
	ExtHeight = Height * SqSize + MbHeight + SbHeight + TbHeight + 8,
	SaneWidth = if ExtWidth >= 200 -> ExtWidth; true -> 200 end,
	SaneHeight = if ExtHeight >= 200 -> ExtHeight; true -> 200 end,
	wxWindow:setSize(Frame, {0, 0, SaneWidth, SaneHeight}),
	true = wxFrame:show(Frame),
	wxFrame:connect(Frame, close_window),
	wxFrame:connect(Frame, paint),
	wxFrame:connect (Frame, command_menu_selected),
	WxDc = wxPaintDC:new(Frame),
	Brushes = ets:new(brushes, [set]),
	WhiteBrush = wxBrush:new({255, 255, 255}),
	ets:insert(Brushes, {{255, 255, 255}, WhiteBrush}),
	BlackBrush = wxBrush:new({0, 0, 0}),
	ets:insert(Brushes, {{0, 0, 0}, BlackBrush}),
	Data = ets:new(data, [set]),
	{ok, #cgolam_display_wx_svr{
		width       = Width,
		height      = Height,
		sqsize      = SqSize,
		data        = Data,
		wx          = Wx,
		frame       = Frame,
		wxdc        = WxDc,
		white_brush = WhiteBrush,
		black_brush = BlackBrush,
		brushes     = Brushes
	}}
.


%% @private

code_change(_OldVsn, State, _Extra) ->
	{ok, State}
	.


%% @private

handle_info({update, FieldUpdate}, State = #cgolam_display_wx_svr{
		frame = Frame
		}) ->
	WxDc = wxPaintDC:new(Frame),
	server_update(State#cgolam_display_wx_svr{wxdc=WxDc}, FieldUpdate),
	wxPaintDC:destroy(WxDc),
	{noreply, State}
;

handle_info(sync, State = #cgolam_display_wx_svr{}) ->
	{noreply, server_sync(State)}
;

handle_info(
		#wx{obj=Frame,event=#wxClose{type=close_window}},
		State = #cgolam_display_wx_svr{frame=Frame}
		) ->
	{stop, normal, State}
;

handle_info(
		#wx{id=?frame_file_exit,obj=Frame,event=#wxCommand{type=command_menu_selected}},
		State = #cgolam_display_wx_svr{frame=Frame}
		) ->
	{stop, normal, State}
;

handle_info(#wx{event=#wxPaint{type=paint}}, State = #cgolam_display_wx_svr{
		data  = Data,
		frame = Frame
		}) ->
	WxDc = wxPaintDC:new(Frame),
	server_update(State#cgolam_display_wx_svr{wxdc=WxDc}, ets:tab2list(Data)),
	wxPaintDC:destroy(WxDc),
	{noreply, State}
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
	{stop, {error, {unexpected_call, Msg}}, {error, {unexpected_call, Msg}}, State}
.


%% @private

terminate(_Reason, #cgolam_display_wx_svr{
		wxdc = WxDc
		}) ->
	wxPaintDC:destroy(WxDc),
	undefined
.


%% @private

server_update(State = #cgolam_display_wx_svr{
		sqsize      = SqSize,
		data        = Data,
		wxdc        = WxDc,
		black_brush = BlackBrush
		}, [CellState = {{X, Y}, false} | T]) ->
	ets:insert(Data, CellState),
	wxDC:setBrush(WxDc, BlackBrush),
	wxDC:drawRectangle(WxDc, {X*SqSize, Y*SqSize, SqSize, SqSize}),
	server_update(State, T)
;
server_update(State = #cgolam_display_wx_svr{
		sqsize      = SqSize,
		data        = Data,
		wxdc        = WxDc,
		white_brush = WhiteBrush
		}, [CellState = {{X, Y}, true} | T]) ->
	ets:insert(Data, CellState),
	wxDC:setBrush(WxDc, WhiteBrush),
	wxDC:drawRectangle(WxDc, {X*SqSize, Y*SqSize, SqSize, SqSize}),
	server_update(State, T)
;
server_update(State = #cgolam_display_wx_svr{
		sqsize  = SqSize,
		data    = Data,
		wxdc    = WxDc,
		brushes = Brushes
		}, [CellState = {{X, Y}, {col, Col}} | T]) ->
	ets:insert(Data, CellState),
	Brush = case ets:lookup(Brushes, Col) of
		[{Col, B}] -> B;
		[] -> B = wxBrush:new(Col), ets:insert(Brushes, {Col, B}), B
		end,
	wxDC:setBrush(WxDc, Brush),
	wxDC:drawRectangle(WxDc, {X*SqSize, Y*SqSize, SqSize, SqSize}),
	server_update(State, T)
;
server_update(State = #cgolam_display_wx_svr{}, []) ->
	State
;
server_update(State=#cgolam_display_wx_svr{}, FieldUpdate) when not is_list(FieldUpdate) ->
	server_update(State, [FieldUpdate])
.


%% @private

server_sync(State = #cgolam_display_wx_svr{
		width  = Width,
		height = Height,
		frame  = Frame,
		gen    = Gen
		}) ->
	wxFrame:setStatusText(Frame, io_lib:format("Width ~b height ~b generation ~b", [Width, Height, Gen])),
	State#cgolam_display_wx_svr{gen=Gen+1}
.
