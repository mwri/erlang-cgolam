%% @author Michael Wright <mjw@methodanalysis.com>
%% @copyright 2018 Michael Wright <mjw@methodanalysis.com>
%% 
%% @doc Rules behaviour specification module for 'cgolam' app.


-module(cgolam_rules).


-type rules() :: term() .


-export_type([rules/0]).


-callback new
	(RulesModCfg :: list()) ->
		Rules :: rules() .

-callback calc
	(Rules :: rules(), Field :: cgolam_field:field(), X :: integer(), Y :: integer()) ->
		CellState :: term() .

-callback init
	(Rules :: rules(), Field0 :: cgolam_field:field(), Type :: atom(), InitCfg :: list()) ->
		Field1 :: cgolam_field:field() .
