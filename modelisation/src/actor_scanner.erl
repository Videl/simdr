-module(actor_scanner).
-include("app_configuration.hrl").

% -ifdef(TEST).
% -include_lib("eunit/include/eunit.hrl").
% -endif.

-behaviour(actor_contract).

%% Actor Contract Behaviors Callbacks

-export([
	create/0,
	answer/2]).

%% Behavior implementation

create() ->
	actor_contract:create(?MODULE, actor_contract:random_id(), [], undefined, 2, []).

answer(ScannerConfig, {actor_product, ProductConfig}) ->
	actor_contract:work(actor_contract:get_work_time(ScannerConfig)),
	case actor_contract:get_option(ProductConfig, quality) =:= unknown_option  of 
	true -> 
		Transfo = actor_contract:get_option(ProductConfig, processed),
		[{Quality, Luck}] = Transfo,
		FinalQuality = actor_contract:random_weighted(Luck, Quality),
		ProductConf = actor_contract:set_option(
			ProductConfig, 
			quality, 
			FinalQuality);
	false -> 
		FinalQuality = actor_contract:get_option(ProductConfig, quality),
		ProductConf =  ProductConfig
	end,
	{NewScannerConfig, NewProductConfig} = actor_contract:add_to_list_data(
	ScannerConfig, {scanned,product, {ProductConf, quality, is, FinalQuality}},
	ProductConf, {was,scanned,by,{ScannerConfig, quality, is, FinalQuality}}),
	% Answer
	{NewScannerConfig, 
	{actor_product, NewProductConfig, actor_contract:get_id(NewProductConfig)}, 
	supervisor};

answer(RFIDConfig, Request) ->
	actor_contract:answer(RFIDConfig, Request).