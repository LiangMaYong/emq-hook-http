%%--------------------------------------------------------------------
%% EMQ HOOK HTTP PLUGIN 
%%--------------------------------------------------------------------

-module(emq_hook_http_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    emq_hook_http:load(application:get_all_env()).

stop(_State) ->
    emq_hook_http:unload().
