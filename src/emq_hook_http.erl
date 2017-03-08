%%--------------------------------------------------------------------
%% EMQ HOOK HTTP PLUGIN 
%%--------------------------------------------------------------------

-module(emq_hook_http).

-include("emq_hook_http.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-export([load/1, unload/0]).

-import(emq_hook_http_cli, [request/3, feedvar/2, feedvar/3]).

%% Hooks functions

-export([on_client_connected/3, on_client_disconnected/3]).

%% Called when the plugin application start
load(Env) ->
    emqttd:hook('client.connected', fun ?MODULE:on_client_connected/3, [Env]),
    emqttd:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3, [Env]).

on_client_connected(ConnAck, Client = #mqtt_client{client_id = ClientId}, _Env) ->
    io:format("client ~s connected, connack: ~w~n", [ClientId, ConnAck]),
    {ok, Client}.

on_client_disconnected(Reason, _Client = #mqtt_client{client_id = ClientId}, _Env) ->
    io:format("client ~s disconnected, reason: ~w~n", [ClientId, Reason]),
    ok.

%% Called when the plugin application stop
unload() ->
    emqttd:unhook('client.connected', fun ?MODULE:on_client_connected/3),
    emqttd:unhook('client.disconnected', fun ?MODULE:on_client_disconnected/3).

