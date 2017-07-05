%%%--------------------------------------------------------------------------------
%% The MIT License (MIT)
%%
%% Copyright (c) 2017 LiangMaYong
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/ or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%%--------------------------------------------------------------------------------

-module(emq_hook_http_connect).

-include("emq_hook_http.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-export([load/1, unload/0]).

-import(emq_hook_http_cli, [request/3, feed_params_val/5, feed_params_val/6, feed_params_val/7]).

%% Hooks functions

-export([on_client_connected/3, on_client_disconnected/3]).

-export([on_session_created/3, on_session_terminated/4]).

%% Called when the plugin application start
load(Env) ->
  emqttd:hook('client.connected', fun ?MODULE:on_client_connected/3, [Env]),
  emqttd:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3, [Env]),
  emqttd:hook('session.created', fun ?MODULE:on_session_created/3, [Env]),
  emqttd:hook('session.terminated', fun ?MODULE:on_session_terminated/4, [Env]).

%% Called when the plugin application stop
unload() ->
  emqttd:unhook('client.connected', fun ?MODULE:on_client_connected/3),
  emqttd:unhook('client.disconnected', fun ?MODULE:on_client_disconnected/3),
  emqttd:unhook('session.created', fun ?MODULE:on_session_created/3),
  emqttd:unhook('session.terminated', fun ?MODULE:on_session_terminated/4).

%% -------------------------------------------------------
%% Client
%% -------------------------------------------------------

on_client_connected(ConnAck, Client = #mqtt_client{username = Username, client_id = ClientId,client_pid = ClientPid}, _Env) ->
  io:format("\n client ~s connected, connack: ~w, clientPid:~w~n", [ClientId, ConnAck,ClientPid]),
  TopicTable = [{<<"TopicA">>,1}],
  ClientPid ! {subscribe,TopicTable},
  Action = on_client_connected,
  do_hook_request(ClientId, Username, Action, Client).

on_client_disconnected(Reason, _Client = #mqtt_client{username = Username, client_id = ClientId}, _Env) ->
  io:format("\n client ~s disconnected, reason: ~w~n", [ClientId, Reason]),
  Action = on_client_disconnected,
  do_hook_request(ClientId, Username, Action).

%% -------------------------------------------------------
%% Session
%% -------------------------------------------------------

on_session_created(ClientId, Username, _Env) ->
  io:format("\n session(~s/~s) created.", [ClientId, Username]),
  Action = on_session_created,
  do_hook_request(ClientId, Username, Action).

on_session_terminated(ClientId, Username, Reason, _Env) ->
  io:format("\n session(~s/~s) terminated: ~p.", [ClientId, Username, Reason]),
  Action = on_session_terminated,
  do_hook_request(ClientId, Username, Action, Reason).


%% -------------------------------------------------------
%% do_hook_request
%% -------------------------------------------------------

do_hook_request(ClientId, Username, Action, Obj) ->
  HookReq = get_req(application:get_env(emq_hook_http, hook_req, undefined)),
  {do_http_request(ClientId, Username, Action, HookReq), Obj}.

do_hook_request(ClientId, Username, Action) ->
  HookReq = get_req(application:get_env(emq_hook_http, hook_req, undefined)),
  do_http_request(ClientId, Username, Action, HookReq).

do_http_request(ClientId, Username, Action, #http_request{method = Method, url = Url, params = Params, appkey = Appkey}) ->
  case request(Method, Url, feed_params_val(Params, ClientId, Username, Action, Appkey)) of
    {ok, 200, _Body} -> ok;
    {ok, _Code, _Body} -> error;
    {error, _Error} -> error
  end.

get_req(Config) ->
  Method = proplists:get_value(method, Config, post),
  Url = proplists:get_value(url, Config),
  Params = proplists:get_value(params, Config),
  AppKey = proplists:get_value(appkey, Config),
  #http_request{method = Method, url = Url, params = Params, appkey = AppKey}.
