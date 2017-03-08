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

-module(emq_hook_http).

-include("emq_hook_http.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-export([load/1, unload/0]).

-import(emq_hook_http_cli, [request/3, feed_params_val/6, feed_params_val/8]).

%% Hooks functions

-export([on_client_connected/3, on_client_disconnected/3]).

-export([on_client_subscribe/4, on_client_unsubscribe/4]).

-export([on_session_created/3, on_session_subscribed/4, on_session_unsubscribed/4, on_session_terminated/4]).

%% Called when the plugin application start
load(Env) ->
  emqttd:hook('client.connected', fun ?MODULE:on_client_connected/3, [Env]),
  emqttd:hook('client.disconnected', fun ?MODULE:on_client_disconnected/3, [Env]),
  emqttd:hook('client.subscribe', fun ?MODULE:on_client_subscribe/4, [Env]),
  emqttd:hook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/4, [Env]),
  emqttd:hook('session.created', fun ?MODULE:on_session_created/3, [Env]),
  emqttd:hook('session.subscribed', fun ?MODULE:on_session_subscribed/4, [Env]),
  emqttd:hook('session.unsubscribed', fun ?MODULE:on_session_unsubscribed/4, [Env]),
  emqttd:hook('session.terminated', fun ?MODULE:on_session_terminated/4, [Env]).

%% Called when the plugin application stop
unload() ->
  emqttd:unhook('client.connected', fun ?MODULE:on_client_connected/3),
  emqttd:unhook('client.disconnected', fun ?MODULE:on_client_disconnected/3),
  emqttd:unhook('client.subscribe', fun ?MODULE:on_client_subscribe/4),
  emqttd:unhook('client.unsubscribe', fun ?MODULE:on_client_unsubscribe/4),
  emqttd:unhook('session.created', fun ?MODULE:on_session_created/3),
  emqttd:unhook('session.subscribed', fun ?MODULE:on_session_subscribed/4),
  emqttd:unhook('session.unsubscribed', fun ?MODULE:on_session_unsubscribed/4),
  emqttd:unhook('session.terminated', fun ?MODULE:on_session_terminated/4).

%% ------------------------------------------------------------------------------------------------
%% Client
%% ------------------------------------------------------------------------------------------------

on_client_connected(ConnAck, Client = #mqtt_client{username = Username, client_id = ClientId}, _Env) ->
  io:format("\n client ~s connected, connack: ~w~n", [ClientId, ConnAck]),
  do_hook_request(ClientId, Username, "on_client_connected", "", Client).

on_client_disconnected(Reason, _Client = #mqtt_client{username = Username, client_id = ClientId}, _Env) ->
  io:format("\n client ~s disconnected, reason: ~w~n", [ClientId, Reason]),
  do_hook_request(ClientId, Username, "on_client_disconnected", "").

on_client_subscribe(ClientId, Username, TopicTable = #mqtt_topic{topic = Topic}, _Env) ->
  io:format("\n client(~s/~s) will subscribe: ~p~n", [Username, ClientId, TopicTable]),
  do_hook_request(ClientId, Username, "on_client_subscribe", binary_to_list(Topic), TopicTable).

on_client_unsubscribe(ClientId, Username, TopicTable = #mqtt_topic{topic = Topic}, _Env) ->
  io:format("\n client(~s/~s) unsubscribe ~p~n", [ClientId, Username, TopicTable]),
  do_hook_request(ClientId, Username, "on_client_unsubscribe", binary_to_list(Topic), TopicTable).

%% ------------------------------------------------------------------------------------------------
%% Session
%% ------------------------------------------------------------------------------------------------

on_session_created(ClientId, Username, _Env) ->
  io:format("\n session(~s/~s) created.", [ClientId, Username]),
  do_hook_request(ClientId, Username, "on_session_created", "").

on_session_subscribed(ClientId, Username, {Topic, Opts}, _Env) ->
  io:format("\n session(~s/~s) subscribed: ~p~n", [Username, ClientId, {Topic, Opts}]),
  do_hook_request(ClientId, Username, "on_session_subscribed", "", {Topic, Opts}).

on_session_unsubscribed(ClientId, Username, {Topic, Opts}, _Env) ->
  io:format("\n session(~s/~s) unsubscribed: ~p~n", [Username, ClientId, {Topic, Opts}]),
  do_hook_request(ClientId, Username, "on_session_unsubscribed", "").

on_session_terminated(ClientId, Username, Reason, _Env) ->
  io:format("\n session(~s/~s) terminated: ~p.", [ClientId, Username, Reason]),
  do_hook_request(ClientId, Username, "on_session_terminated", "", Reason).

%% ---------------------------
%% do_hook_request
%% ---------------------------

do_hook_request(ClientId, Username, Action, Topic, Obj) ->
  HookReq = r(application:get_env(emq_hook_http, hook_req, undefined)),
  do_http_request(ClientId, Username, Action, Topic, HookReq),
  {ok, Obj}.

do_hook_request(ClientId, Username, Action, Topic) ->
  HookReq = r(application:get_env(emq_hook_http, hook_req, undefined)),
  do_http_request(ClientId, Username, Action, Topic, HookReq),
  ok.

do_http_request(ClientId, Username, Action, Topic, #http_request{method = Method, url = Url, params = Params, appkey = Appkey}) ->
  case request(Method, Url, feed_params_val(Params, ClientId, Username, Action, Appkey, Topic)) of
    {ok, 200, _Body} -> ok;
    {ok, _Code, _Body} -> error;
    {error, Error} -> lager:error("HTTP ~s Error: ~p", [Url, Error]), error
  end.

