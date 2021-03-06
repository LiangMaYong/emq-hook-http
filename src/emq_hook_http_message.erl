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

-module(emq_hook_http_message).

-include("emq_hook_http.hrl").

-include_lib("emqttd/include/emqttd.hrl").

-include_lib("emqttd/include/emqttd_protocol.hrl").

-export([load/1, unload/0]).

-import(emq_hook_http_cli, [request/3, feed_params_val/5, feed_params_val/6, feed_params_val/7, parser_app_id/1]).

%% Hooks functions

-export([on_message_publish/2, on_message_delivered/4, on_message_ack/4]).

%% Called when the plugin application start
load(Env) ->
  emqttd:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]),
  emqttd:hook('message.delivered', fun ?MODULE:on_message_delivered/4, [Env]),
  emqttd:hook('message.acked', fun ?MODULE:on_message_ack/4, [Env]).

%% Called when the plugin application stop
unload() ->
  emqttd:unhook('message.publish', fun ?MODULE:on_message_publish/2),
  emqttd:unhook('message.delivered', fun ?MODULE:on_message_delivered/4),
  emqttd:unhook('message.acked', fun ?MODULE:on_message_ack/4).

%% -------------------------------------------------------
%% Message
%% -------------------------------------------------------

%% message publish
on_message_publish(Message = #mqtt_message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  {ok, Message};

%% message publish
on_message_publish(Message, _Env) ->
  io:format("\n publish ~s~n", [emqttd_message:format(Message)]),
  Action = on_message_publish,
  do_hook_request(Action, Message).

%% message delivered
on_message_delivered(ClientId, Username, Message, _Env) ->
  io:format("\n delivered to client(~s/~s): ~s~n", [Username, ClientId, emqttd_message:format(Message)]),
  Action = on_message_delivered,
  do_hook_request(ClientId, Username, Action, Message).

%% message ask
on_message_ack(ClientId, Username, Message = #mqtt_message{topic = Topic, payload = Payload}, _Env) ->
  io:format("\n client(~s/~s) acked: ~s~n", [Username, ClientId, emqttd_message:format(Message)]),
  Client = emqttd_cm:lookup(ClientId),
  % SubTopic = lists:concat(["$command/auto_sub/", Username, "/sub/"]),
  handle_auto_subscribe(Message, Client),
  Action = on_message_acked,
  do_hook_request(ClientId, Username, Action, Message).

%% -------------------------------------------------------
%% handle_auto_sub
%% -------------------------------------------------------

%% auto subscribe
handle_auto_subscribe(Message = #mqtt_message{topic = <<"$command/", _/binary>>, payload = Payload}, _Client = #mqtt_client{username = Username, client_id = ClientId, client_pid = ClientPid}) ->
  Topic = Message#mqtt_message.topic,
  io:format("\n  handle command message topic:~s, clientId:~s,pid:~w~n", [Topic, ClientId, ClientPid]),
  FlagSub = string:equal(binary_to_list(Topic), "$command/" ++ parser_app_id(ClientId) ++ "/subscribe/" ++ binary_to_list(Username) ++ "/"),
  FlagUnSub = string:equal(binary_to_list(Topic), "$command/" ++ parser_app_id(ClientId) ++ "/unsubscribe/" ++ binary_to_list(Username) ++ "/"),
  if
    FlagSub ->
      TopicTable = [{Payload, 1}],
      ClientPid ! {subscribe, TopicTable},
      true;
    FlagUnSub ->
      Topics = [Payload],
      ClientPid ! {unsubscribe, Topics},
      true;
    true ->
      false
  end,
  ok;

handle_auto_subscribe(_Message, _Client) ->
  ok.

%% -------------------------------------------------------
%% do_hook_request
%% -------------------------------------------------------

do_hook_request(Action, Message = #mqtt_message{topic = Topic, payload = Payload, from = {ClientId, Username}}) ->
  HookRequest = get_request(application:get_env(emq_hook_http, hook, undefined)),
  {do_http_request(ClientId, Username, Action, Topic, Payload, HookRequest), Message}.

do_hook_request(ClientId, Username, Action, Message = #mqtt_message{topic = Topic, payload = Payload}) ->
  HookRequest = get_request(application:get_env(emq_hook_http, hook, undefined)),
  {do_http_request(ClientId, Username, Action, Topic, Payload, HookRequest), Message}.

do_http_request(ClientId, Username, Action, Topic, Payload, #http_request{method = Method, url = Url, params = Params, appkey = Appkey}) ->
  case request(Method, Url, feed_params_val(Params, ClientId, Username, Action, Appkey, Topic, Payload)) of
    {ok, 200, _Body} -> ok;
    {ok, _Code, _Body} -> error;
    {error, _Error} -> error
  end.

get_request(Config) ->
  Method = proplists:get_value(method, Config, post),
  Url = proplists:get_value(url, Config),
  Params = proplists:get_value(params, Config),
  AppKey = proplists:get_value(appkey, Config),
  #http_request{method = Method, url = Url, params = Params, appkey = AppKey}.
