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

-export([load/1, unload/0]).

-import(emq_hook_http_cli, [request/3, feed_params_val/5, feed_params_val/8]).

%% Hooks functions

-export([on_message_publish/2, on_message_delivered/4, on_message_acked/4]).

%% Called when the plugin application start
load(Env) ->
  emqttd:hook('message.publish', fun ?MODULE:on_message_publish/2, [Env]),
  emqttd:hook('message.delivered', fun ?MODULE:on_message_delivered/4, [Env]),
  emqttd:hook('message.acked', fun ?MODULE:on_message_acked/4, [Env]).

%% ------------------------------------------------------------------------------------

%% transform message and return
on_message_publish(Message = #mqtt_message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  {ok, Message};

on_message_publish(Message, _Env) ->
  io:format("\n publish ~s~n", [emqttd_message:format(Message)]),
  Action = on_message_publish,
  do_hook_request("", "", Action, Message).

on_message_delivered(ClientId, Username, Message, _Env) ->
  io:format("\n delivered to client(~s/~s): ~s~n", [Username, ClientId, emqttd_message:format(Message)]),
  Action = on_message_delivered,
  do_hook_request(ClientId, Username, Action, Message).

on_message_acked(ClientId, Username, Message, _Env) ->
  io:format("\n client(~s/~s) acked: ~s~n", [Username, ClientId, emqttd_message:format(Message)]),
  Action = on_message_acked,
  do_hook_request(ClientId, Username, Action, Message).

%% ------------------------------------------------------------------------------------

%% Called when the plugin application stop
unload() ->
  emqttd:unhook('message.publish', fun ?MODULE:on_message_publish/2),
  emqttd:unhook('message.delivered', fun ?MODULE:on_message_delivered/4),
  emqttd:unhook('message.acked', fun ?MODULE:on_message_acked/4).

%% ---------------------------
%% do_hook_request
%% ---------------------------

do_hook_request(ClientId, Username, Action, Message) ->
  HookReq = r(application:get_env(emq_hook_http, hook_req, undefined)),
  do_http_request(ClientId, Username, Action, HookReq, Message),
  {ok, Message}.

do_http_request(ClientId, Username, Action, #http_request{method = Method, url = Url, params = Params, appkey = Appkey}, Message) ->
  Topic = binary_to_list(Message#mqtt_message.topic),
  Msg = binary_to_list(Message#mqtt_message.payload),
  From = binary_to_list(Message#mqtt_message.from),
  case request(Method, Url, feed_params_val(Params, ClientId, Username, Action, Appkey, Topic, Msg, From)) of
    {ok, 200, _Body} -> ok;
    {ok, _Code, _Body} -> error;
    {error, Error} -> lager:error("HTTP ~s Error: ~p", [Url, Error]), error
  end.
