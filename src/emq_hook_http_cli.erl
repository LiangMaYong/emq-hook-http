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

-module(emq_hook_http_cli).

-include_lib("emqttd/include/emqttd.hrl").

-export([request/3, feed_params_val/5, feed_params_val/6, feed_params_val/7, parser_app_id/1]).

%%--------------------------------------------------------------------
%% HTTP Request
%%--------------------------------------------------------------------

request(get, Url, Params) ->
  Req = {Url ++ "?" ++ mochiweb_util:urlencode(Params), []},
  reply(httpc:request(get, Req, [{autoredirect, true}], []));

request(post, Url, Params) ->
  Req = {Url, [], "application/x-www-form-urlencoded", mochiweb_util:urlencode(Params)},
  reply(httpc:request(post, Req, [{autoredirect, true}], [])).

reply({ok, {{_, Code, _}, _Headers, Body}}) ->
  {ok, Code, Body};
reply({ok, Code, Body}) ->
  {ok, Code, Body};
reply({error, Error}) ->
  {error, Error}.

%%--------------------------------------------------------------------
%% HTTP feed_params_val
%%--------------------------------------------------------------------

feed_params_val(Params, ClientId, Username, Action, AppKey) ->
  lists:map(fun
              ({Param, "%c"}) -> {Param, ClientId};
              ({Param, "%u"}) -> {Param, Username};
              ({Param, "%a"}) -> {Param, Action};
              ({Param, "%ak"}) -> {Param, AppKey};
              ({Param, "%t"}) -> {Param, ""};
              ({Param, "%p"}) -> {Param, ""};
              (Param) -> Param
            end, Params).

feed_params_val(Params, ClientId, Username, Action, AppKey, Topic) ->
  lists:map(fun
              ({Param, "%c"}) -> {Param, ClientId};
              ({Param, "%u"}) -> {Param, Username};
              ({Param, "%a"}) -> {Param, Action};
              ({Param, "%ak"}) -> {Param, AppKey};
              ({Param, "%t"}) -> {Param, Topic};
              ({Param, "%p"}) -> {Param, ""};
              (Param) -> Param
            end, Params).

feed_params_val(Params, ClientId, Username, Action, AppKey, Topic, Payload) ->
  lists:map(fun
              ({Param, "%c"}) -> {Param, ClientId};
              ({Param, "%u"}) -> {Param, Username};
              ({Param, "%a"}) -> {Param, Action};
              ({Param, "%ak"}) -> {Param, AppKey};
              ({Param, "%t"}) -> {Param, Topic};
              ({Param, "%p"}) -> {Param, Payload};
              (Param) -> Param
            end, Params).

parser_app_id(_ClientId) ->
  "ad".



