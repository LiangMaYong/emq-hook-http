## emq_hook_http
EMQ broker hook http plugin

Home : [https://github.com/LiangMaYong/emq-hook-http](https://github.com/LiangMaYong/emq-hook-http)

## Setup
In Makefile,
```
DEPS += emq_hook_http

dep_emq_hook_http = git https://github.com/liangmayong/emq-hook-http master
```
In relx.config
```
{emq_emq_hook_http, load}
```
In _rel/emqttd/etc/plugins/emq_emq_hook_http.conf
```
##--------------------------------------------------------------------
##  HOOK HTTP CONFIG:
##--------------------------------------------------------------------
##  %u = username
##  %c = clientid
##  %a = action
##  %ak = appkey
##  %t = topic
##  %p = payload
##--------------------------------------------------------------------
emq_hook_http.hook_req = http://127.0.0.1:8080/mqtt/hook
emq_hook_http.hook_req.appkey = emq_appkey
emq_hook_http.hook_req.method = post
emq_hook_http.hook_req.params = clientid=%c,username=%u,action=%a,appkey=%ak,topic=%t,payload=%p
```
## LICENSE
Copyright © LiangMaYong

Distributed under [MIT](https://github.com/LiangMaYong/emq-hook-http/blob/master/LICENSE) license.

## Author
LiangMaYong

ibeam@qq.com
