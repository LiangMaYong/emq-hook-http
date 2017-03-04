PROJECT = emq_hook_http
PROJECT_DESCRIPTION = EMQ Hook Http Plugin
PROJECT_VERSION = 1.0.0

BUILD_DEPS = emqttd cuttlefish
dep_emqttd = git https://github.com/emqtt/emqttd master
dep_cuttlefish = git https://github.com/emqtt/cuttlefish

NO_AUTOPATCH = cuttlefish

COVER = true

include erlang.mk

app:: rebar.config

app.config::
	./deps/cuttlefish/cuttlefish -l info -e etc/ -c etc/emq_hook_http.conf -i priv/emq_hook_http.schema -d data
