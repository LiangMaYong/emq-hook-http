{application, emq_hook_http, [
	{description, "EMQ Hook Http Plugin"},
	{vsn, "1.0.0"},
	{id, "3394549-dirty"},
	{modules, ['emq_hook_http','emq_hook_http_app','emq_hook_http_cli','emq_hook_http_message','emq_hook_http_subscription','emq_hook_http_super']},
	{registered, [emq_hook_http_sup]},
	{applications, [kernel,stdlib]},
	{mod, {emq_hook_http_app, []}}
]}.