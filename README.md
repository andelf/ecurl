# ecurl - erlang libcurl nif

pull requests are welcome.

	NOTE: this is a working-in-progress. DO NOT use in production.

TODO:

```
nif doesn't support calling erlang, so we can't pass call back fun. may be
port drivers a better.
```

```erlang
1> l(ecurl).
{module,ecurl}
2> {ok, H} = ecurl:easy_init().
{ok,<<>>}
3> ecurl:easy_setopt(H, verbose, 1).
ok
4> ecurl:easy_setopt(H, url, "http://www.baidu.com").
ok
5> ecurl:easy_perform(H).
...blah...
```
