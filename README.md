erlvox
=====

An Erlang library to use VOICEVOX

https://voicevox.hiroshiba.jp/

Build
-----

    $ rebar3 compile


Sample
-----

You will need voicevox engine running.
Let's say you have voicevox engine running on:
- `http://127.0.0.1:50021`
- `http://192.168.64.50:50021`
- `http://192.168.64.51:50021`
- `http://192.168.64.52:50021`

voicevox engine on localhost (`http://127.0.0.1:50021`) should only be used for query building.

```erlang
{ok,AudioQuery} = erlvox:audio_query(
  {{127,0,0,1}, 50021}, % voicevox engine for query building.
  3, % speaker id.
  <<"あいうえお"/utf8>> % text for synthesizing.
),
{ok, Audio} = erlvox:synthesis(
  [ % voicevox engines for synthesizing.
    {{192,168,64,50}, 50021}, 
    {{192,168,64,51}, 50021}, 
    {{192,168,64,52}, 50021}
  ],
  3, % speaker id.
  AudioQuery
).
```

<details>
<summary>Expand shell

    $ rebar3 shell
</summary>

```
Erlang/OTP 24 [erts-12.2.1] [source] [64-bit] [smp:24:24] [ds:24:24:10] [async-threads:1] [jit]

Eshell V12.2.1  (abort with ^G)
1> {ok,AudioQuery} = erlvox:audio_query(
1>   {{127,0,0,1}, 50021}, % voicevox engine for query building.
1>   3, % speaker id.
1>   <<"あいうえお"/utf8>> % text for synthesizing.
1> ),
1> {ok, Audio} = erlvox:synthesis(
1>   [ % voicevox engines for synthesizing.
1>     {{192,168,64,50}, 50021}, 
1>     {{192,168,64,51}, 50021}, 
1>     {{192,168,64,52}, 50021}
1>   ],
1>   3, % speaker id.
1>   AudioQuery
1> ).
{ok,<<82,73,70,70,36,192,0,0,87,65,86,69,102,109,116,32,
      16,0,0,0,1,0,1,0,192,93,0,...>>}
2> 
```
</details>
