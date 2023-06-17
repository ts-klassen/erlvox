-module(erlvox).

-export([version/1, speakers/1, audio_query/3, synthesis/3]).

version(Host) ->
  request(Host, get, <<"/version">>).

speakers(Host) ->
  request(Host, get, <<"/speakers">>).

audio_query(Host, Speaker, Text) ->
  Uri = voicevox_uri(<<"/audio_query?">>, [
    {speaker, Speaker},
    {text, Text}
  ]),
  request(Host, post, Uri).

synthesis(Host, Speaker, AudioQuery) ->
  Uri = voicevox_uri(<<"/synthesis?">>, [
    {speaker, Speaker},
    {<<"enable_interrogative_upspeak">>, true}
  ]),
  request(Host, post, {Uri, AudioQuery, audio}).



voicevox_uri(Uri, []) -> Uri;
voicevox_uri(Uri, [{Key, true}|Data]) ->
  voicevox_uri(Uri, [{Key, <<"true">>}|Data]);
voicevox_uri(Uri, [{Key, false}|Data]) ->
  voicevox_uri(Uri, [{Key, <<"true">>}|Data]);
voicevox_uri(Uri, [{text, Value}|Data]) ->
  voicevox_uri(Uri, [{<<"text">>, Value}|Data]);
voicevox_uri(Uri, [{speaker, Value}|Data]) ->
  Speaker = list_to_binary(integer_to_list(Value)),
  voicevox_uri(Uri, [{<<"speaker">>, Speaker}|Data]);
voicevox_uri(Uri, [{Key, Value}|Data]) ->
  EncodedKey = cow_uri:urlencode(Key),
  EncodedValue = cow_uri:urlencode(Value),
  NextUri = <<
    Uri/binary,
    EncodedKey/binary, 
    <<"=">>/binary, 
    EncodedValue/binary, 
    <<"&">>/binary
  >>,
  voicevox_uri(NextUri, Data).

request(Host, Method, Data) ->
  try request(throws, Host, Method, Data) of
    Res -> {ok, Res}
  catch
    throw:X -> {error, {caught, throw, X}};
    exit:X -> {error, {caught, exit, X}};
    error:X -> {error, {caught, error, X}}
  end.

request(throws, {Domain, Port}, get, Uri) ->
  application:ensure_all_started(gun),
  {ok, ConnPid} = gun:open(Domain, Port),
  StreamRef = gun:get(ConnPid, Uri),
  {response, nofin, 200, _} = gun:await(ConnPid, StreamRef),
  {ok, Res} = gun:await_body(ConnPid, StreamRef),
  gun:close(ConnPid),
  jsone:decode(Res);
  
request(throws, {Domain, Port}, post, {Uri, Body, audio}) ->
  application:ensure_all_started(gun),
  {ok, ConnPid} = gun:open(Domain, Port),
  StreamRef = gun:post(ConnPid, Uri, [
    {<<"Content-Type">>, <<"application/json">>},
    {<<"accept">>, <<"audio/wav">>}
  ]),
  ok = gun:data(ConnPid, StreamRef, fin, jsone:encode(Body)),
  {response, nofin, 200, _} = gun:await(ConnPid, StreamRef),
  {ok, Res} = gun:await_body(ConnPid, StreamRef),
  gun:close(ConnPid),
  Res;

request(throws, {Domain, Port}, post, {Uri, Body}) ->
  application:ensure_all_started(gun),
  {ok, ConnPid} = gun:open(Domain, Port),
  StreamRef = gun:post(ConnPid, Uri, [
    {<<"Content-Type">>, <<"application/json">>},
    {<<"accept">>, <<"application/json">>}
  ]),
  ok = gun:data(ConnPid, StreamRef, fin, jsone:encode(Body)),
  {response, nofin, 200, _} = gun:await(ConnPid, StreamRef),
  {ok, Res} = gun:await_body(ConnPid, StreamRef),
  gun:close(ConnPid),
  jsone:decode(Res);

request(throws, Host, post, Uri) ->
  request(throws, Host, post, {Uri, <<"">>}).
